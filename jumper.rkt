#lang racket/gui
(require racket/serialize)

(define (debug text) (writeln text) (flush-output))

(define HISTORY-FILE (build-path (getenv "APPDATA") "Jumper" "history"))

(define app-frame%
  (class frame%
    (super-new
     [label "Jumper"] [min-width 500] [min-height 400])
    (define/override (on-subwindow-char receiver event)
      (when (equal? (send event get-key-code) 'escape) (exit))
      (define selection (send entries get-selection))
      (define (select-bounded selection)
        (send entries select (min (max 0 selection) (- (send entries get-number) 1))))
      (if (and selection (not (send event get-shift-down)))
          (case (send event get-key-code)
            ['down (select-bounded (+ selection 1))]
            ['up (select-bounded (- selection 1))]
            ['next (select-bounded (+ selection (send entries number-of-visible-items)))]
            ['prior (select-bounded (- selection (send entries number-of-visible-items)))]
            [else #f])
          #f))
    (define/override (on-subwindow-event receiver event)
      (send filter-text focus)
      #f)))

(define frame (new app-frame%))

(define MAX-ENTRIES 100)
(define SHOW-MORE "<show more entries>")

(define (path->entry path)
  (define path-string (path->string path))
  (substring path-string 0 (min 199 (string-length path-string))))

(define (file-matches-filter? path)
  (andmap (lambda (word)
            (string-contains? (string-downcase (path->string path)) word))
          filter-words))

(define update-thread null)
(define (update-list)
  (when (not (null? update-thread))
    (writeln (list "Killing" (current-thread)))
    (kill-thread update-thread))
  (set! update-thread (current-thread))
  (define num-entries 0)
  (define start-time (current-milliseconds))
  (define filtered-files
    (for/list ([path (reverse all-files)]
               #:when (file-matches-filter? path)
               #:break (= num-entries MAX-ENTRIES))
      (set! num-entries (add1 num-entries))
      path))
  (writeln (list "Filtered in" (- (current-milliseconds) start-time)))
  (define filtered-entries (map path->entry filtered-files))
  (when (= num-entries MAX-ENTRIES)
    (set! filtered-entries (append filtered-entries (list SHOW-MORE))))
  (send entries set filtered-entries)
  (when (> num-entries 0) (send entries select 0)))

(define (history-bump path amount)
  (define normalized-path (normalize-path path))
  (hash-set! history normalized-path (+ amount (hash-ref! history normalized-path 0)))
  (let-values ([(base name must-be-dir) (split-path path)])
    (when (and (> amount 0) base) (history-bump base (floor (/ amount 2))))))

(define (history-decay)
  (hash-for-each history (lambda (path weight)
                           (define new-weight (sub1 weight))
                           (if (<= new-weight 0)
                               (hash-remove! history path)
                               (hash-set! history path new-weight)))))

(define filter-words (list))

(define filter-text
  (new text-field%
       [label "Jump To"]
       [callback (lambda (text event)
                   (case (send event get-event-type)
                     ['text-field-enter
                      (open-selected-entry)]
                     ['text-field
                      (begin
                        (set! filter-words (string-split (string-downcase (send filter-text get-value))))
                        (thread update-list))]))]
       [parent frame]))

(define entries (new list-box%
                     [label #f]
                     [choices (list "")]  ; empty list not accepted; we clear later
                     [selection 0]
                     [parent frame]
                     [callback (lambda (box event)
                                 (when (equal? (send event get-event-type) 'list-box-dclick)
                                   (open-selected-entry)))]))
(send entries clear)

(define (open-selected-entry)
  (define selection (send entries get-string (send entries get-selection)))
  (if (equal? selection SHOW-MORE)
      (begin
        (set! MAX-ENTRIES (* 2 MAX-ENTRIES))
        (update-list))
      (begin
        (history-bump (string->path selection) 10)
        (history-decay)
        (with-handlers ([exn? (lambda (e) (display e))])
          (let-values ([(base name must-be-dir) (split-path HISTORY-FILE)])
            (unless (directory-exists? base) (make-directory base)))
          (define history-file
            (open-output-file HISTORY-FILE #:exists 'replace))
          (write (serialize history) history-file)
          (close-output-port history-file))
        (system (string-append "explorer" " \"" selection "\""))
        (exit))))

(send frame show #t)
(send filter-text focus)

(define EXCLUDED-PATHS
  (map (lambda (string)
         (normal-case-path (simplify-path (path->complete-path (string->path string)))))
       (list
        (getenv "ProgramFiles")
        (getenv "ProgramFiles(X86)")
        (getenv "ProgramData")
        (getenv "APPDATA")
        (getenv "LOCALAPPDATA")
        (string-append (getenv "APPDATA") "\\..\\" "LocalLow")
        (getenv "TEMP")
        (getenv "SystemRoot")
        "C:\\Users\\Default"
        "C:\\$RECYCLE.BIN"
        "C:\\$WinREAgent")))

(define (exclude-path? path)
  (or
   (member (normal-case-path (path->complete-path path)) EXCLUDED-PATHS)
   (let-values ([(base name must-be-dir) (split-path path)])
     (equal? (string-ref (path->string name) 0) #\.))))

(define history
  (with-handlers
      ([exn? (lambda (e) (writeln "Warning: Could not load history file!")
               (make-hash (list (cons (find-system-path 'home-dir) 1))))])
    (deserialize (read (open-input-file HISTORY-FILE)))))

(define sorted-history-paths
  (sort (hash-keys history)
        (lambda (path1 path2) (< (hash-ref history path1) (hash-ref history path2)))))

(define all-files sorted-history-paths)
(send entries set (map path->entry (reverse sorted-history-paths)))
(send entries select 0)

(define (traverse-robust proc traverse? start)
  (when (traverse? start)
    (define entries (with-handlers ([exn? (lambda (exn) '())])
                      (map (lambda (entry) (build-path start entry))
                           (directory-list start))))
    (for ([entry entries])
      (proc entry)
      (when (equal? (file-or-directory-type entry) 'directory)
        (traverse-robust proc traverse? entry)))))

(define already-traversed (make-hash))
(define (traverse-and-add-to-list path)
  (traverse-robust
   (lambda (path)
     (when (not (hash-has-key? history path))
       (set! all-files (cons path all-files))
       (when (file-matches-filter? path)
         (cond
           [(< (send entries get-number) MAX-ENTRIES)
            (send entries append (path->entry path))
            (when (= (send entries get-number) 1) (send entries select 0))]
           [(= (send entries get-number) MAX-ENTRIES)
            (send entries append SHOW-MORE)]))))
   (lambda (path) (and
                   (not (exclude-path? path))
                   (not (hash-has-key? already-traversed path))
                   (hash-set! already-traversed path #t)))
   path))

(thread (lambda ()
          (define start-time (current-seconds))
          (for-each traverse-and-add-to-list (reverse sorted-history-paths))
          (for-each traverse-and-add-to-list (filesystem-root-list))
          (writeln (list "Scanned" (length all-files) "entries in" (- (current-seconds) start-time)))))
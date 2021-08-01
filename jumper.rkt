#lang racket/gui

(define (debug text) (writeln text) (flush-output))

(define app-frame%
  (class frame%
    (super-new
     [label "Jumper"] [min-width 300] [min-height 400])
    (define/override (on-subwindow-char receiver event)
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
          #f))))

(define frame (new app-frame%))

(define all-files (list))
(define MAX-ENTRIES 100)
(define SHOW-MORE "<show more entries>")

(define (path->entry path)
  (define path-string (path->string path))
  (substring path-string 0 (min 199 (string-length path-string))))

(define (file-matches-filter? path)
  (regexp-match filter-pattern (string-downcase (path->string path))))

(define (update-list)
  (define num-entries 0)
  (define filtered-files
    (for/list ([path all-files]
               #:when (file-matches-filter? path)
               #:break (= num-entries MAX-ENTRIES))
      (set! num-entries (add1 num-entries))
      path))
  (define filtered-entries (map path->string filtered-files))
  (when (= num-entries MAX-ENTRIES)
    (set! filtered-entries (append filtered-entries (list SHOW-MORE))))
  (send entries set filtered-entries)
  (when (> num-entries 0) (send entries select 0)))

(define filter-pattern (regexp ""))

(define filter-text
  (new text-field%
       [label "Jump To"]
       [callback (lambda (text event)
                   (case (send event get-event-type)
                     ['text-field-enter
                      (begin
                        (define selection (send entries get-string (send entries get-selection)))
                        (if (equal? selection SHOW-MORE)
                            (begin
                              (set! MAX-ENTRIES (* 2 MAX-ENTRIES))
                              (update-list))
                            (begin
                              (system (string-join (list "explorer" selection)))
                              (exit))))]
                     ['text-field
                      (begin
                        (set!
                         filter-pattern
                         (regexp (string-join
                                  (map (lambda (s) (string-append "(" s ")"))
                                       (string-split (string-downcase (send filter-text get-value))))
                                  ".*")))
                        (update-list))]))]
       [parent frame]))

(define entries (new list-box%
                     [label #f]
                     [choices (list "")]  ; empty list not accepted; we clear later
                     [selection 0]
                     [parent frame]))
(send entries clear)

(send frame show #t)
(send filter-text focus)

(define EXCLUDED-PATHS
  (map normal-case-path (map string->path (list
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
                                           "C:\\$WinREAgent"))))

(define (exclude-path? path)
  (or
   (member (normal-case-path (path->complete-path path)) EXCLUDED-PATHS)
   (let-values ([(base name must-be-dir) (split-path path)])
     (equal? (string-ref (path->string name) 0) #\.))))

(define (traverse-robust proc dive? start)
  (define entries (with-handlers ([exn? (lambda (exn) '())])
                    (map (lambda (entry) (build-path start entry))
                         (directory-list start))))
  (for ([entry entries])
    (proc entry)
    (if (and
         (equal? (file-or-directory-type entry) 'directory)
         (dive? entry))
        (traverse-robust proc dive? entry)
        null)))

(thread (lambda ()
          (define start-time (current-seconds))
          (traverse-robust
           (lambda (path)
             (when (file-matches-filter? path)
                 (cond
                   [(< (send entries get-number) MAX-ENTRIES)
                    (send entries append (path->entry path))
                    (when (= (send entries get-number) 1) (send entries select 0))]
                   [(= (send entries get-number) MAX-ENTRIES)
                    (send entries append SHOW-MORE)]))
             (set! all-files (append all-files (list path))))
           (lambda (path) (not (exclude-path? path)))
           (string->path "C:\\"))
          (writeln (- (current-seconds) start-time))))

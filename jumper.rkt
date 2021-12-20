#lang racket/base
(require racket/class)
(require racket/gui/base)
(require racket/string)
(require racket/math)
(require racket/system)
(require racket/path)
(require racket/stream)
(require "scanner.rkt")
(require "recents.rkt")

(define shift-down? #f)
(define control-down? #f)

(define-values (screen-width screen-height) (get-display-size))
(define frame-width (/ screen-width 2))
(define frame-height (/ screen-height 2))
(define app-frame%
  (class frame%
    (field (status-text ""))

    (super-new
     [label "Jumper"]
     [min-width frame-width] [min-height frame-height]
     [x (/ (- screen-width frame-width) 2)] [y (/ (- screen-height frame-height) 2)])

    (define (update-modifier-state event)
        (set! shift-down? (send event get-shift-down))
        (set! control-down? (send event get-control-down)))

    (define/override (on-subwindow-char receiver event)
      (when (equal? (send event get-key-code) 'escape)
        (if (non-empty-string? (send filter-text get-value))
            (begin
              (send filter-text set-value "")
              (trigger-update-list))
            (exit)))
      (when (equal? (send event get-key-code) #\return)
        (update-modifier-state event))

      (define selection (send entries get-selection))
      (define (select-bounded selection)
        (send entries select (min (max 0 selection) (- (send entries get-number) 1))))
      (if (and (send entries is-enabled?) selection (not (send event get-shift-down)))
          (case (send event get-key-code)
            ['down (select-bounded (+ selection 1))]
            ['up (select-bounded (- selection 1))]
            ['next (select-bounded (+ selection (send entries number-of-visible-items)))]
            ['prior (select-bounded (- selection (send entries number-of-visible-items)))]
            [else #f])
          #f))

    (define/override (on-subwindow-event receiver event)
      (update-modifier-state event)
      (send filter-text focus)
      #f)

    (define/override (set-status-text text [transient #f])
      (unless transient (set! status-text text))
      (super set-status-text text))

    (define/public (reset-status-text)
      (send this set-status-text status-text))))


(define frame (new app-frame%))
(send frame show #t)
(send frame create-status-line)

(define MAX-ENTRIES 100)
(define SHOW-MORE "<show more entries>")

(define (path->entry path)
  (define path-string (path->string path))
  (define path-string-len (string-length path-string))
  (define half-len (- (exact-floor (/ MAX-ENTRY-LEN 2)) 2))
  (if (< path-string-len MAX-ENTRY-LEN)
      path-string
      (string-append
       (substring path-string 0 half-len)
       "···"
       (substring path-string (- path-string-len half-len) path-string-len))))

(define (string-asciize str)
  (list->string
   (for/list ([b (in-bytes (string->bytes/utf-8 (string-normalize-nfd str)))]
              #:when (< b 128))
     (integer->char b))))

(define (string-normalize str)
  (string-asciize (string-downcase str)))

(define (item-matches-filter? item)
  (andmap (lambda (word) (string-contains? (path-item-string item)  word)) filter-words))

(struct path-item (path string))
(define (make-path-item path) (path-item path (string-normalize (path->string path))))

(define update-thread (thread (lambda () null)))
(define (update-list)
  (kill-thread update-thread)
  (set! update-thread (current-thread))
  (thread-suspend scan-thread)
  (send entries enable #f)
  (send frame set-status-text "Filtering..." #t)
  (define num-entries 0)
  (define filtered-files
    (for/list ([path-item (reverse all-files)]
               #:when (item-matches-filter? path-item)
               #:break (= num-entries MAX-ENTRIES))
      (set! num-entries (add1 num-entries))
      (path-item-path path-item)))
  (send entries set (map path->entry filtered-files))
  (for ([(path index) (in-indexed filtered-files)])
    (send entries set-data index path))
  (when (= num-entries MAX-ENTRIES) (send entries append SHOW-MORE))
  (when (> num-entries 0) (send entries select 0))
  (send entries enable #t)
  (send frame reset-status-text)
  (thread-resume scan-thread))


(define filter-words (list))

(define (trigger-update-list)
  (set! filter-words (string-split (string-normalize (send filter-text get-value))))
  (thread update-list))

(define filter-text
  (new text-field%
       [label "Jump To"]
       [callback (lambda (text event)
                   (case (send event get-event-type)
                     ['text-field-enter (open-selected-entry)]
                     ['text-field (trigger-update-list)]))]
       [parent frame]))
(send filter-text focus)

(define entries (new list-box%
                     [label #f]
                     [choices (list "")]  ; empty list not accepted; we clear later
                     [selection 0]
                     [parent frame]
                     [callback (lambda (box event)
                                 (when (equal? (send event get-event-type) 'list-box-dclick)
                                   (open-selected-entry)))]))
(send entries clear)

(define-values (char-width _) (get-window-text-extent "X" view-control-font))
(define MAX-ENTRY-LEN (/ (send entries get-width) char-width))

(define (handler-app path)
  (define ext (if (path-get-extension path) (path-get-extension path) #""))
  (case (bytes->string/utf-8 ext)
    [(".xls" ".xlsx") "excel"]
    [(".doc" ".docx") "winword"]
    [(".ppt" ".pptx") "powerpnt"]
    [else "explorer"]))

(define (open-selected-entry)
  (define selection-string (send entries get-string (send entries get-selection)))
  (define selection-path (send entries get-data (send entries get-selection)))
  (if (equal? selection-string SHOW-MORE)
      (begin
        (set! MAX-ENTRIES (* 2 MAX-ENTRIES))
        (thread update-list))
      (begin
        (history-bump selection-path 10)
        (history-decay)
        (with-handlers ([exn? (lambda (e) (message-box
                                           "Error" (format "Could not save history: ~a" (exn-message e))
                                           #f (list 'ok 'stop)))])
          (history-save))

        (let ([command-prefix
               (if (path-is-http? selection-path)
                   (if control-down? "explorer" (string-append "start " (handler-app selection-path)))
                   (if control-down? "explorer /select," "explorer"))])
          (process (string-append command-prefix " \"" (path->string selection-path) "\"")))
        (when (not shift-down?) (exit)))))

(define all-files (list))
(define already-listed (make-hash))

(define (add-to-list path)
  (when (not (hash-has-key? already-listed path))
    (hash-set! already-listed path #t)
    (define item (make-path-item path))
    (set! all-files (cons item all-files))
    (when (item-matches-filter? item)
      (cond
        [(< (send entries get-number) MAX-ENTRIES)
         (send entries append (path->entry path) path)
         (when (= (send entries get-number) 1) (send entries select 0))]
        [(= (send entries get-number) MAX-ENTRIES)
         (send entries append SHOW-MORE)]))))

(define (traverse-and-add-to-list start)
  (traverse add-to-list start))

(define (path-is-root? path)
  (cond
    [(path-is-local? path)
     (member path (filesystem-root-list))]
    [(path-is-http? path)
     (= (length (string-split (substring (path->string path) (string-length "https://")) "/")) 2)]
    [else (equal? (path->string path) "\\\\")]))


(define (path-fix-https path)
  (if (and path (path-is-http? path))
      (string->path (string-replace (path->string path) "https:/" "https://" #:all? #f))
      path))

(define (parent-dirs-except-roots paths)
  (filter (lambda (p) (and p (not (path-is-root? p))))
          (for/list ([path paths])
            (let-values ([(base name must-be-dir) (split-path path)])
              (path-fix-https base)))))

(define scan-thread
  (thread (lambda ()
            (send frame set-status-text "Loading recents...")
            (define recents
              (with-handlers
                ([exn? (lambda (e)
                         (message-box "Error" (format "Error loading recents: ~a" (exn-message e))
                                      #f (list 'ok 'stop))
                         (exit 1))])
                (load-recents)))
            (add-to-list (stream-first recents))
            (send entries select 0)
            (for ([path (stream-rest recents)] #:when (path-maybe-exists? path))
              (add-to-list path))
            (define recents-list (map path-item-path all-files))

            (define start-time (current-seconds))
            (send frame set-status-text "Searching recents...")
            (for ([path recents-list] #:when (path-is-local-directory? path))
              (traverse-and-add-to-list path))

            (send frame set-status-text "Searching one level up...")
            (define recents-one-level-up (parent-dirs-except-roots recents-list))
            (for ([path recents-one-level-up] #:when (path-maybe-exists? path))
              (add-to-list path))
            (for ([path recents-one-level-up] #:when (path-is-local-directory? path))
              (traverse-and-add-to-list path))

            (send frame set-status-text "Searching two levels up...")
            (define recents-two-level-up (parent-dirs-except-roots recents-one-level-up))
            (for ([path recents-two-level-up] #:when (path-maybe-exists? path))
              (add-to-list path))
            (for ([path recents-two-level-up] #:when (path-is-local-directory? path))
              (traverse-and-add-to-list path))

            (for ([root (filesystem-root-list)])
              (send frame set-status-text (format "Searching drive ~a..." root))
              (traverse-and-add-to-list root))
            (send frame set-status-text
                  (format "Scanned ~a entries in ~as"
                          (length all-files)
                          (- (current-seconds) start-time))))))

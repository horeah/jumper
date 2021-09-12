#lang racket/gui
(require "scanner.rkt")
(require "history.rkt")

(define app-frame%
  (class frame%
    (super-new
     [label "Jumper"] [min-width 500] [min-height 400])
    (define/override (on-subwindow-char receiver event)
      (when (equal? (send event get-key-code) 'escape) (exit))
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
      (send filter-text focus)
      #f)))

(define frame (new app-frame%))
(send frame create-status-line)

(define MAX-ENTRIES 100)
(define SHOW-MORE "<show more entries>")

(define (path->entry path)
  (define path-string (path->string path))
  (substring path-string 0 (min 199 (string-length path-string))))

(define (file-matches-filter? path)
  (andmap (lambda (word)
            (string-contains? (string-downcase (path->string path)) word))
          filter-words))

(define update-thread (thread (lambda () null)))
(define (update-list)
  (kill-thread update-thread)
  (set! update-thread (current-thread))
  (thread-suspend scan-thread)
  (send entries enable #f)
  (send frame set-status-text "Searching...")
  (define num-entries 0)
  (define filtered-files
    (for/list ([path (reverse all-files)]
               #:when (file-matches-filter? path)
               #:break (= num-entries MAX-ENTRIES))
      (set! num-entries (add1 num-entries))
      path))
  (define filtered-entries (map path->entry filtered-files))
  (when (= num-entries MAX-ENTRIES)
    (set! filtered-entries (append filtered-entries (list SHOW-MORE))))
  (send entries set filtered-entries)
  (when (> num-entries 0) (send entries select 0))
  (send entries enable #t)
  (when (thread-dead? scan-thread) (send frame set-status-text ""))
  (thread-resume scan-thread))


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
        (with-handlers ([exn? (lambda (e) (message-box
                                           "Error" (format "Could not save history: ~a" (exn-message e))
                                           #f (list 'ok 'stop)))])
          (history-save))
        (process (string-append "explorer" " \"" selection "\""))
        (exit))))

(send frame show #t)
(send filter-text focus)

(with-handlers
  ([exn? (lambda (e)
           (message-box "Error" (format "Could not load history: ~a" (exn-message e))
                        #f (list 'ok 'stop))
           (exit 1))])
  (history-load))
(define all-files sorted-history-paths)
(send entries set (map path->entry (reverse sorted-history-paths)))
(send entries select 0)

(define (traverse-and-add-to-list start)
  (traverse
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
   start))

(send frame set-status-text "Searching...")
(define scan-thread
  (thread (lambda ()
            (define start-time (current-seconds))
            (for-each traverse-and-add-to-list (reverse sorted-history-paths))
            (for-each traverse-and-add-to-list (filesystem-root-list))
            (send frame set-status-text
                  (format "Scanned ~a entries in ~as"
                          (length all-files)
                          (- (current-seconds) start-time))))))

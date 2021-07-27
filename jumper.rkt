#lang racket/gui
(current-directory (find-system-path 'home-dir))

(define (debug text)
  (writeln text)
  (flush-output))

(define frame (new frame%
                   [label "Jumper"]
                   [min-width 300]
                   [min-height 400]))

(define all-files (list))
(define MAX-ENTRIES 100)
(define SHOW-MORE "<show more entries>")

(define (path->entry path)
  (define path-string (path->string path))
  (substring path-string 0 (min 199 (string-length path-string))))

(define (update-list)
  (define updated-entries
    (filter (lambda (filename) (string-contains? filename filter-value))
            (map path->entry all-files)))
  (send entries set (if (< (length updated-entries) MAX-ENTRIES)
                           updated-entries
                           (append (take updated-entries MAX-ENTRIES) (list SHOW-MORE))))
  (with-handlers ([exn:fail:contract? (lambda (exn) null)])
    (send entries select 0)))

(define filter-value "")

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
                        (set! filter-value (send filter-text get-value))
                        (update-list))]))]
       [parent frame]))

(define entries (new list-box%
                     [label #f]
                     [choices (list "")]  ; empty list not accepted; we clear later
                     [selection 0]
                     [parent frame]))
(send entries clear)

(send frame show #t)

(define access #t)
(thread (lambda ()
          (fold-files
           (lambda (path type result)
             (if (string-contains? (path->string path) filter-value)
                 (cond
                   [(< (send entries get-number) MAX-ENTRIES)
                    (send entries append (path->entry path))]
                   [(= (send entries get-number) MAX-ENTRIES)
                    (send entries append SHOW-MORE)])
                 null)
             (set! all-files (append all-files (list path)))
             (set! access #t)
             (if (equal? type 'dir)
                 (with-handlers ([exn? (lambda (exn) (set! access #f))])
                   (directory-list path))
                 null)
             (values null access))  ; only mark for traversal if we were able to get access
           (list))
          (debug "Done loading")))

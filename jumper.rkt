#lang racket/gui
(current-directory "C:\\Users\\Horea\\QTronic")

(define frame (new frame%
                   [label "Jumper"]
                   [min-width 300]
                   [min-height 400]))

(define all-files (list))

(define (path->entry path)
  (define path-string (path->string path))
  (substring path-string 0 (min 199 (string-length path-string))))

(define (update-list)
  (define updated-entries
    (filter (lambda (filename) (string-contains? filename filter-value))
            (map path->entry all-files)))
  (send entries set updated-entries)
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
                        (system (string-join (list "explorer"
                                                   (send entries get-string (send entries get-selection)))))
                        (exit))]
                     ['text-field
                      (begin
                        (set! filter-value (send filter-text get-value))
                        (update-list))]))]
       [parent frame]))

(define entries (new list-box%
                     [label #f]
                     [choices '("initial")]
                     [selection 0]
                     [parent frame]))

(send frame show #t)

(thread (lambda ()
          (fold-files
           (lambda (path type result)
             (if (string-contains? (path->string path) filter-value)
                 (send entries append (path->entry path))
                 null)
             (set! all-files (append all-files (list path))))
           (list))
          (writeln "Done loading")))

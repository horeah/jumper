#lang racket/gui

(define frame (new frame%
                   [label "Jumper"]
                   [min-width 300]
                   [min-height 400]))

(define (find-all-files-as-string) (map path->string (find-files (lambda (f) #t))))

(define filter-text
  (new text-field%
       [label "Jump To"]
       [callback (lambda (text event)
                   (if (equal? (send event get-event-type) 'text-field-enter)
                       (system (string-join (list "explorer"
                                                  (send entries get-string (send entries get-selection)))))
                       (begin
                         (send entries set
                               (filter (lambda (filename) (string-contains? filename (send filter-text get-value)))
                                       (find-all-files-as-string)))
                         (send entries set-selection 0))))]

       [parent frame]))

(define entries (new list-box%
                     [label #f]
                     [choices (find-all-files-as-string)]
                     [selection 0]
                     [parent frame]))

(send frame show #t)

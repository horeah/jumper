#lang racket/base
(require racket/path)
(require racket/serialize)
(provide history-load history-save history-bump history-decay history sorted-history-paths)

(define HISTORY-FILE (build-path (getenv "APPDATA") "Jumper" "history"))
(define history (make-hash))
(define sorted-history-paths (list))

(define (history-load)
  (set! history
        (with-handlers
          ([exn? (lambda (e) (writeln "Warning: Could not load history file!")
                   (make-hash (list (cons (find-system-path 'home-dir) 1))))])
          (deserialize (read (open-input-file HISTORY-FILE)))))
  (set! sorted-history-paths
        (sort (hash-keys history)
              (lambda (path1 path2) (< (hash-ref history path1) (hash-ref history path2))))))


(define (history-save)
  (with-handlers ([exn? (lambda (e) (display e))])
    (let-values ([(base name must-be-dir) (split-path HISTORY-FILE)])
      (unless (directory-exists? base) (make-directory base)))
    (define history-file
      (open-output-file HISTORY-FILE #:exists 'replace))
    (write (serialize history) history-file)
    (close-output-port history-file)))


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

#lang racket/base
(provide traverse)


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
(define (traverse proc start)
  (traverse-robust
   proc
   (lambda (path) (and
                   (not (exclude-path? path))
                   (not (hash-has-key? already-traversed path))
                   (hash-set! already-traversed path #t)))
   start))

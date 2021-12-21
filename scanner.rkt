#lang racket/base
(require racket/string)
(provide traverse)


(define EXCLUDED-PATHS
  (map (lambda (string)
         (path->directory-path (normal-case-path (simplify-path (path->complete-path
                                                                 (string->path string))))))
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
  (define normalized-path
    (path->string (path->directory-path (normal-case-path path))))
  (or
   (ormap (lambda (excluded-dir)
            (string-prefix?
             normalized-path
             (path->string excluded-dir)))
          EXCLUDED-PATHS)
   (let-values ([(base name must-be-dir) (split-path path)])
     (equal? (string-ref (path->string name) 0) #\.))))


(define (traverse-robust proc traverse? start depth)
  (when (and
         (> depth 0)
         (traverse? start))
    (define entries (with-handlers ([exn? (lambda (exn) '())])
                      (map (lambda (entry) (build-path start entry))
                           (directory-list start))))
    (for ([entry entries])
      (proc entry)
      (when (equal? (file-or-directory-type entry) 'directory)
        (traverse-robust proc traverse? entry (- depth 1))))))


(define already-traversed (make-hash))
(define (traverse proc start [depth +inf.0])
  (traverse-robust
   proc
   (lambda (path) (and
                   (not (exclude-path? path))
                   (> depth (hash-ref already-traversed path 0))
                   (hash-set! already-traversed path depth)))
   start
   depth))

#lang racket/base
(require racket/path)
(require racket/serialize)
(require racket/string)
(require racket/stream)
(require racket/file)
(require racket/list)
(require json)
(require ffi/com)
(require net/uri-codec)

(provide history-save history-bump history-decay load-recents
         path-is-http? path-is-local? path-maybe-exists? path-is-local-directory?)

(define HISTORY-FILE (build-path (getenv "APPDATA") "Jumper" "history"))
(define history (make-hash))
(define sorted-history-paths (list))

(define (history-load)
  (set! history
        (if (file-exists? HISTORY-FILE)
            (deserialize (call-with-input-file HISTORY-FILE read))
            (make-hash)))
  (set! sorted-history-paths
        (sort (hash-keys history)
              (lambda (path1 path2) (> (hash-ref history path1) (hash-ref history path2))))))


(define (history-save)
    (let-values ([(base name must-be-dir) (split-path HISTORY-FILE)])
      (unless (directory-exists? base) (make-directory base)))
    (define history-file
      (open-output-file HISTORY-FILE #:exists 'replace))
    (write (serialize history) history-file)
    (close-output-port history-file))


(define (history-bump path amount)
  (define normalized-path
    (if (path-is-http? path) path (normalize-path path)))
  (hash-set! history normalized-path (+ amount (hash-ref! history normalized-path 0))))


(define (history-decay)
  (hash-for-each history (lambda (path weight)
                           (define new-weight (sub1 weight))
                           (if (<= new-weight 0)
                               (hash-remove! history path)
                               (hash-set! history path new-weight)))))


(define (load-shortcuts-dir dir)
  (define shell (com-create-instance "WScript.Shell"))
  (define targets
    (for/stream ([path (directory-list dir #:build? #t)])
      (case (path-get-extension path)
        [(list #".lnk")
         (define file-name (path->string (path-replace-extension (file-name-from-path path) "")))
         (unless (or
                  (string-prefix? file-name "ms-gamingoverlay")
                  (string-prefix? file-name "ms-settings")
                  (string-prefix? file-name "ms-screensketchedit")
                  (string-prefix? file-name "ms-powerpoint")
                  (string-prefix? file-name "microsoft-edgehttp")
                  (string-prefix? file-name "https--")
                  (string-prefix? file-name "windowsdefender--")
                  (string-suffix? file-name "automaticDestinations-ms"))
           (define shortcut (com-invoke shell "CreateShortcut" (path->string path)))
           (com-get-property shortcut "TargetPath"))]
        [(list #".url")
         (define lines (file->lines path))
         (uri-decode (second (string-split (second lines) "=")))])))

  (for/stream ([target targets]
             #:when (non-empty-string? target))
    (string->path target)))


(define (load-mru-cache dir)
  (define result (stream))
  (for ([id (directory-list dir #:build? #t)])
    (for ([app (directory-list id #:build? #t)])
      (for ([doclist (directory-list app #:build? #t)]
            #:when (string-prefix? (path->string (file-name-from-path doclist)) "Documents_"))
        (set! result
              (stream-append result
                             (for/stream ([doc (call-with-input-file doclist read-json)])
                               (string->path (hash-ref doc 'DocumentUrl))))))))
  result)


(define (load-recents)
  (history-load)
  (stream-append
   sorted-history-paths
   (load-shortcuts-dir (build-path (getenv "APPDATA") "Microsoft\\Windows\\Recent"))
   (load-shortcuts-dir (build-path (getenv "APPDATA") "Microsoft\\Office\\Recent"))
   (load-mru-cache (build-path (getenv "LOCALAPPDATA") "Microsoft\\Office\\16.0\\MruServiceCache"))))


(define (path-is-http? path)
  (string-prefix? (path->string path) "https:"))


(define (path-is-local? path)
  (equal? (string-ref (path->string path) 1) #\:))


(define (path-is-local-directory? path)
  (and (path-is-local? path) (directory-exists? path)))


(define (path-maybe-exists? path)
  (if (path-is-local? path)
      (or (file-exists? path) (directory-exists? path))
      #t))
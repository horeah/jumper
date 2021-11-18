#lang racket/base
(require racket/path)
(require racket/serialize)
(require racket/string)
(require racket/stream)
(require racket/file)
(require racket/list)
(require json)
(require ffi/com)

(provide history-load history-save history-bump history-decay history sorted-history-paths
         path-is-online? load-recents)

(define HISTORY-FILE (build-path (getenv "APPDATA") "Jumper" "history"))
(define history (make-hash))
(define sorted-history-paths (list))

(define (history-load)
  (set! history
        (if (file-exists? HISTORY-FILE)
            (deserialize (call-with-input-file HISTORY-FILE read))
            (make-hash (list (cons (find-system-path 'home-dir) 1)))))
  (set! sorted-history-paths
        (sort (hash-keys history)
              (lambda (path1 path2) (< (hash-ref history path1) (hash-ref history path2))))))


(define (history-save)
    (let-values ([(base name must-be-dir) (split-path HISTORY-FILE)])
      (unless (directory-exists? base) (make-directory base)))
    (define history-file
      (open-output-file HISTORY-FILE #:exists 'replace))
    (write (serialize history) history-file)
    (close-output-port history-file))


(define (history-bump path amount)
  (define normalized-path
    (if (path-is-online? path) path (normalize-path path)))
  (hash-set! history normalized-path (+ amount (hash-ref! history normalized-path 0)))
  (unless (path-is-online? path)
    (let-values ([(base name must-be-dir) (split-path path)])
      (when (and (> amount 0) base) (history-bump base (floor (/ amount 2)))))))


(define (history-decay)
  (hash-for-each history (lambda (path weight)
                           (define new-weight (sub1 weight))
                           (if (<= new-weight 0)
                               (hash-remove! history path)
                               (hash-set! history path new-weight)))))

(define (load-recents)
  (stream-append 
   (load-recents-dir (build-path (getenv "APPDATA") "Microsoft\\Windows\\Recent"))
   (load-recents-dir (build-path (getenv "APPDATA") "Microsoft\\Office\\Recent"))
   (load-mru-cache (build-path (getenv "LOCALAPPDATA") "Microsoft\\Office\\16.0\\MruServiceCache"))))


(define (load-recents-dir dir)
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
         (second (string-split (second lines) "="))])))

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


(define (path-is-online? path)
  (string-prefix? (path->string path) "https:"))

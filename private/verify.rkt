#lang racket/base

(require racket/contract/base)
(provide
  (contract-out
    [verify
     (-> (and/c string? file-exists?) (listof (list/c symbol? any/c)) boolean?)]
    ;; (verify mod-path id+ctc*)
    ;; Apply Soft Contract verification to `mod-path`,
    ;;  with the goal of showing each id meets the associated spec in `id+ctc*`
))

(require
  require-typed-scv/private/log
  (only-in racket/list
    last)
  (only-in racket/system
    system*)
  (only-in racket/port
    peeking-input-port
    with-output-to-string)
  (only-in racket/string
    string-contains?
    string-trim))

;; =============================================================================

(define (verify mod-path id+ctc*)
  (call-with-tmpfile mod-path
    (λ (mod-path.bak)
      (copy/scv mod-path mod-path.bak id+ctc*)
      (log-rts-info "running SCV on '~a' with spec '~a'" mod-path id+ctc*)
      (scv-safe? (run-scv mod-path.bak)))))

(define (copy/scv src-name dst-name id+ctc)
  (call-with-input-file src-name
    (λ (src-port)
      (call-with-output-file dst-name
        (λ (dst-port)
          ;; copy lang line
          (display "#lang " dst-port)
          (displayln (read-lang src-port) dst-port)
          ;; copy body
          (copy-without-provides src-port dst-port)
          ;; print verification condition
          (newline dst-port)
          (displayln "(require require-typed-scv/private/fake-type)" dst-port)
          (displayln "(provide (contract-out" dst-port)
          (for ([x (in-list id+ctc)])
            (display "  " dst-port)
            (displayln x dst-port))
          (displayln "))" dst-port)
          (void))))))

(define (copy-without-provides src-port dst-port)
  (let loop ()
    (define v (read src-port))
    (cond
     [(eof-object? v)
      (void)]
     [(and (pair? v) (memq (car v) '(provide #%provide)))
      (loop)]
     [else
      (writeln v dst-port)
      (loop)])))

;; read-lang : input-port? -> (or/c #f string?)
(define read-lang
  (let ([read-language-fail (gensym 'read-language-fail)])
    (λ (port)
      (define port* (peeking-input-port port))
      (port-count-lines! port*)
      (and
       (with-handlers ([exn:fail:read? (λ (e) #false)])
         (not (eq? (read-language port* (λ () read-language-fail))
                   read-language-fail)))
       (let* ([end (file-position port*)]
              [str (read-string end port)]
              [hash-lang-positions (regexp-match-positions* "#lang|#!" str)]
              [start (cdr (last hash-lang-positions))])
         (string-trim (substring str start)))))))

(define (run-scv fname)
  (shell "raco" "scv" fname))

(define (scv-safe? output)
  (string=? (string-trim output) "Safe"))

;; -----------------------------------------------------------------------------

(define (call-with-tmpfile fname k)
  (define tmp (make-backup-filename fname))
  (define res (k tmp))
  (when (file-exists? tmp)
    (delete-file tmp))
  res)

(define (make-backup-filename fname)
  (define bak (path-add-extension fname ".bak" #"."))
  (if (not (file-exists? bak))
    bak
    (for/or ([i (in-naturals 1)])
      (let ([n (path-replace-extension bak (format ".bak~a" i))])
        (and (not (file-exists? n)) n)))))

(define (path-string->string x)
  (if (path? x) (path->string x) x))

(define (shell pre-exe . pre-cmd*)
  (define exe (find-exe pre-exe))
  (define success? (box #f))
  (define cmd* (map path-string->string pre-cmd*))
  (define str
    (with-output-to-string
      (λ ()
        (set-box! success? (apply system* exe cmd*)))))
  (if (unbox success?)
    (string-trim str)
    (raise-user-error 'shell "failed to apply '~a' to arguments '~a'" exe cmd*)))

;; find-exe : path-string? -> path-string?
(define (find-exe pre-exe)
  (define fep (find-executable-path pre-exe))
  (if (path? fep)
    fep
    (raise-user-error 'shell "cannot find executable '~a', please install and try again" pre-exe)))


#lang racket/base

(provide
  require/typed/scv)

(require
  racket/port
  require-typed-scv/private/parse-type
  (only-in typed/racket/base
    require/typed)
  (only-in typed/racket/unsafe
    unsafe-require/typed)
  (for-syntax
    racket/base
    require-typed-scv/private/verify
    require-typed-scv/private/log
    syntax/parse
    typed/untyped-utils))

;; =============================================================================

(define-syntax (require/typed/scv stx)
  (if (not (syntax-local-typed-context?))
    (raise-user-error 'require/typed/scv "must be called in a typed context")
    (syntax-parse stx
     [(_ mod-path:str [f:id t] ...)
      #:when (let* ([mp (syntax->string #'mod-path)]
                    [ok?
                     (verify
                       mp
                       (for/list ([f-stx (in-list (syntax-e #'(f ...)))]
                                  [t-stx (in-list (syntax-e #'(t ...)))])
                         (list (syntax->symbol f-stx) (syntax->type-contract-rep t-stx))))]
                    [_log
                     (if ok?
                       (log-rts-info "successfully verified '~a'" mp)
                       (log-rts-info "failed to verify '~a'" mp))])
               ok?)
      (syntax/loc stx
        (unsafe-require/typed mod-path [f t] ...))]
     [(_ . arg*)
      (syntax/loc stx
        (require/typed . arg*))])))

(define-for-syntax (syntax->string stx)
  (define v (syntax-e stx))
  (if (string? v)
    v
    (raise-argument-error 'syntax->string "(syntaxof string?)" stx)))

(define-for-syntax (syntax->symbol stx)
  (define v (syntax-e stx))
  (if (symbol? v)
    v
    (raise-argument-error 'syntax->symbol "(syntaxof symbol?)" stx)))


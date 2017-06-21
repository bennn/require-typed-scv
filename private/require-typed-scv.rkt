#lang racket/base

;; TODO
;; - allow "contract-out-clause*" for forall/exists variables
;;   (one TR clause corresponds to multiple contract-out)

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

(begin-for-syntax
  (define-syntax-class opt-parent
    ;; basically "identity", but rules out some things
    #:attributes (name contract-out)
    (pattern name:id
      #:attr contract-out #`#,(syntax->symbol #'name))
    (pattern (name:id parent:id)
      #:attr contract-out #`(#,(syntax->symbol #'name) #,(syntax->symbol #'parent))))

  (define-syntax-class field/type
    #:attributes (contract-out)
    (pattern (field:id (~datum :) ty:expr)
      #:attr contract-out #`(#,(syntax->symbol #'field) #,(syntax->type-rep #'ty))))

  (define-splicing-syntax-class (struct-opts struct-name)
    #:attributes (ctor-value type)
    (pattern (~seq (~optional (~seq (~and key (~or #:extra-constructor-name #:constructor-name))
                              name:id))
                   (~optional (~seq #:type-name type:id) #:defaults ([type struct-name])))
      #:attr ctor-value (if (attribute key) #'(key name) #'())))

  (define-syntax-class require/typed-clause
    #:attributes (contract-out-clause new-type)
    (pattern [nm:id ty:expr]
      #:attr new-type #'#f
      #:attr contract-out-clause
             #`(#,(syntax->symbol #'nm) #,(syntax->type-rep #'ty)))
    (pattern [(orig-nm:id new-nm:id) ty:expr]
      #:attr new-type #'#f
      #:attr contract-out-clause
             #`(rename #,(syntax->symbol #'orig-nm)
                       #,(syntax->symbol #'new-nm)
                       #,(syntax->type-rep #'ty)))
    (pattern [(~or (~datum struct) #:struct)
              (~optional (~seq (tvar ...)) #:defaults ([(tvar 1) '()]))
              struct-nm:opt-parent
              (ft*:field/type ...)
              (~var opts (struct-opts #'nm.nm))]
      #:attr new-type #'struct-nm.name
      #:attr contract-out-clause
             #`(struct struct-nm.contract-out (ft*.contract-out ...)))
  )
)

(define-syntax (require/typed/scv stx)
  (if (not (syntax-local-typed-context?))
    (raise-user-error 'require/typed/scv "must be called in a typed context")
    (syntax-parse stx
     [(_ mod-path:str c*:require/typed-clause ...)
      #:when (let* ([mp (syntax->string #'mod-path)]
                    [_ (let ([u* (get-unbound-types (filter identifier? (syntax-e #'(c*.new-type ...))))])
                         (unless (null? u*)
                           (raise-syntax-error #f "unbound types" stx (map syntax-e u*))))]
                    [ok?
                     (parameterize ([current-directory (syntax->directory stx)])
                       (verify
                         mp
                         (syntax->datum #'(c*.contract-out-clause ...))))]
                    [_log
                     (if ok?
                       (log-require-typed-scv-info "successfully verified '~a'" mp)
                       (log-require-typed-scv-info "failed to verify '~a'" mp))])
               ok?)
      (syntax/loc stx
        (unsafe-require/typed mod-path c* ...))]
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

(define-for-syntax (syntax->directory stx)
  (define src (syntax-source stx))
  (if (path-string? src)
    (let-values ([(b n d) (split-path src)])
      b)
    (current-directory)))


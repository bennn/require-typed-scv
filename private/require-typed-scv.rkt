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
    racket/match
    require-typed-scv/private/verify
    require-typed-scv/private/log
    syntax/id-table
    syntax/parse
    typed/untyped-utils))

;; =============================================================================

(begin-for-syntax
  (struct require-typed-clause [] #:transparent)
  (struct rtnormal require-typed-clause [name type] #:transparent)
  (struct rtrename require-typed-clause [original-name new-name type] #:transparent)
  (struct rtstruct require-typed-clause [name parent field*] #:transparent)
  (struct rtopaque require-typed-clause [type pred] #:transparent)

  (define-syntax-class opt-parent
    #:attributes (name parent)
    (pattern name:id
      #:attr parent #f)
    (pattern (name:id parent:id)))

  (define-syntax-class field/type
    #:attributes (field type)
    (pattern (field:id (~datum :) type:expr)))

  (define-splicing-syntax-class struct-opts
    ;; #:constructor-name
    ;; #:extra-constructor-name
    ;; #:type-name
    (pattern (~seq (~optional (~seq (~and key (~or #:extra-constructor-name #:constructor-name)) name:id))
                   (~optional (~seq #:type-name type:id) #:defaults ([type #'#f])))))

  (define-syntax-class require/typed-clause
    #:attributes (rt-clause)
    (pattern [name:id type:expr]
      #:attr rt-clause (rtnormal #'name #'type))
    (pattern [(original-name:id new-name:id) type:expr]
      #:attr rt-clause (rtrename #'original-name #'new-name #'type))
    (pattern [(~or (~datum struct) #:struct)
              (~optional (~seq (tvar ...)) #:defaults ([(tvar 1) '()]))
              struct-nm:opt-parent
              (ft*:field/type ...)
              opts:struct-opts]
      #:attr rt-clause (rtstruct (attribute struct-nm.name)
                                 (attribute struct-nm.parent)
                                 (map cons (syntax-e #'(ft*.field ...)) (syntax-e #'(ft*.type ...)))))
    (pattern [#:opaque type-name:id pred-name:id]
      #:attr rt-clause (rtopaque #'type-name #'pred-name))
  )
)

(define-syntax (require/typed/scv stx)
  (if (not (syntax-local-typed-context?))
    (raise-user-error 'require/typed/scv "must be called in a typed context")
    (syntax-parse stx
     [(_ mod-path:str c*:expr ...)
      #:when (log-require-typed-scv-debug "parsing (require/typed/scv ~a ....)" (syntax-e #'mod-path))
      #:when (let* ([mp (syntax->string #'mod-path)]
                    [cwd (syntax->directory stx)]
                    [co-clause*+extra-defs
                     (let* ([rtc*
                             (for/list ([c (in-list (syntax-e #'(c* ...)))])
                               (syntax-parse c
                                [x:require/typed-clause
                                 (attribute x.rt-clause)]
                                [_
                                 (raise-syntax-error 'require/typed/scv
                                   "expected a require/typed clause" stx c)]))]
                            [extra-type-map
                             (require-typed-clause*->free-id-table rtc*)])
                       (require-typed-clause*->contract-out-clause* rtc* extra-type-map))]
                    [ok?
                     (parameterize ([current-directory cwd])
                       (verify mp (car co-clause*+extra-defs) (cdr co-clause*+extra-defs)))]
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

(define-for-syntax (require-typed-clause*->free-id-table rtc*)
  (for/fold ([acc (make-immutable-free-id-table)])
            ([rtc (in-list rtc*)])
    (match rtc
     [(rtstruct name _ _)
      (free-id-table-set acc name (syntax->symbol name))]
     [(rtopaque type pred)
      (free-id-table-set acc type (syntax->symbol pred))]
     [_
      acc])))

(define-for-syntax (require-typed-clause*->contract-out-clause* rtc* extra-type-map)
  (define-values [co-clause* extra-defs]
    (for/fold ([co-clause* '()]
               [extra-defs '()])
              ([rtc (in-list rtc*)])
      (define-values [co extra]
        (require-typed-clause->contract-out-clause rtc extra-type-map))
      (values (cons co co-clause*) (append (reverse extra) extra-defs))))
  (cons (reverse co-clause*) (reverse extra-defs)))

(define-for-syntax (require-typed-clause->contract-out-clause rtc extra-type-map)
  (log-require-typed-scv-debug "making contract-out clause ~a" rtc)
  (match rtc
   [(rtnormal name type)
    (define-values [ctc extra] (syntax->type-rep type extra-type-map))
    (define clause `(,(syntax->symbol name) ,ctc))
    (values clause extra)]
   [(rtrename original-name new-name type)
    (define-values [ctc extra] (syntax->type-rep type extra-type-map))
    (define clause
      `(rename ,(syntax->symbol original-name)
               ,(syntax->symbol new-name)
               ,ctc))
    (values clause extra)]
   [(rtstruct name parent field*)
    (define-values [field-ctc* extra]
      (for/fold ([f* '()]
                 [extra-defs '()])
                ([ft (in-list field*)])
        (define-values [ctc extra] (syntax->type-rep (cdr ft) extra-type-map))
        (define clause `(,(syntax->symbol (car ft)) ,ctc))
        (values (cons clause f*) (append extra extra-defs))))
    (define clause
      `(struct ,(if parent
                  `(,(syntax->symbol name) ,(syntax->symbol parent))
                  (syntax->symbol name))
               ,(reverse field-ctc*)))
    (values clause extra)]
   [(rtopaque type pred)
    (values `(,(syntax->symbol pred) (-> any/c boolean?)) '())]
   [_
    (raise-argument-error 'require-typed-clause->contract-out-clause "require-typed-clause?" 0 rtc extra-type-map)]))

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


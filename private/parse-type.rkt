#lang racket/base

(provide
  (for-syntax
    syntax->type-rep))

(require
  (prefix-in tr- typed/racket/base)
  (prefix-in f- soft-contract/fake-contract)
  (only-in require-typed-scv/private/define-type
    lookup-type-alias)
  (for-syntax
    racket/base
    require-typed-scv/private/log
    syntax/id-set
    syntax/parse
    (only-in syntax/id-table
      free-id-table-ref)))

;; =============================================================================

(define-for-syntax (syntax->type-rep stx extra-type-map*)
  ;; TODO https://github.com/philnguyen/soft-contract/issues/82
  ;; syntax->datum
  (let syntax->type-rep ([stx stx])
    (syntax-parse stx
     [((~or (~literal tr-->) (~literal f-->)) . arg*) (cons '-> (map syntax->type-rep (syntax-e #'arg*)))]
     [((~or (~literal values) (~literal tr-Values)) . t*) (cons 'values (map syntax->type-rep (syntax-e #'t*)))]
     [((~literal tr-HashTable) k v) (list 'hash/c (syntax->type-rep #'k) (syntax->type-rep #'v))]
     [((~literal tr-Listof) t) (list 'listof (syntax->type-rep #'t))]
     [((~literal tr-Pairof) a b) (list 'cons/c (syntax->type-rep #'a) (syntax->type-rep #'b))]
     [((~literal tr-U) . t*) (cons 'or/c (map syntax->type-rep (syntax-e #'t*)))]
     [((~literal tr-Vector) . t*) (cons 'vector/c (map syntax->type-rep (syntax-e #'t*)))]
     [((~literal tr-Vectorof) t) (list 'vectorof (syntax->type-rep #'t))]
     [(~literal tr-Any) 'any/c]
     [(~literal tr-Boolean) 'boolean?]
     [(~literal tr-False) '#f]
     [(~literal tr-Float) 'flonum?]
     [(~literal tr-Integer) 'integer?]
     [(~literal tr-Natural) 'exact-nonnegative-integer?]
     [(~literal tr-Path) 'path?]
     [(~literal tr-Path-String) 'path-string?]
     [(~literal tr-Real) 'real?]
     [(~literal tr-String) 'string?]
     [(~literal tr-Symbol) 'symbol?]
     [(~literal tr-True) '#t]
     [(~literal tr-Void) 'void]
     [(~datum #f) '#f]
     [x:id
      #:when (lookup-type-alias #'x)
      (syntax->type-rep (lookup-type-alias #'x))]
     [x:id
      (free-id-table-ref extra-type-map* #'x (lambda () (raise-user-error "unbound type" (syntax-e #'x))))]
     [_
      (raise-user-error 'syntax->type-rep "cannot parse type ~a" (syntax->datum stx))])))

(define-for-syntax (first-char str)
  (if (zero? (string-length str))
    #\A
    (string-ref str 0)))

(define-for-syntax (char-downcase? c)
  (char<=? #\a c #\z))

;; =============================================================================
;;; too hard
;(module* test typed/racket/base
;  (require typed/rackunit)
;  (require/typed (for-template (submod ".."))
;    (syntax->type-rep (-> Syntax 
;
;  (check-equal?
;    (syntax->type-rep #'Integer)
;    'integer?)
;)

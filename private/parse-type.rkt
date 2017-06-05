#lang racket/base

(provide
  (for-syntax
    syntax->type-rep))

(require
  (prefix-in tr- typed/racket/base)
  (prefix-in f- soft-contract/fake-contract)
  (for-syntax
    racket/base
    require-typed-scv/private/log
    syntax/parse))

;; =============================================================================

(define-for-syntax (syntax->type-rep stx)
  ;; TODO https://github.com/philnguyen/soft-contract/issues/82
  ;; syntax->datum
  (syntax-parse stx
   [((~or (~literal tr-->) (~literal f-->)) . arg*)
    (cons '-> (map syntax->type-rep (syntax-e #'arg*)))]
   [((~literal tr-U) . t*)
    (cons 'or/c (map syntax->type-rep (syntax-e #'t*)))]
   [((~literal tr-Listof) t)
    (list 'listof (syntax->type-rep #'t))]
   [((~literal tr-Vectorof) t)
    (list 'vectorof (syntax->type-rep #'t))]
   [((~literal tr-Vector) . t*)
    (cons 'vector/c (map syntax->type-rep (syntax-e #'t*)))]
   [((~literal tr-HashTable) k v)
    (list 'hash/c (syntax->type-rep #'k) (syntax->type-rep #'v))]
   [((~literal tr-Pairof) a b)
    (list 'cons/c (syntax->type-rep #'a) (syntax->type-rep #'b))]
   [(~literal tr-Natural)
    'exact-nonnegative-integer?]
   [(~literal tr-Real)
    'real?]
   [(~literal tr-Integer)
    'integer?]
   [(~literal tr-String)
    'string?]
   [(~literal tr-Boolean)
    'boolean?]
   [(~literal tr-True)
    '#t]
   [(~literal tr-False)
    '#f]
   [(~literal tr-Any)
    'any/c]
   [((~or (~literal values) (~literal tr-Values)) . t*)
    (cons 'values (map syntax->type-rep (syntax-e #'t*)))]
   [x:id
    #:when (char-downcase? (first-char (symbol->string (syntax-e #'x))))
    (log-require-typed-scv-error "assuming '~a' is valid racket/contract" (syntax-e #'x))
    (syntax-e #'x)]
   [_
    (raise-user-error 'syntax->type-rep "cannot parse type ~a" (syntax->datum stx))]))

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

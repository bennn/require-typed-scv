#lang racket/base

(provide
  (for-syntax
    syntax->type-rep))

(require
  (prefix-in tr- typed/racket/base)
  (for-syntax
    racket/base
    syntax/parse))

;; =============================================================================

(define-for-syntax (syntax->type-rep stx)
  ;; TODO https://github.com/philnguyen/soft-contract/issues/82
  ;; syntax->datum
  (syntax-parse stx
   #:literals (tr--> tr-Any tr-True tr-False tr-Real tr-Natural tr-Integer tr-String tr-Pairof tr-Listof tr-Vectorof tr-Boolean tr-HashTable)
   [(tr--> . arg*)
    (cons '-> (map syntax->type-rep (syntax-e #'arg*)))]
   [(tr-U . t*)
    (cons 'or/c (map syntax->type-rep (syntax-e #'t*)))]
   [(tr-Listof t)
    (list 'listof (syntax->type-rep #'t))]
   [(tr-Vectorof t)
    (list 'vectorof (syntax->type-rep #'t))]
   [(tr-HashTable k v)
    (list 'hash/c (syntax->type-rep #'k) (syntax->type-rep #'v))]
   [(tr-Pairof a b)
    (list 'cons/c (syntax->type-rep #'a) (syntax->type-rep #'b))]
   [tr-Natural
    'exact-nonnegative-integer?]
   [tr-Real
    'real?]
   [tr-Integer
    'integer?]
   [tr-String
    'string?]
   [tr-Boolean
    'boolean?]
   [tr-True
    '#t]
   [tr-False
    '#f]
   [tr-Any
    'any/c]
   [x:id
    (string->symbol (string-downcase (format "~a?" (syntax-e #'x))))]
   [_
    (raise-user-error 'syntax->type-rep "cannot parse type ~a" (syntax->datum stx))]))

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

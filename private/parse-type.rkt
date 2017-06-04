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
   #:literals (tr--> tr-Any tr-Natural tr-Integer tr-String tr-Listof tr-Vectorof tr-Boolean tr-HashTable)
   [(tr--> . arg*)
    (cons '-> (map syntax->type-rep (syntax-e #'arg*)))]
   [(tr-Listof t)
    (list 'listof (syntax->type-rep #'t))]
   [(tr-Vectorof t)
    (list 'vectorof (syntax->type-rep #'t))]
   [(tr-HashTable k v)
    (list 'hash/c (syntax->type-rep #'k) (syntax->type-rep #'v))]
   [tr-Natural
    'exact-nonnegative-integer?]
   [tr-Integer
    'integer?]
   [tr-String
    'string?]
   [tr-Boolean
    'boolean?]
   [tr-Any
    'any/c]
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

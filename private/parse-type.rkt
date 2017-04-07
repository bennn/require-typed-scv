#lang racket/base

(provide
  (for-syntax
    syntax->type-contract-rep))

(require
  (prefix-in tr- typed/racket/base)
  (for-syntax
    racket/base
    syntax/parse))
;    ;; requires for typed racket
;   typed-racket/base-env/base-structs
;   typed-racket/env/mvar-env
;   typed-racket/env/tvar-env
;   typed-racket/env/type-alias-env
;   typed-racket/private/parse-type
;   typed-racket/rep/type-rep
;   typed-racket/typecheck/internal-forms
;   typed-racket/types/numeric-tower
;   typed-racket/types/resolve
;   typed-racket/types/subtype
;   typed-racket/types/union
;   typed-racket/utils/tc-utils
;   (only-in typed-racket/base-env/base-types-extra ->)
;    (only-in (submod typed-racket/private/type-contract test-exports)
;      type->contract)
;    (only-in typed-racket/private/parse-type
;      parse-type)))

;; =============================================================================

;; very simple!
(define-for-syntax (syntax->type-contract-rep stx)
  (syntax-parse stx
   #:literals (tr--> tr-Integer tr-String)
   [(tr--> . arg)
    (cons '-> (map syntax->type-contract-rep (syntax-e #'arg)))]
   [tr-Integer
    'integer?]
   [tr-String
    'string?]))

;; =============================================================================
;;; too hard
;(module* test typed/racket/base
;  (require typed/rackunit)
;  (require/typed (for-template (submod ".."))
;    (syntax->type-contract-rep (-> Syntax 
;
;  (check-equal?
;    (syntax->type-contract-rep #'Integer)
;    'integer?)
;)

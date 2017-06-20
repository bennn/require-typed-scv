#lang racket/base

(provide
  (rename-out [-define-type define-type])
  (for-syntax
    lookup-type-alias))

(require
  (prefix-in tr: (only-in typed/racket/base define-type))
  (for-syntax racket/syntax racket/base syntax/parse syntax/id-table))

;; =============================================================================

(define-for-syntax ALIAS-TABLE
  (make-free-id-table))

(define-syntax (-define-type stx)
  (syntax-parse stx
   [(ctx id ty)
    (free-id-table-set! ALIAS-TABLE #'id #'ty)
    (syntax/loc stx
      (tr:define-type id ty))]
   [(ctx . x)
    #:with orig-define-type (format-id #'ctx "define-type")
    (syntax/loc stx
      (tr:define-type . x))]
   [ctx:id
    (syntax/loc stx
      tr:define-type)]))

(define-for-syntax (lookup-type-alias id)
  (free-id-table-ref ALIAS-TABLE id #f))


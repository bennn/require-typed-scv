#lang typed/racket

(require require-typed-scv)
(define-type Stack (Listof Integer))

(require/typed/scv "08-provide.rkt"
  (init (-> Stack))
  (hd (-> Stack Integer))
  (tl (-> Stack Stack))
  (push (-> Stack Integer Stack)))

(module+ test
  (require typed/rackunit)
  (require/typed racket/contract/base
    (has-contract? (-> Any Any)))

  (check-false (has-contract? hd))
  (check-false (has-contract? tl))
  (check-false (has-contract? push))
  (check-false (has-contract? init))
  (check-equal? (hd (tl (push (push (init) 4) 5))) 4))

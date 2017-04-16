#lang typed/racket/base
(require require-typed-scv)
(require/typed/scv "02-provide.rkt"
  (id (-> Integer Integer))
  (plus2 (-> Integer Integer)))

(module+ test
  (require typed/rackunit)
  (require/typed racket/contract/base
    (has-contract? (-> Any Any)))

  (check-false (has-contract? id))
  (check-equal? (id 1) 1)
  (check-equal? (id 2) 2)
  (check-false (has-contract? plus2))
  (check-equal? (plus2 1) 3)
  (check-equal? (plus2 -4) -2))

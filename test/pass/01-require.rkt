#lang typed/racket/base
(require require-typed-scv)
(require/typed/scv "01-provide.rkt"
  (id (-> Integer Integer)))

(module+ test
  (require typed/rackunit)
  (require/typed racket/contract/base
    (has-contract? (-> Any Any)))

  (check-false (has-contract? id))
  (check-equal? (id 1) 1)
  (check-equal? (id 2) 2))

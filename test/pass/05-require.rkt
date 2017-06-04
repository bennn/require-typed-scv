#lang typed/racket/base
(require require-typed-scv)
(require/typed/scv "05-provide.rkt"
  (rev (-> (Vectorof Integer) (Vectorof Integer))))

(module+ test
  (require typed/rackunit)
  (require/typed racket/contract/base
    (has-contract? (-> Any Any)))

  (check-false (has-contract? rev))
  (check-equal? (rev '#(1 2 3)) '#(3 2 1)))

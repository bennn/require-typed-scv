#lang typed/racket/base
(require require-typed-scv)
(require/typed/scv "03-provide.rkt"
  (rev (-> (Listof Integer) (Listof Integer))))

(module+ test
  (require typed/rackunit)
  (require/typed racket/contract/base
    (has-contract? (-> Any Any)))

  (check-false (has-contract? rev))
  (check-equal? (rev '(1 2 3)) '(3 2 1)))

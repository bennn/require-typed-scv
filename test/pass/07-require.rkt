#lang typed/racket

(require require-typed-scv)
(require/typed/scv "07-provide.rkt"
  (append (-> (Listof Integer) (Listof Integer) (Listof Integer))))

(module+ test
  (require typed/rackunit)
  (check-equal? (append '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6)))

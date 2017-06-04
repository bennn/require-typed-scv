#lang typed/racket
;(require require-typed-scv)
;(require/typed/scv "04-provide.rkt"
;  (symbolize-keys (-> (HashTable String Any) (HashTable Symbol Any))))
;
;(module+ test
;  (require typed/rackunit)
;  (require/typed racket/contract
;    (has-contract? (-> Any Boolean)))
;
;  (check-false (has-contract? symbolize-keys))
;  (check-equal? (symbolize-keys (ann (make-immutable-hash '(("A" . 1) ("B" . 2))) (HashTable String Any)))))

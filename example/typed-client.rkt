#lang typed/racket

(require require-typed-scv)

(require/typed/scv "untyped-library.rkt"
  ;; import `add2` at a compatible type
  (add2 (-> Integer Integer)))

(require/typed/scv "untyped-library.rkt"
  ;; import `add3` at an incompatible type
  (add3 (-> Integer String)))

;; Verification should:
;; - succeed for `add2`
;; - fail for `add3`
;;
;; Therefore:
;; - `add2` should not get wrapped in a contract
;; - `add3` SHOULD get a contract
;;
;; Furthermore:
;; - calling `add3` should raise a dynamic type error

(module+ test
  (require typed/rackunit)

  (check-equal? (add2 2) 4)

  (check-exn exn:fail:contract?
    (λ () (add3 2))))

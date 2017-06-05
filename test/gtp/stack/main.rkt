#lang typed/racket/base

;; TODO use stack type
;(define-type Stack (Listof Integer))

(require require-typed-scv)
(require/typed/scv "stack.rkt"
  [init (-> (Listof Integer))]
  [push ((Listof Integer) Integer . -> . (Listof Integer))])

(: main (Natural . -> . Void))
(define (main N)
  (for/fold ([st (init)])
            ([i (in-range N)])
    (push st i))
  (void))

(time (main 60000))

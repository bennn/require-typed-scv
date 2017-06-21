#lang racket/base
(module+ test
  (require rackunit syntax/macro-testing)

  (check-exn #rx"unbound types"
    (λ ()
      (convert-compile-time-error
        (expand #'(module test typed/racket
                    (require require-typed-scv)
                    (require/typed/scv "dont-care.rkt"
                      (add1 (-> Int Int)))
                    (add1 1)))))))

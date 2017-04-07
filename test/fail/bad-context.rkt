#lang racket/base
(module+ test
  (require rackunit syntax/macro-testing)

  (check-exn #rx"require/typed/scv"
    (λ ()
      (convert-compile-time-error
        (expand #'(module test racket/base
                    ;; Fail because wrong #lang
                    (require require-typed-scv)
                    (require/typed/scv "dont-care.rkt"
                      (f (-> Integer Integer)))))))))

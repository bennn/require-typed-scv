#lang typed/racket/base
(require require-typed-scv)
(require/typed/scv "11-provide.rkt"
  [#:opaque Foo foo?]
  [foo0 Foo]
  [foo++ (-> Foo Foo)])

(module+ test
  (require typed/rackunit)
  (check-true (and (foo++ foo0) #true)))

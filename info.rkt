#lang info
(define collection "require-typed-scv")
(define deps '("base" "soft-contract" "typed-racket-lib"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib" "typed-racket-doc"))
(define pkg-desc "require/typed, powered by Soft Contracts")
(define version "0.0")
(define pkga-authors '(ben))
(define scribblings '(("docs/require-typed-scv.scrbl" ())))
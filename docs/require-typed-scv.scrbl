#lang scribble/manual
@require[(for-label racket/base (only-in typed/racket Integer String -> require/typed) require-typed-scv)]

@title{Require Typed, with Soft Contract Verification}
@author{@hyperlink["https://github.com/bennn"]{Ben Greenman}}

@defmodule[require-typed-scv]{
  Provides a macro like Typed Racket's @racket[require/typed], except that it
  applies the Soft Contracts solver to remove some contracts.
}

@defform[#:id require/typed/scv (require/typed/scv m [id type] ...)]{
  Like @racket[require/typed], but uses the Soft Contract verifier to check if
   every @racket[id] meets its specification @racket[type].
  If so, applies no contracts to @racket[m].

  For verification to succeed:
  @itemlist[
  @item{
    The module path @racket[m] must be a string.
  }
  @item{
    Keywords like @racket[#:opaque] or @racket[#:struct] must not appear in the
     spec.
  }
  @item{
    @emph{All} the given @racket[id] must meet their type.
    @margin-note{To remove contracts from @emph{some} of the identifiers,
     use a separate call to @racket[require/typed/scv].}
  }
  ]

  If verification fails, @racket[require/typed/scv] defaults to the behavior
   of @racket[require/typed].
}

Example: (copied from the @filepath{require-typed-scv/example} folder)

@filepath{untyped-library.rkt}
@codeblock|{
#lang racket

(provide add2 add3)

(define (add2 n)
  (+ n 2))

(define (add3 n)
  (+ n 3))
}|

@filepath{typed-client.rkt}
@codeblock|{
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
}|

To test, run

@exec{raco test typed-client.rkt}

To test with logging, run

@exec|{PLTSTDERR="error info@require-typed-scv" raco test typed-client.rkt}|

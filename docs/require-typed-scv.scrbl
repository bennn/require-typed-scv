#lang scribble/manual
@require[(for-label (only-in typed/racket require/typed) require-typed-scv)]

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


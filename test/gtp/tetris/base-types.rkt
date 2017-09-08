#lang typed/racket

(define-type Color Symbol)
(require require-typed-scv)
(require/typed/scv "data.rkt"
  [#:struct posn ([x : Real]
                  [y : Real])]
  [#:struct block ([x : Real]
                   [y : Real]
                   [color : Symbol])]
  [#:struct tetra ([center : posn]
                   [blocks : (Listof block)])]
  [#:struct world ([tetra : tetra]
                   [blocks : (Listof block)])])

(define-type Posn posn)
(define-type Block block)
(define-type Tetra tetra)
(define-type World world)
(define-type BSet  (Listof Block))

(provide
 (struct-out posn)
 (struct-out block)
 (struct-out tetra)
 (struct-out world)
 Posn
 Block
 Tetra
 World
 Color
 BSet
 Color
 BSet)

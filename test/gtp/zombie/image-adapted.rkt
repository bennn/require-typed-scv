#lang typed/racket/base

(require require-typed-scv)

(require/typed/scv "image.rkt"
  (#:struct image ((impl : Any)))
  (empty-scene (-> Real Real image))
  (place-image (-> image Real Real image image))
  (circle (-> Real String String image))
)
(define-type Image image)

(provide
  Image
  empty-scene
  place-image
  circle
)

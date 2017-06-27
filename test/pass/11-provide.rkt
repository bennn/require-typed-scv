#lang racket/base

(struct foo [a])

(define (foo++ f)
  (foo (+ (foo-a f) 1)))

(define foo0 (foo 0))

(provide foo? foo0 foo++)

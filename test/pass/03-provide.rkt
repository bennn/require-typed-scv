#lang racket/base

(define (rev xs)
  (let loop ([xs xs] [ys '()])
    (if (null? xs)
      ys
      (loop (cdr xs) (cons (car xs) ys)))))

(provide rev)

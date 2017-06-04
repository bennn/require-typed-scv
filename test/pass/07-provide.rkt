#lang racket

(define (append xs ys)
  (if (null? xs)
    ys
    (cons (car xs) (append (cdr xs) ys))))

(provide append)

#lang racket

(provide init hd tl push)

(define (init) '())
(define (hd x) (if (null? x) (error 'death) (car x)))
(define (tl x) (if (null? x) (error 'death) (cdr x)))
(define (push x y) (cons y x))

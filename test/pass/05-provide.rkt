#lang racket/base

(define (rev xs)
  (define L (vector-length xs))
  (define ys (make-vector L))
  (let loop ([i 0])
    (if (= i L)
      ys
      (begin
        (vector-set! ys (- L i 1) (vector-ref xs i))
        (loop (+ i 1))))))

(provide rev)

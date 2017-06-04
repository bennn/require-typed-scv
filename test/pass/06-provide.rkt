#lang racket/base

(struct myCons [hd tl])
(struct myNil [])

(define mycar myCons-hd)
(define mycdr myCons-tl)
(define mynull? myNil?)

(define (rev xs)
  (let loop ([xs xs] [ys (myNil)])
    (if (mynull? xs)
      ys
      (loop (mycdr xs) (myCons (mycar xs) ys)))))

(provide rev (struct-out myCons) (struct-out myNil))

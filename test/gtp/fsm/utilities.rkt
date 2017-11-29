#lang racket

;; Utility Functions


(provide
 ;sum
 ;relative-average
 ;choose-randomly
 (contract-out ;;bg: SCV needs contract-out
   (relative-average
    (-> (listof real?) real? real?))
   (choose-randomly
    (-> [listof (>=/c 0)] exact-nonnegative-integer? (or/c boolean? real?)
        [listof (>=/c 0)]))))

;; =============================================================================

(define (sum l)
  (apply + l))


(define (relative-average l w)
  (define z (length l))
  (define s (sum l))
  (exact->inexact
   (/ s
      (if (zero? w) (error 'BG) w)
      (if (zero? z) (error 'BG) z))))

;; -----------------------------------------------------------------------------

(define (choose-randomly probabilities speed q)
  (define %s (accumulated-%s probabilities))
  (for/list ([n (in-range speed)])
    [define r (if (boolean? q) (random) q)]
    ;; population is non-empty so there will be some i such that ...
    (let loop  ([%s  %s])
      (cond
        [(empty? %s) (error 'BG)]
        [(< r (first %s)) 0]
        [else (add1 (loop (rest %s)))]))
    #;
    (for/last ([p (in-naturals)] [% (in-list %s)] #:final (< r %)) p)))

;; [Listof Probability] -> [Listof Probability]
;; calculate the accumulated probabilities 

(define (accumulated-%s probabilities)
  (define total (sum probabilities))
  (let relative->absolute
    ([payoffs  probabilities][so-far  #i0.0])
    (cond
      [(empty? payoffs) '()]
      [else (define nxt (+ so-far (first payoffs)))
            (cons
             (safe-quotient nxt total) (relative->absolute (rest payoffs) nxt))])))

;;bg added
(define (safe-quotient a b)
  (if (zero? b)
    (error 'BG)
    (/ a b)))


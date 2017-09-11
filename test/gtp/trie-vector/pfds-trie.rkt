#lang racket

(provide lookup bind trie insert tries)

(require racket/match)

;(define-struct Mt ())
;(define-struct Some (elem))

(define (make-Mt) #f)
(define (Mt? x) (not x))
(define (Some-elem x) x)
(define (make-Some x) x)

(define (make-Trie opt map)
  (vector opt map))
(define (Trie-opt t)
  (vector-ref t 0))
(define (Trie-map t)
  (vector-ref t 1))
;; --- end edit

(define (empty) 
  (make-Trie (make-Mt) 
             (make-immutable-hash null)))

(define (lookup keys map)
  (if (null? keys)
      (let ([opt (Trie-opt map)])
        (if (Mt? opt)
            (error 'lookup "given key not found in the trie")
            (Some-elem opt)))
      (let* ([fst (car keys)]
             [hash (Trie-map map)]
             [val (hash-ref hash fst)]) ;;bg might error
          (lookup (cdr keys) val))))

(define (bind lok v map)
  (define-values [fst rst]
    (if (null? lok)
      (error 'bg:empty-list)
      (values (car lok) (cdr lok))))
  (let* ([hash (Trie-map map)]
         [opt (Trie-opt map)]
         [tree (let ([prev (hash-ref hash fst (lambda () #f))])
                 (if (and prev (not (null? rst)))
                   (bind rst v prev)
                   (build v rst)))])
    (make-Trie opt (hash-set hash fst tree))))

(define (build val lstk)
  (if (null? lstk)
      (make-Trie (make-Some val) 
                 (make-immutable-hash null))
      (make-Trie (make-Mt) 
                 (make-immutable-hash 
                  (list (cons (car lstk) (build val (cdr lstk))))))))

(define (trie lst)
  (insert (get-vals lst) lst (empty)))

(define (get-vals lst)
  (define (local ctr lstk)
    (if (null? lstk)
      (error 'bg:empty-list)
      (if (null? (cdr lstk))
          (cons ctr null)
          (cons ctr (local (add1 ctr) (cdr lstk))))))
  (local 1 lst))

;; While creating the tree, 
;; if   (hash-ref hash k) throws an error, 
;; then it means that that there is no entry for k. So build a new
;;      Trie for rest of the key and create an entry for k. 
;; else go deeper into the insert searching for the rest of the key.

(define (insert lstv lstk tri)
  (match (list lstv lstk)
    [(list null null) tri]
    [(list (cons v vs) (cons (cons k ks) rstk))
     (let* ([hash (Trie-map tri)]
            [tree (let ([prev (hash-ref hash k (lambda () #f))])
                    (if prev
                      (go-deep prev ks v)
                      (build v ks)))])
       (insert vs rstk
               (make-Trie (Trie-opt tri) (hash-set hash k tree))))]
    [_ (error 'bg:bad-match)]))

(define (tries lstv lstk)
  (insert lstv lstk (empty)))

;; Uses the same trick as previous one does
(define (go-deep tri lstk val)
  (if (null? lstk)
      (make-Trie (make-Some val) (Trie-map tri))
      (let* ([hash (Trie-map tri)]
             [k (car lstk)]
             [ks (cdr lstk)]
             [v (hash-ref hash k)]
             [insert (go-deep v ks val)])
        (make-Trie (Trie-opt tri) (hash-set hash k insert)))))

#lang racket

(provide lookup bind trie insert tries)

(require racket/match)

(define-struct Mt ())
(define-struct Some (elem))

(define (make-Trie opt map)
  (vector opt map))
(define (Trie-opt t)
  (vector-ref t 0))
(define (Trie-map t)
  (vector-ref t 1))
;; --- end edit

(define (empty) 
  (make-Trie (make-Mt)
             (hash) #;(make-immutable-hash null)))

(define (lookup keys map)
  (if (null? keys)
      (let ([opt (Trie-opt map)])
        (if (Mt? opt)
            (error 'lookup "given key not found in the trie")
            (Some-elem opt)))
      (let ([fst (car keys)]
            [hash (Trie-map map)])
        ;with-handlers
        ;    ([exn:fail? (lambda (error?) 
        ;                  (error 'lookup "given key not found in the trie"))])
          (lookup (cdr keys) (hash-ref hash fst)))))

(define (bind lok v map)
  (let ([hash (Trie-map map)]
        [fst (car lok)]
        [rst (cdr lok)]
        [opt (Trie-opt map)])
    (make-Trie opt hash #;(hash-set hash fst 
                             (with-handlers 
                                      ([exn:fail? 
                                        (lambda (error?) (build v rst))])
                                    (bind rst v (hash-ref hash fst)))))))

(define (build val lstk)
  (if (null? lstk)
      (make-Trie (make-Some val) 
                 (hash) #;(make-immutable-hash null) )
      (make-Trie (make-Mt) 
                 (hash (car lstk) (build val (cdr lstk)))
                 #;(make-immutable-hash 
                  (list (cons (car lstk) (build val (cdr lstk))))))))

(define (trie lst)
  (insert (get-vals lst) lst (empty)))

(define (get-vals lst)
  (define (local ctr lstk)
    (if (null? (cdr lstk))
        (cons ctr null)
        (cons ctr (local (add1 ctr) (cdr lstk)))))
  (local 1 lst))

;; While creating the tree, 
;; if   (hash-ref hash k) throws an error, 
;; then it means that that there is no entry for k. So build a new
;;      Trie for rest of the key and create an entry for k. 
;; else go deeper into the insert searching for the rest of the key.

(define (insert lstv lstk tri)
  (match (list lstv lstk)
    [(list '() '()) tri]
    [(list (cons v vs) (cons (cons k ks) rstk))
     (let* ([hash (Trie-map tri)]
            [tree ;with-handlers ([exn:fail? (lambda (error?) 
                  ;                                  (build v ks))])
                         (go-deep (hash-ref hash k) ks v) ])
       (insert vs rstk
               (make-Trie (Trie-opt tri) hash #;(hash-set hash k tree))))]))

(define (tries lstv lstk)
  (insert lstv lstk (empty)))

;; Uses the same trick as previous one does
(define (go-deep tri lstk val)
  (if (null? lstk)
      (make-Trie (make-Some val) (Trie-map tri))
      (let* ([hash (Trie-map tri)]
             [k (car lstk)]
             [ks (cdr lstk)]
             [insert ;with-handlers
                     ;         ([exn:fail? (lambda (error?) (build val ks))])
                            (go-deep (hash-ref hash k) ks val)])
        (make-Trie (Trie-opt tri) hash #;(hash-set hash k insert)))))

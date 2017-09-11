#lang typed/racket
(require require-typed-scv)

(define-type Trie (Vector (U #f Natural) (HashTable Natural (Vector (U #f Natural) HashTableTop))))

(require/typed/scv "pfds-trie.rkt"
  (trie (-> (Listof (Listof Natural))
            (Vector (U #f Natural)
                    (HashTable Natural (Vector (U #f Natural) HashTableTop)))))
  (bind (-> (Listof Natural)
            Natural
            (Vector (U #f Natural)
                    (HashTable Natural (Vector (U #f Natural) HashTableTop)))
            (Vector (U #f Natural)
                    (HashTable Natural (Vector (U #f Natural) HashTableTop))))))

(define ITERS 100)

(define (main)
  (for/fold : Trie
            ([t : Trie (trie '((0)))])
            ([i : Natural (in-range ITERS)])
    (bind (list i) i t))
  (void))

(time (main))

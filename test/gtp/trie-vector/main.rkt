#lang typed/racket
(require require-typed-scv)
;(define-type (Trie K V) (Vector (Option V) (HashTable K (Trie K V))))
;(define-type MyTrie (Trie (Listof Natural) Natural))
(require/typed/scv "pfds-trie.rkt"
  (trie (-> (Listof (Listof Natural)) (Vector Any hash?)))
  (bind (-> (Listof Natural) Natural (Vector Any hash?) (Vector Any hash?))))

(define ITERS 100)

(define (main)
  (for/fold : (Vector Any (HashTable (Listof Natural) Any))
            ([t : (Vector Any (HashTable (Listof Natural) Any)) (trie '((0)))])
            ([i : Natural (in-range ITERS)])
    (bind (list i) i t))
  (void))

(time (main))

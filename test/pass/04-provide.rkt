#lang racket

(define (symbolize-keys h)
  ;bg: parser error
  ;(for/hash ([(k v) (in-hash h)])
  ;  (values (string->symbol k) v))
  (let loop ([acc (hash)]
             [kv* (hash->list h)])
    (if (null? kv*)
      acc
      (loop (hash-set acc (string->symbol (caar kv*) (cadr kv*))) (cdr kv*)))))

(provide symbolize-keys)

;; σ@: no address #(struct:-α.wrp #(struct:-𝒾 hash/c9 /Users/ben/code/racket/fork/racket/collects/racket/contract/private/hash.rkt))
;; context...:
;; /Users/ben/code/racket/fork/extra-pkgs/scv/soft-contract/soft-contract/reduction/compile.rkt:129:8
;; /Users/ben/code/racket/fork/extra-pkgs/scv/soft-contract/soft-contract/reduction/memoize.rkt:23:8: ⟦k⟧*
;; /Users/ben/code/racket/fork/extra-pkgs/scv/soft-contract/soft-contract/reduction/main.rkt:145:4: for-loop
;; /Users/ben/code/racket/fork/extra-pkgs/scv/soft-contract/soft-contract/reduction/main.rkt:45:4: loop!
;; /Users/ben/code/racket/fork/extra-pkgs/scv/soft-contract/soft-contract/reduction/main.rkt:32:2: run
;; .../more-scheme.rkt:261:28
;; /Users/ben/code/racket/fork/extra-pkgs/scv/soft-contract/soft-contract/cmdline.rkt: [running body]
;; /Users/ben/code/racket/fork/racket/collects/raco/raco.rkt: [running body]
;; /Users/ben/code/racket/fork/racket/collects/raco/main.rkt: [running body]
;; shell: failed to apply '/usr/local/bin/raco' to arguments '(scv 04-provide.rkt.bak4)'

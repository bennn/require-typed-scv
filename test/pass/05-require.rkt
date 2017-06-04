#lang typed/racket/base
;(require require-typed-scv)
;(require/typed/scv "05-provide.rkt"
;  (rev (-> (Vectorof Integer) (Vectorof Integer))))
;
;(module+ test
;  (require typed/rackunit)
;  (require/typed racket/contract/base
;    (has-contract? (-> Any Any)))
;
;  (check-false (has-contract? rev))
;  (check-equal? (rev '#(1 2 3)) '#(3 2 1)))

;;σ@: no address #(struct:-α.wrp #(struct:-𝒾 vectorof/proc /Users/ben/code/racket/fork/racket/collects/racket/contract/private/vector.rkt))
;;  context...:
;;   /Users/ben/code/racket/fork/extra-pkgs/scv/soft-contract/soft-contract/reduction/compile.rkt:129:8
;;   /Users/ben/code/racket/fork/extra-pkgs/scv/soft-contract/soft-contract/reduction/memoize.rkt:23:8: ⟦k⟧*
;;   [repeats 1 more time]
;;   /Users/ben/code/racket/fork/extra-pkgs/scv/soft-contract/soft-contract/reduction/main.rkt:145:4: for-loop
;;   /Users/ben/code/racket/fork/extra-pkgs/scv/soft-contract/soft-contract/reduction/main.rkt:45:4: loop!
;;   /Users/ben/code/racket/fork/extra-pkgs/scv/soft-contract/soft-contract/reduction/main.rkt:32:2: run
;;   .../more-scheme.rkt:261:28
;;   /Users/ben/code/racket/fork/extra-pkgs/scv/soft-contract/soft-contract/cmdline.rkt: [running body]
;;   /Users/ben/code/racket/fork/racket/collects/raco/raco.rkt: [running body]
;;   /Users/ben/code/racket/fork/racket/collects/raco/main.rkt: [running body]
;;shell: failed to apply '/usr/local/bin/raco' to arguments '(scv 05-provide.rkt.bak1)'

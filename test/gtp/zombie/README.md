zombie
===

FAILED
- `require-typed-scv` needs to implement recursive types
- untyped modules need to have contracts, but I refuse

```
$ raco scv zombie.rkt
σ@: no address #(struct:-α.wrp #(struct:-𝒾 image? /Users/ben/code/racket/my-pkgs/require-typed-scv/test/gtp/zombie/image.rkt))
  context...:
   /Users/ben/code/racket/fork/extra-pkgs/scv/soft-contract/soft-contract/reduction/compile.rkt:140:8
   /Users/ben/code/racket/fork/extra-pkgs/scv/soft-contract/soft-contract/reduction/main.rkt:145:4: for-loop
   /Users/ben/code/racket/fork/extra-pkgs/scv/soft-contract/soft-contract/reduction/main.rkt:45:4: loop!
   /Users/ben/code/racket/fork/extra-pkgs/scv/soft-contract/soft-contract/reduction/main.rkt:32:2: run
   .../more-scheme.rkt:261:28
   /Users/ben/code/racket/fork/extra-pkgs/scv/soft-contract/soft-contract/cmdline.rkt: [running body]
   /Users/ben/code/racket/fork/racket/collects/raco/raco.rkt: [running body]
   /Users/ben/code/racket/fork/racket/collects/raco/main.rkt: [running body]
```


History
---

Config where only `main.rkt` is typed

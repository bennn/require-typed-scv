zordoz
===

FAILURE
hash/c in library

```
$ raco make zo-find.rkt
"zo-find.rkt":
making #<path:/Users/ben/code/racket/my-pkgs/require-typed-scv/test/gtp/zordoz/zo-find.rkt>
making #<path:/Users/ben/code/racket/my-pkgs/require-typed-scv/private/parse-type.rkt>
making #<path:/Users/ben/code/racket/my-pkgs/require-typed-scv/private/require-typed-scv.rkt>
making #<path:/Users/ben/code/racket/my-pkgs/require-typed-scv/main.rkt>
/Users/ben/code/racket/fork/pkgs/zo-lib/compiler/zo-structs.rkt:73:54: hash/c: unbound identifier in module
context...:
#(100231 module) #(100232 module zo-structs 0) #(101805 use-site)
#(101807 use-site) #(110042 local) #(110043 intdef)
other binding...:
#<module-path-index:(soft-contract/fake-contract)>
#(-372332593401462910 module)
#(-372332593401462909 module expand-keep-contracts 0) #(100231 module)
#(100232 module zo-structs 0)
in: hash/c
context...:
/Users/ben/code/racket/fork/racket/collects/racket/contract/private/arrow-higher-order.rkt:342:33
/Users/ben/code/racket/fork/extra-pkgs/scv/soft-contract/soft-contract/parse/expand.rkt:65:0: do-expand
/Users/ben/code/racket/fork/extra-pkgs/scv/soft-contract/soft-contract/parse/private.rkt:66:2: parse-files
/Users/ben/code/racket/fork/extra-pkgs/scv/soft-contract/soft-contract/parse/main.rkt:18:2: parse-files
/Users/ben/code/racket/fork/extra-pkgs/scv/soft-contract/soft-contract/verifier.rkt:24:2: havoc-files
/Users/ben/code/racket/fork/racket/collects/racket/private/more-scheme.rkt:261:28
/Users/ben/code/racket/fork/racket/collects/racket/private/more-scheme.rkt:163:2: select-handler/no-breaks
/Users/ben/code/racket/fork/extra-pkgs/scv/soft-contract/soft-contract/cmdline.rkt: [running body]
/Users/ben/code/racket/fork/racket/collects/raco/raco.rkt: [running body]
/Users/ben/code/racket/fork/racket/collects/raco/main.rkt: [running body]
shell: failed to apply '/usr/local/bin/raco' to arguments '(scv zo-transition.rkt.0.bak)'
compilation context...:
/Users/ben/code/racket/my-pkgs/require-typed-scv/test/gtp/zordoz/zo-find.rkt
```


History
---

- changed `zo` type to `Any`, just to see if things would work

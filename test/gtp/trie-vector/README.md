trie-vector
===

FAILURE
need `hash/c`, also verification fails even on the simplified `pfds` code


History
---

Similar to John Clement's trie code from the mailing list.

Changes:
- trie datatype is a vector, not a struct
- removed `with-handlers`, `(match ... ((list null null) ....) ....)`
  because scv does not support them
- code probably doesn't compute the right result

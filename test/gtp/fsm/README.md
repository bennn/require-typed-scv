fsm
==

FAILURE

Not sure why

```
require-typed-scv: running SCV on 'population.rkt' with spec '((build-random-population (-> exact-nonnegative-integer? (cons/c (vectorof automaton?) (vectorof automaton?)))) (population-payoffs (-> (cons/c (vectorof automaton?) (vectorof automaton?)) (listof exact-nonnegative-integer?))) (death-birth (-> (cons/c (vectorof automaton?) (vectorof automaton?)) exact-nonnegative-integer? (or/c #f real?) (cons/c (vectorof automaton?) (vectorof automaton?)))) (match-up* (-> (cons/c (vectorof automaton?) (vectorof automaton?)) exact-nonnegative-integer? (cons/c (vectorof automaton?) (vectorof automaton?)))))'
require-typed-scv: failed to verify 'population.rkt'
```


History
---

Adapted from FSM configuration 0100 (only main.rkt is typed).

Changelog:
- remove keyword arguments
- add contracts to utilities, automata


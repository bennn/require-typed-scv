#lang typed/racket

;; Run a Simulation of Interacting Automata
(random-seed 7480)

;; =============================================================================
(require require-typed-scv)
;; --- automata-adapted
(define-type Probability Nonnegative-Real)
(define-type Population (cons Automaton* Automaton*))
(define-type Automaton* [Vectorof Automaton])
(define-type Payoff Nonnegative-Real)

(define-type State Natural)
(define-type Transition* [Vectorof Transition])
(define-type Transition [Vectorof State])

(require require-typed-check)
(require/typed/check "automata.rkt"
 [#:opaque Automaton automaton?]
 (automaton-payoff (-> Automaton Payoff))
 (defects (-> Payoff Automaton))
 (cooperates (-> Payoff Automaton))
 (tit-for-tat (-> Payoff Automaton))
 (grim-trigger (-> Payoff Automaton))
 (make-random-automaton
  (-> Natural Automaton))
 (match-pair
   (-> Automaton Automaton Natural (values Automaton Automaton)))
 (automaton-reset
  (-> Automaton Automaton))
 (clone
  (-> Automaton Automaton))
)
;; -- end automata-adapted
(require/typed/scv "population.rkt"
 (build-random-population
  (-> Natural (Pairof (Vectorof Automaton) (Vectorof Automaton))))
 (population-payoffs (-> (Pairof (Vectorof Automaton) (Vectorof Automaton)) [Listof Natural]))
 (death-birth
  (-> (Pairof (Vectorof Automaton) (Vectorof Automaton)) Natural (U False Real) (Pairof (Vectorof Automaton) (Vectorof Automaton))))
 (match-up*
  (-> (Pairof (Vectorof Automaton) (Vectorof Automaton)) Natural (Pairof (Vectorof Automaton) (Vectorof Automaton))))
)
;(require/typed/scv "population.rkt"
; (build-random-population
;  (-> Natural Population))
; (population-payoffs (-> Population [Listof Payoff]))
; (death-birth
;  (-> Population Natural [#:random (U False Real)] Population))
; (match-up*
;  (-> Population Natural Population))
;)
(require/typed/scv "utilities.rkt"
 (relative-average (-> [Listof Real] Real Real))
)

;; effect: run timed simulation, create and display plot of average payoffs
;; effect: measure time needed for the simulation
(define (main)
   (simulation->lines
    (evolve (build-random-population 100) 1000 10 20))
   (void))

(: simulation->lines (-> [Listof Payoff] [Listof [List Integer Real]]))
;; turn average payoffs into a list of Cartesian points 
(define (simulation->lines data)
  (for/list : [Listof [List Integer Real]]
    ([d : Payoff (in-list data)][n : Integer (in-naturals)])
    (list n d)))

(: evolve (-> Population Natural Natural Natural [Listof Payoff]))
;; computes the list of average payoffs over the evolution of population p for
;; c cycles of of match-ups with r rounds per match and at birth/death rate of s
(define (evolve p c s r)
  (cond
    [(zero? c) '()]
    [else (define p2 (match-up* p r))
          ;; Note: r is typed as State even though State is not exported 
          (define pp (population-payoffs p2))
          (define p3 (death-birth p2 s #f))
          ;; Note: s same as r
          ({inst cons Payoff [Listof Payoff]}
           (cast (relative-average pp r) Payoff)
           ;; Note: evolve is assigned (-> ... [Listof Probability])
           ;; even though it is explicitly typed ... [Listof Payoff]
           (evolve p3 (- c 1) s r))]))

;; -----------------------------------------------------------------------------
(time (main))


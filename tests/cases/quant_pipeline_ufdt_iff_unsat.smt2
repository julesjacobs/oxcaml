; Front-end quantified pipeline (OXSMT_QUANT_PIPELINE) exemplar: a definitional equivalence
; `A(s) <-> (forall n. P(s,n))`, written as a Boolean `=` with a quantified side. The
; hand-coded classifier reads the `=` via read_term, which rejects the nested `forall`, so
; the whole lemma is DROPPED (sat-degrade sentinel -> unknown). The pipeline expands the
; iff both polarities and clausifies to `forall s n. (not (A s)) or (P s n)` and
; `forall s. (not (P s (k s))) or (A s)` (k a fresh Skolem function of s), zero dropped.
; With `A(s0)` the first clause gives `P(s0, n)` for all n, hence `P(s0, 5)`, contradicting
; `not (P s0 5)` -> unsat. OFF: unknown; ON: unsat. Both sound.
(set-logic UFLIA)
(set-info :status unsat)
(declare-sort S 0)
(declare-fun a (S) Bool)
(declare-fun p (S Int) Bool)
(declare-fun s0 () S)
(assert (forall ((s S)) (= (a s) (forall ((n Int)) (p s n)))))
(assert (a s0))
(assert (not (p s0 5)))
(check-sat)

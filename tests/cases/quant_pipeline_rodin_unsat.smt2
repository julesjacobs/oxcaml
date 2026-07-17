; Front-end quantified pipeline (OXSMT_QUANT_PIPELINE) exemplar: the Rodin goal shape
; `not (and (forall ...) (forall ...))`, a quantifier nested inside a term (under `not`/
; `and`), which the hand-coded classifier DROPS (sat-degrade sentinel -> unknown). The
; pipeline clausifies it: NNF turns it into `(exists x. not P x) or (exists y. not Q y)`,
; Skolemizes the top-level existentials to fresh CONSTANTS k1/k2, and lowers the ground
; disjunction `(not (P k1)) or (not (Q k2))` as a real assertion (zero dropped content).
; With the two universal lemmas it refutes: P/Q hold everywhere, so the disjunction is
; false -> unsat. OFF this file degrades to `unknown` (the refuting negation is dropped);
; ON it is `unsat`. Both are sound; :status is the true status.
(set-logic UFLIA)
(set-info :status unsat)
(declare-fun p (Int) Bool)
(declare-fun q (Int) Bool)
(assert (forall ((x Int)) (p x)))
(assert (forall ((y Int)) (q y)))
(assert (not (and (forall ((x Int)) (p x)) (forall ((y Int)) (q y)))))
(check-sat)

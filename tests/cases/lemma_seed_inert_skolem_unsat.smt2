; MBQI-lite ground-term seeding of a trigger-inert Skolem universal (lemmas-climb chunk 3).
; The positive `exists` under the `forall` Skolemizes to a fresh function `f` of the
; enclosing universal: `forall x. (and ((f x) = x) (>= x 0))`. The ONLY App subterm over
; `x` is the Skolem head `(f x)`, and `f` has NO ground occurrence, so E-matching can never
; fire this lemma (its trigger is ground-less). The ground `(q a)` registers `a` in the
; e-graph; seeding instantiates `x |-> a`, yielding `(and ((f a) = a) (>= a 0))`, whose
; `(>= a 0)` conjunct contradicts the ground `(< a 0)` -> unsat.
;
; This is the chunk-3 RED: with seeding DISABLED (OXSMT_LEMMA_SEED=0) the inert universal
; never instantiates, the ground check saturates with a live lemma, and the verdict
; degrades to `unknown` (THE SOUNDNESS RULE) instead of `unsat`.
(set-logic UFLIA)
(set-info :status unsat)
(declare-fun a () Int)
(declare-fun q (Int) Bool)
(assert (q a))
(assert (< a 0))
(assert (forall ((x Int)) (and (exists ((y Int)) (= y x)) (>= x 0))))
(check-sat)

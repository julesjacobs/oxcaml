; Refinement-shaped: a predicate over ints interacts with EUF congruence. is_pos(x) and
; ¬is_pos(y) with x=y is unsat — is_pos(x) ~ true, is_pos(y) ~ false, but x=y makes the
; two applications congruent, so true ~ false. Exercises the ⊤/⊥ predicate bridge over a
; LIA-sorted argument (the argument equality is a plain Int Eq, EUF-owned here).
(set-logic QF_UFLIA)
(set-info :status unsat)
(declare-fun is_pos (Int) Bool)
(declare-const x Int)
(declare-const y Int)
(assert (is_pos x))
(assert (not (is_pos y)))
(assert (= x y))
(check-sat)

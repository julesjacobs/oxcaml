; Small eq_diamond: each level forces x_i = x_{i+1} through one of two OR-branches, so
; transitively x0 = x3, contradicting (not (= x0 x3)) => unsat. Exercises EUF equality
; PROPAGATION + explanation across disjunctions — the same ask-time-explanation defect the
; #102 fix closes, in the UNSAT direction (diagnosis bucket #2, eq_diamond family, was
; unknown pre-fix). See logs/euf-explain-fix-design.md.
(set-logic QF_UF)
(set-info :status unsat)
(declare-sort U 0)
(declare-fun x0 () U)
(declare-fun x1 () U)
(declare-fun x2 () U)
(declare-fun x3 () U)
(declare-fun a0 () U)
(declare-fun b0 () U)
(declare-fun a1 () U)
(declare-fun b1 () U)
(declare-fun a2 () U)
(declare-fun b2 () U)
(assert (or (and (= x0 a0) (= a0 x1)) (and (= x0 b0) (= b0 x1))))
(assert (or (and (= x1 a1) (= a1 x2)) (and (= x1 b1) (= b1 x2))))
(assert (or (and (= x2 a2) (= a2 x3)) (and (= x2 b2) (= b2 x3))))
(assert (not (= x0 x3)))
(check-sat)

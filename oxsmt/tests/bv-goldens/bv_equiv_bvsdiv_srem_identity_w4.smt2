; symbolic-equivalence guard: (op) == independent reference, valid for every
; operand at this width, so the NEGATION is unsat. A regression of the op's
; expansion makes it sat.
(set-logic QF_UFBV)
(set-info :status unsat)
(declare-const a (_ BitVec 4))
(declare-const b (_ BitVec 4))
(assert (not (=> (not (= b (_ bv0 4))) (= a (bvadd (bvmul b (bvsdiv a b)) (bvsrem a b))))))
(check-sat)

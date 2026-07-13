; symbolic-equivalence guard: (op) == independent reference, valid for every
; operand at this width, so the NEGATION is unsat. A regression of the op's
; expansion makes it sat.
(set-logic QF_UFBV)
(set-info :status unsat)
(declare-const a (_ BitVec 8))
(declare-const b (_ BitVec 8))
(assert (not (= (= (bvcomp a b) (_ bv1 1)) (= a b))))
(check-sat)

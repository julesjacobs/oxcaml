; symbolic-equivalence guard: (op) == independent reference, valid for every
; operand at this width, so the NEGATION is unsat. A regression of the op's
; expansion makes it sat.
(set-logic QF_UFBV)
(set-info :status unsat)
(declare-const a (_ BitVec 3))
(declare-const b (_ BitVec 3))
(assert (not (= (bvnand a b) (bvor (bvnot a) (bvnot b)))))
(check-sat)

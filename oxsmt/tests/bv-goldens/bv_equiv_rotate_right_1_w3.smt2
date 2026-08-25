; symbolic-equivalence guard: (op) == independent reference, valid for every
; operand at this width, so the NEGATION is unsat. A regression of the op's
; expansion makes it sat.
(set-logic QF_UFBV)
(set-info :status unsat)
(declare-const a (_ BitVec 3))
(assert (not (= ((_ rotate_right 1) a) (bvor (bvlshr a (_ bv1 3)) (bvshl a (_ bv2 3))))))
(check-sat)

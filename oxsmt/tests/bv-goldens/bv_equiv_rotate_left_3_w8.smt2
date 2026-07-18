; symbolic-equivalence guard: (op) == independent reference, valid for every
; operand at this width, so the NEGATION is unsat. A regression of the op's
; expansion makes it sat.
(set-logic QF_UFBV)
(set-info :status unsat)
(declare-const a (_ BitVec 8))
(assert (not (= ((_ rotate_left 3) a) (bvor (bvshl a (_ bv3 8)) (bvlshr a (_ bv5 8))))))
(check-sat)

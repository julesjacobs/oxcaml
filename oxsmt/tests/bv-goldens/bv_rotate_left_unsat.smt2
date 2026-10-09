; rotate_left 1 of 0001 (width 4) = 0010 = 2.
(set-logic QF_UFBV)
(set-info :status unsat)
(assert (not (= ((_ rotate_left 1) (_ bv1 4)) (_ bv2 4))))
(check-sat)

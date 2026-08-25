; rotate_right 1 of 0001 (width 4) = 1000 = 8.
(set-logic QF_UFBV)
(set-info :status unsat)
(assert (not (= ((_ rotate_right 1) (_ bv1 4)) (_ bv8 4))))
(check-sat)

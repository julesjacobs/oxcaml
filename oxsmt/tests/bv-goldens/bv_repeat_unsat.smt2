; repeat 2 of 10 (width 2) = 1010 (width 4) = 10.
(set-logic QF_UFBV)
(set-info :status unsat)
(assert (not (= ((_ repeat 2) (_ bv2 2)) (_ bv10 4))))
(check-sat)

; bvnor = bvnot (bvor): 12 nor 10 = ~(1100 | 1010) = ~(1110) = 0001 = 1 (width 4).
(set-logic QF_UFBV)
(set-info :status unsat)
(assert (not (= (bvnor (_ bv12 4) (_ bv10 4)) (_ bv1 4))))
(check-sat)

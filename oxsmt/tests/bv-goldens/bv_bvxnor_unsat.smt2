; bvxnor = bvnot (bvxor): 12 xnor 10 = ~(1100 ^ 1010) = ~(0110) = 1001 = 9 (width 4).
(set-logic QF_UFBV)
(set-info :status unsat)
(assert (not (= (bvxnor (_ bv12 4) (_ bv10 4)) (_ bv9 4))))
(check-sat)

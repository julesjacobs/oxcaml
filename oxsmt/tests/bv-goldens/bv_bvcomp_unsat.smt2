; bvcomp: 1-bit equality reduction. 5 == 5 -> #b1 ; 5 vs 6 -> #b0.
(set-logic QF_UFBV)
(set-info :status unsat)
(assert (not (and (= (bvcomp (_ bv5 4) (_ bv5 4)) (_ bv1 1))
                  (= (bvcomp (_ bv5 4) (_ bv6 4)) (_ bv0 1)))))
(check-sat)

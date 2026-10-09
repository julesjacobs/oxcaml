; signed division: -6 sdiv 2 = -3 (width 4: bv10 sdiv bv2 = bv13). unsat = value confirmed.
(set-logic QF_UFBV)
(set-info :status unsat)
(assert (not (= (bvsdiv (_ bv10 4) (_ bv2 4)) (_ bv13 4))))
(check-sat)

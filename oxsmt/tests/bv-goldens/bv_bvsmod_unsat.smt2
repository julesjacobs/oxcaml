; signed modulo (sign of divisor): -7 smod 3 = 2 (width 4: bv9 smod bv3 = bv2).
(set-logic QF_UFBV)
(set-info :status unsat)
(assert (not (= (bvsmod (_ bv9 4) (_ bv3 4)) (_ bv2 4))))
(check-sat)

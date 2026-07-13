; signed remainder (sign of dividend): -7 srem 3 = -1 (width 4: bv9 srem bv3 = bv15).
(set-logic QF_UFBV)
(set-info :status unsat)
(assert (not (= (bvsrem (_ bv9 4) (_ bv3 4)) (_ bv15 4))))
(check-sat)

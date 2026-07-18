; bvnand = bvnot (bvand): 12 nand 10 = ~(1100 & 1010) = ~(1000) = 0111 = 7 (width 4).
(set-logic QF_UFBV)
(set-info :status unsat)
(assert (not (= (bvnand (_ bv12 4) (_ bv10 4)) (_ bv7 4))))
(check-sat)

; div/mod certified case: (mod x 4) is in [0,4), so >= 4 is impossible -> unsat.
; Post euclidean elimination: x = 4*q + r, 0 <= r < 4, and r >= 4 -> grind proves False.
(set-logic QF_LIA)
(set-info :status unsat)
(declare-const x Int)
(assert (>= (mod x 4) 4))
(check-sat)

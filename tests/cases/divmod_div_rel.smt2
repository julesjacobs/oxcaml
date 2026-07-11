; div certified case: 4*(div x 4) = x - (mod x 4), so 4*(div x 4) >= x + 1 forces
; (mod x 4) <= -1, impossible since mod >= 0 -> unsat. Elimination: x = 4*q + r,
; 0 <= r < 4, 4*q >= x + 1 -> grind proves False.
(set-logic QF_LIA)
(set-info :status unsat)
(declare-const x Int)
(assert (>= (* 4 (div x 4)) (+ x 1)))
(check-sat)

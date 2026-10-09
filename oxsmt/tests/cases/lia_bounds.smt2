; LIA bounds: 0 <= x <= 0 forces x = 0, so x != 0 is unsat.
(set-logic QF_LIA)
(set-info :status unsat)
(declare-const x Int)
(assert (>= x 0))
(assert (<= x 0))
(assert (distinct x 0))
(check-sat)

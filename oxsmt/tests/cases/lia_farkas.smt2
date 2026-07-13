; LIA Farkas conflict: x >= 2 and y >= 1 give x + y >= 3, contradicting x + y <= 2.
(set-logic QF_LIA)
(set-info :status unsat)
(declare-const x Int)
(declare-const y Int)
(assert (<= (+ x y) 2))
(assert (>= x 2))
(assert (>= y 1))
(check-sat)

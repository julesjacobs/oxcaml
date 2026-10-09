; ite: (ite (x > 0) 1 0) = 1 requires x > 0, contradicting x <= 0.
(set-logic QF_LIA)
(set-info :status unsat)
(declare-const x Int)
(assert (= (ite (> x 0) 1 0) 1))
(assert (<= x 0))
(check-sat)

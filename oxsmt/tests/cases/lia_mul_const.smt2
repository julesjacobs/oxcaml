; Linear multiplication by a constant: 2*x = 4 forces x = 2, so x != 2 is unsat.
(set-logic QF_LIA)
(set-info :status unsat)
(declare-const x Int)
(assert (= (* 2 x) 4))
(assert (distinct x 2))
(check-sat)

; let-binding: t := x + 1; with x = 1 we get t = 2, so asserting t = 5 is unsat.
(set-logic QF_LIA)
(set-info :status unsat)
(declare-const x Int)
(assert (= x 1))
(assert (let ((t (+ x 1))) (= t 5)))
(check-sat)

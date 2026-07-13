; Fixture: a satisfiable LIA query (x > 0 and x < 2 over the integers).
(set-logic QF_LIA)
(set-info :status sat)
(declare-fun x () Int)
(assert (> x 0))
(assert (< x 2))
(check-sat)

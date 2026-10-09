; Satisfiable LIA: 0 <= x <= 5 and x = 3. Model in lia_sat.model.
(set-logic QF_LIA)
(set-info :status sat)
(declare-const x Int)
(assert (>= x 0))
(assert (<= x 5))
(assert (= x 3))
(check-sat)

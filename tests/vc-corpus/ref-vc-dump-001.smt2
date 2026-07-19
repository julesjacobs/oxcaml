(set-option :produce-unsat-cores true)
(assert (not (= (>= 3 3) true)))
(check-sat-assuming ())
(get-unsat-core)

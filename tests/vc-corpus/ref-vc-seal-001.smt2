(set-option :produce-unsat-cores true)
(assert (not (= (= 1 1) true)))
(check-sat-assuming ())
(get-unsat-core)

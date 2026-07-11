; Satisfiable Bool-sorted =: (= p q) holds when p and q share a truth value.
; Model (iff_sat.model): p := true, q := true.
(set-logic QF_UF)
(set-info :status sat)
(declare-const p Bool)
(declare-const q Bool)
(assert (= p q))
(check-sat)

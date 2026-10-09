; Pure-Boolean: De Morgan is a tautology, so its negation (via Bool-sorted =, an iff) is
; unsat. Exercises and/or/not plus iff in the wired clausifier. Gate-certified.
(set-logic QF_UF)
(set-info :status unsat)
(declare-const p Bool)
(declare-const q Bool)
(assert (not (= (not (and p q)) (or (not p) (not q)))))
(check-sat)

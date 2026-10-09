; Pure-Boolean: a variable and its negation cannot both hold. Unsat propositionally
; (the SAT core alone decides it; no theory reasoning). Gate-certified.
(set-logic QF_UF)
(set-info :status unsat)
(declare-const p Bool)
(assert (and p (not p)))
(check-sat)

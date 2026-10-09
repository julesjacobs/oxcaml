; Pure-Boolean (no theory atoms): negating the law of excluded middle is unsat.
; A real propositional verdict from the wired solver (M1-wiring), gate-certified.
(set-logic QF_UF)
(set-info :status unsat)
(declare-const p Bool)
(assert (not (or p (not p))))
(check-sat)

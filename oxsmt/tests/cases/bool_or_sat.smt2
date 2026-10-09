; Pure-Boolean satisfiable: (p or q) with q false forces p true. The wired solver returns
; a real model (only propositional variables). Model in bool_or_sat.model. Gate-certified.
(set-logic QF_UF)
(set-info :status sat)
(declare-const p Bool)
(declare-const q Bool)
(assert (or p q))
(assert (not q))
(check-sat)

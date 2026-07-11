; DEGRADATION HONEYPOT (LIA). x<0 and x>0 are two independent atoms to the propositional
; core, so the boolean skeleton is SATISFIABLE — but the theory is UNSAT. The declared
; :status is unsat and the wired v1 solver must answer `unknown` (THE SOUNDNESS RULE:
; theory atoms present ⇒ a propositional sat is downgraded). If wiring ever regresses to
; answering `sat`, the harness LABEL check fires as a SOUNDNESS failure (red), not a mere
; golden diff. When LIA (M3) lands this should become a real `unsat`.
(set-logic QF_LIA)
(set-info :status unsat)
(declare-const x Int)
(assert (< x 0))
(assert (> x 0))
(check-sat)

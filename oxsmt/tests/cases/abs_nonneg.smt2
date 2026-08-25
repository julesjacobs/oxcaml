; abs certified case: (abs x) >= 0 always, so (abs x) < 0 is unsat. abs desugars to
; ite(x >= 0, x, -x); grind (open Classical) discharges the ite. Exercises abs end to end.
(set-logic QF_LIA)
(set-info :status unsat)
(declare-const x Int)
(assert (< (abs x) 0))
(check-sat)

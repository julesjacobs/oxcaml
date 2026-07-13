; Fixture: push/pop with two check-sats — exercises the "one verdict block per
; check-sat" contract. The harness produces one (goal ...) per check-sat, in
; order, and the in-effect :status is tracked independently for each.
(set-logic QF_LIA)
(declare-fun x () Int)
(push 1)
(set-info :status sat)
(assert (> x 0))
(check-sat)
(pop 1)
(push 1)
(set-info :status unsat)
(assert (< x 0))
(assert (> x 0))
(check-sat)
(pop 1)

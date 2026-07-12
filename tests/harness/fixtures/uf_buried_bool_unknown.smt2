; Known-gap fixture (ADR-UF-models R4c + the wiring-bool-leaf-forwarding gap): a
; Bool-codomain predicate whose argument never SURFACES as a SAT atom. h(b) must differ
; from both h(true) and h(false); this is genuinely UNSAT (b is true or false, forcing
; h(b) to equal one of them), but a bare Bool variable b is a propositional variable the
; seam does not forward to the theory, so from the combinator's view b under h is
; buried/undetermined and it degrades via Combine.Incomplete. The SOLVER therefore returns
; a sound UNKNOWN — never a guessed function-table cell, never a wrong SAT. The golden
; pins that unknown; a regression that flipped this to sat with a fabricated Bool cell
; would change the golden (and, on a real model, be caught by the eval self-check).
(set-logic QF_UF)
(set-info :status unsat)
(declare-fun h (Bool) Bool)
(declare-const b Bool)
(assert (not (= (h b) (h true))))
(assert (not (= (h b) (h false))))
(check-sat)

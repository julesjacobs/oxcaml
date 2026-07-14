; A Bool-codomain predicate whose argument is a BARE Bool variable that never SURFACES as a
; SAT atom (ADR-UF-models R4c). h(b) must differ from both h(true) and h(false); this is
; genuinely UNSAT (b is true or false, forcing h(b) to equal one of them). Before the
; Bool-cardinality completeness fix a bare Bool variable b buried under h was a propositional
; variable the seam did not forward to the theory, so from the combinator's view b under h was
; buried/undetermined and it degraded via Combine.Incomplete to a sound UNKNOWN — never a wrong
; sat, but incomplete.
;
; {!Session.register_bool_terms} + {!Cdclt.bind_bool_var_atom} now bind b to its propositional
; SAT var as an EUF [K_bool] atom, so the SAT core decides it and congruence + the
; [true <> false] axiom refutes the two clauses on whichever branch b takes. The solver now
; returns the correct UNSAT (matching :status). A regression that flipped this to sat with a
; fabricated Bool cell would change the golden (and, on a real model, be caught by the eval
; self-check).
(set-logic QF_UF)
(set-info :status unsat)
(declare-fun h (Bool) Bool)
(declare-const b Bool)
(assert (not (= (h b) (h true))))
(assert (not (= (h b) (h false))))
(check-sat)

; Degradation case: the empty quoted symbol |...| (name "") is accepted by the parser but
; is NOT a printable SMT-LIB symbol, so the shipped printer's quote_symbol refuses it. The
; true answer is sat (|| = true), but the solver CLI cannot emit a well-formed model for
; an unrepresentable name, so it degrades this goal to a sound `unknown` with no model
; rather than crash or emit malformed output. `unknown` vs the sat label is a completeness
; gap, not a soundness failure. Regression for the harness-quoted CLI catch-and-degrade.
(set-logic QF_UF)
(set-info :status sat)
(declare-const || Bool)
(assert ||)
(check-sat)

; Degradation case: the quoted symbol |+| (name "+") is accepted by the parser as an
; ordinary declared const, but "+" collides with a predefined operator, so the shipped
; printer's quote_symbol refuses it (printing it bare or |quoted| would be ambiguous). As
; with the empty-symbol case, the true answer is sat but the CLI degrades this goal to a
; sound `unknown` with no model rather than crash. Regression for the CLI catch-and-degrade
; (sibling of bool_empty_symbol_unknown).
(set-logic QF_UF)
(set-info :status sat)
(declare-const |+| Bool)
(assert |+|)
(check-sat)

; HONEYPOT (a', EUF): claims unsat, but "a and b distinct" is satisfiable in any
; sort of size >= 2. Audits that the EUF / distinct / Fin sat-witness path can go
; red: the gate must NOT certify, and with the witness (S = Fin 2, a := 0, b := 1)
; it should produce REFUTED.
(set-logic QF_UF)
(set-info :status unsat)
(declare-sort S 0)
(declare-const a S)
(declare-const b S)
(assert (distinct a b))
(check-sat)

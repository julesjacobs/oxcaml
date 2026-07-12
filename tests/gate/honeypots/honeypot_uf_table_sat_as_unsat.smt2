; HONEYPOT (§5 coverage gap: drive a NON-NULLARY declare-fun through the SAT path). Claims
; unsat, but "a and b distinct with f collapsing them (f a = f b)" is satisfiable in any
; sort of size >= 2 (this is exactly euf_sat, relabelled). Audits that the FUNCTION-TABLE
; sat-witness path can turn the gate red: the gate must NOT certify the unsat claim, and
; with the witness (S = Fin 2, a := 0, b := 1, f collapses both to 0) it must REFUTED.
(set-logic QF_UF)
(set-info :status unsat)
(declare-sort S 0)
(declare-fun f (S) S)
(declare-const a S)
(declare-const b S)
(assert (distinct a b))
(assert (= (f a) (f b)))
(check-sat)

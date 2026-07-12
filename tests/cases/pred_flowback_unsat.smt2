; Predicate ⊤/⊥ flow-back drives a purely-Boolean consequence: p(a) and a=b entail p(b)
; by congruence; the clause (or (not (p b)) c) then forces c, contradicting (not c).
; Without predicate theory propagation this is still unsat (found reactively via the
; true<>false axiom); the case locks in the interaction end-to-end.
(set-logic QF_UF)
(set-info :status unsat)
(declare-sort S 0)
(declare-fun p (S) Bool)
(declare-const a S)
(declare-const b S)
(declare-const c Bool)
(assert (p a))
(assert (= a b))
(assert (or (not (p b)) c))
(assert (not c))
(check-sat)

; HONEYPOT (§5 coverage gap: drive a NON-NULLARY declare-fun through the SAT path, with
; LOAD-BEARING case rows). Claims unsat, but this is satisfiable in a sort of size >= 2 by
; a NON-CONSTANT f: distinct a b, f(a) and f(b) distinct, and f(a) pinned to a. The witness
; (S = Fin 2, a := 0, b := 1, f := [0->0, 1->1]) needs the case rows to DIFFER from each
; other and from the default — a transport bug that dropped/misread the (case ...) rows and
; kept only (default d) would make f constant, so f(a)=f(b) would violate (distinct (f a)
; (f b)) and Lean would NOT refute: the honeypot then fails its REFUTED .expect and turns
; the gate red. So this pins case-row transport, unlike a collapse-to-default witness.
(set-logic QF_UF)
(set-info :status unsat)
(declare-sort S 0)
(declare-fun f (S) S)
(declare-const a S)
(declare-const b S)
(assert (distinct a b))
(assert (distinct (f a) (f b)))
(assert (= (f a) a))
(check-sat)

; Satisfiable QF_UF with a SURFACED predicate table (ADR-UF-models §4/R4c).
; p a and (not (p b)) force p(a)=true, p(b)=false, so a and b are distinct and p is a
; genuine Bool-codomain function table (both p(a) and p(b) surface as SAT atoms, so the
; cells are bound true/false — never the buried/undetermined H2 class that degrades).
; Model (uf_predicate_sat.model): S = Fin 2, a := 0, b := 1, p := [0->true, 1->false].
(set-logic QF_UF)
(set-info :status sat)
(declare-sort S 0)
(declare-fun p (S) Bool)
(declare-const a S)
(declare-const b S)
(assert (p a))
(assert (not (p b)))
(check-sat)

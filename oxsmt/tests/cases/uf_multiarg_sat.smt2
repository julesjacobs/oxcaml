; Satisfiable QF_UF with a MULTI-ARG (binary) function table (ADR-UF-models §1: n-ary
; App rows keyed by argument-INDEX tuples). g(a,b)=a and g(b,a)=b with a,b distinct is
; satisfiable — the two argument tuples (0,1) and (1,0) are distinct, so no congruence
; forces g(a,b)=g(b,a). Exercises the arity-2 table (case tuples of length 2).
; Model (uf_multiarg_sat.model): S = Fin 2, a := 0, b := 1, g := [(0,1)->0, (1,0)->1].
(set-logic QF_UF)
(set-info :status sat)
(declare-sort S 0)
(declare-fun g (S S) S)
(declare-const a S)
(declare-const b S)
(assert (distinct a b))
(assert (= (g a b) a))
(assert (= (g b a) b))
(check-sat)

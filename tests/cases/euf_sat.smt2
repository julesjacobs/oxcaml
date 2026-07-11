; Satisfiable EUF: a and b distinct, but f collapses them (f a = f b).
; Model (euf_sat.model): S = Fin 2, a := 0, b := 1, f := const 0.
(set-logic QF_UF)
(set-info :status sat)
(declare-sort S 0)
(declare-fun f (S) S)
(declare-const a S)
(declare-const b S)
(assert (distinct a b))
(assert (= (f a) (f b)))
(check-sat)

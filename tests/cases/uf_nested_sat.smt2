; Satisfiable QF_UF with a NESTED application f(g(a)) (ADR-UF-models §1 R4a: extraction
; must recurse into the full subterm closure — a shallow group-by would miss the g(a) row
; and silently default it). (f (g a)) <> (g a) is satisfiable; the model needs BOTH a row
; for g at a AND a row for f at g(a). If g(a)'s row were dropped, the evaluator would read
; g(a) from the wrong cell and reject the model — this fixture is the recursive-closure
; regression guard.
; Model (uf_nested_sat.model): S = Fin 2, a := 0, g := [0->0] (default 0), f := [0->1].
(set-logic QF_UF)
(set-info :status sat)
(declare-sort S 0)
(declare-fun f (S) S)
(declare-fun g (S) S)
(declare-const a S)
(assert (not (= (f (g a)) (g a))))
(check-sat)

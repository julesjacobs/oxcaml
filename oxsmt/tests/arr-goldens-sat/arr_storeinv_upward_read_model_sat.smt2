; A satisfiable nested storeinv-shape query (Armando/Bonacina/Ranise/Schulz PDPAR'05).
; Pre-fix the arrays engine saturated on an under-constrained arrangement — reads on the
; declared arrays a1/a2 were never propagated up to the store terms the assertion mentions
; — so the extracted model failed the independent Array_model_check and the query degraded
; to a (sound) unknown. Upward read propagation (ensure_store_reads in arr.ml) drives the
; engine to a complete arrangement whose extracted model the checker accepts: a CHECKED sat.
; RED (unknown) against the pre-fix engine, GREEN (checked sat) after.
(set-info :smt-lib-version 2.6)
(set-logic QF_AX)
(set-info :status sat)
(declare-sort Index 0)
(declare-sort Element 0)
(declare-fun a1 () (Array Index Element))
(declare-fun a2 () (Array Index Element))
(declare-fun i1 () Index)
(declare-fun i2 () Index)
(assert (let ((?v_0 (store a2 i1 (select a1 i1))) (?v_1 (store a1 i1 (select a2 i1)))) (= (store ?v_1 i1 (select ?v_0 i2)) (store ?v_0 i2 (select ?v_1 i2)))))
(assert (not (= a1 a2)))
(check-sat)
(exit)

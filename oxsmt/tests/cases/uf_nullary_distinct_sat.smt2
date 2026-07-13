; Satisfiable QF_UF, NULLARY-uninterpreted slice (review C1): const-only over an
; uninterpreted sort, NO function table. This is the 3.1% "nullary fast path" the ADR §7
; promises not to regress. Cdclt.model populates sort_cards for a,b:S even with no function,
; so the model is sort-bearing-but-table-free; the CLI renders it through the sidecar body
; (sort + consts, no fun). Locks distinct a b => sat AT THE CLI so this class can never
; silently regress to unknown again (the exact C1 gap: no committed sat golden declared an
; uninterpreted sort before this fixture).
; Model (uf_nullary_distinct_sat.model): S = Fin 2, a := 0, b := 1.
(set-logic QF_UF)
(set-info :status sat)
(declare-sort S 0)
(declare-const a S)
(declare-const b S)
(assert (distinct a b))
(check-sat)

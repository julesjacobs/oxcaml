; Satisfiable QF_UF where a BARE nullary Bool variable is used ONLY as an uninterpreted-
; function argument (never at top level). [p] is buried inside [h p], so the clausifier's
; Boolean skeleton gives it no SAT variable and EUF cannot bind it to true/false — it would
; stay a third opaque Boolean class and Combine's H2 guard ([require_bool_args_bound])
; degrades the query to [unknown] (that is exactly what trunk c37fee56d7 does here).
;
; The Bool-cardinality rule ({!Session.register_bool_terms} +
; {!Cdclt.bind_bool_var_atom}) closes the completeness gap for a bare buried Bool variable:
; [p] is bound to its propositional SAT var as an EUF [K_bool] atom, so the SAT core decides
; it and EUF binds it — the query flips to [sat] with a self-checked model. Distinct from
; [uf_predicate_sat] (a SURFACED applied predicate); this pins the bare-variable arm.
(set-logic QF_UF)
(set-info :status sat)
(declare-fun h (Bool) Bool)
(declare-fun p () Bool)
(assert (h p))
(check-sat)

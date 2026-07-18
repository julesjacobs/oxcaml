; UNSAT QF_UF that exercises the SOUNDNESS case Combine's H2 guard protects. [p] is a bare
; Bool variable used only as the argument of [h]; [h p], [h true], [h false] are asserted
; pairwise-distinct. Since [p] must be true or false, [h p] is congruent to [h true] or
; [h false] — so the distinctness is unsatisfiable (a 3-into-2 pigeonhole over the Bool
; domain). Trunk c37fee56d7 leaves [p] an opaque third Boolean class, cannot see the
; pigeonhole, and DEGRADES to [unknown] (sound but incomplete).
;
; With [p] bound to its propositional SAT var as an EUF [K_bool] atom
; ({!Cdclt.bind_bool_var_atom}), the SAT core decides [p] and EUF discharges the pigeonhole
; by congruence + the [true <> false] axiom on whichever branch it takes: the query is now
; correctly [unsat]. This is the discriminating soundness test — the completeness fix must
; DECIDE the buried Bool variable, never fabricate a [sat] the guard was preventing.
(set-logic QF_UF)
(set-info :status unsat)
(declare-sort S 0)
(declare-fun h (Bool) S)
(declare-fun p () Bool)
(assert (distinct (h p) (h true) (h false)))
(check-sat)

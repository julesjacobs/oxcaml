val p : bool
(* The wildcard let is semantically inert, but its binder and RHS both retain
   the carrier type ['a] inside the lowered predicate.  This exercises type-
   variable substitution in copied alias predicates, not only in the alias's
   outer skeleton. *)
type 'a law = 'a{ let _ = _ in p = true }
val proof : unit law
val list_proof : int list law

(* Alias edge (known accepted limitation): the same dangling provider parameter
   is freshened independently via the direct import ([Cup_provider.identity]) and
   via the re-export ([Cup_reexport.rident]), so the two carry different fresh
   stamps and their refinements no longer unify.  Unifying them (the two [if]
   branches) is a fail-closed rigid clash -- a spurious REJECT, acceptable
   because soundness is never traded for it.  Applying either alias on its own
   still works (see cup_pos.ml). *)
let pick flag = if flag then Cup_provider.identity else Cup_reexport.rident

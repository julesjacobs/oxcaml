(* Accepted incompleteness: a cross-unit dependent result is left opaque (the
   imported parameter is not substituted by the argument), so even the TRUE claim
   [Cup_provider.identity 3 : int{ _ = 3 }] does not prove.  Pinned so any future
   re-introduction of argument substitution is a deliberate, reviewed change. *)
let g : int{ _ = 3 } = Cup_provider.identity 3

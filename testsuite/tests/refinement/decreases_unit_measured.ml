(* Compiled in the same invocation as [decreases_unit_lifetime.ml], and
   before it.  The measure recorded here is keyed on a local identifier, and
   identifier stamps start again at each compilation unit. *)
let[@vox.decreases n] rec countdown (n : int{ _ >= 0 }) : int =
  if n = 0 then 0 else countdown (n - 1)

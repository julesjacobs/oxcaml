(* Same false claim, reached through a let-binder whose inferred type is the
   imported result refinement.  Must also be REJECTED. *)
let leak (x : int) =
  let a = Cup_provider.identity 0 in
  (a : int{ _ = x })

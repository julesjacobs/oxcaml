(* Original hole via the binder-fact path: same false claim through a let-binder
   whose inferred type is the imported result refinement. *)
let leak (x : int) =
  let a = Cup_provider.identity 0 in
  (a : int{ _ = x })

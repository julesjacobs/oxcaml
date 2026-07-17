(* [bad] returns [Cup_provider.identity 0 = 0], which equals its own argument
   [x] only when [x = 0].  Must be REJECTED: the imported callee's parameter
   must not be laundered into the caller's [x] by a stamp collision. *)
let bad (x : int) : int{ _ = x } = Cup_provider.identity 0

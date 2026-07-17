(* Original hole (parameter, result-fact path): [bad] returns
   [Cup_provider.identity 0 = 0], equal to its argument [x] only when [x = 0].
   Must be REJECTED -- the imported parameter must not be laundered into [x]. *)
let bad (x : int) : int{ _ = x } = Cup_provider.identity 0

(* Provider compiled as a SEPARATE unit, so its parameter [x] carries this
   unit's own local stamps.  Imported below, those foreign stamps must not be
   conflated with the caller's local binders. *)
let identity (x : int) : int{ _ = x } = x
let add1 (x : int) : int{ _ = x + 1 } = x + 1

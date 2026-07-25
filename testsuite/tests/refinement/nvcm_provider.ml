(* The claim is false: a verifying compilation disproves it.  Under
   [-vox-no-verify] it is accepted and reaches the interface unchecked. *)
let value : int{ _ = 0 } = 1

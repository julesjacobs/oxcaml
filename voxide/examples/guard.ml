(* Guarding a refined call.

   [need_pos] demands a positive argument.  Calling it under [if y > 0]
   discharges the contract from the branch fact; the [else] branch never
   calls it, so nothing is required there. *)

let need_pos (x : int{ _ > 0 }) = x

let use_guard (y : int) =
  if y > 0 then need_pos y else 0

(* Take from a slot that was never filled, at type string. *)
module Slot = Unique_cell.Slot
let () =
  let r : string Slot.t Slot.step = Slot.empty () (Ghost_pref.empty ()) in
  let s = Slot.take r.value r.state in
  print_int (String.length s.value); print_newline ()

external poll : unit -> unit = "%poll"

(* This test checks that a CFG [Poll] reaches the LLVM backend instead of being
   dropped. *)

let[@inline never] [@local never] run n =
  poll ();
  n + 1

(* A library unit with refined functions, used across modules.

   [pos] demands and promises a positive int; [one] promises a positive
   int.  The obligations they induce arise at the CALL sites in the
   client unit (see Client.ml). *)

let pos (x : int{ _ > 0 }) : int{ _ > 0 } = x

let one () : int{ _ > 0 } = 1

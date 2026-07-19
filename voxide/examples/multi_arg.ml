(* Several refined arguments on one call.

   [add_pos] demands both arguments be positive and promises a positive
   sum.  The single call [add_pos 3 4] carries TWO contract obligations
   -- [3 > 0] and [4 > 0] -- on the one line, plus the result
   annotation [a + b > 0] on the body. *)

let add_pos (a : int{ _ > 0 }) (b : int{ _ > 0 }) : int{ _ > 0 } = a + b

let use = add_pos 3 4

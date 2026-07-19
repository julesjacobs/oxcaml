(* Nested refined calls: an obligation on a deep subterm.

   In [pos (pos 5)] the inner call carries [5 > 0] (the argument [5]);
   the outer call carries [pos 5 > 0] (the argument is the whole inner
   call), discharged from the inner call's [_ > 0] result contract. *)

let pos (x : int{ _ > 0 }) : int{ _ > 0 } = x

let use = pos (pos 5)

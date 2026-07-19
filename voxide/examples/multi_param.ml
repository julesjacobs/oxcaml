(* A result contract that mentions a parameter.

   [at_least n] returns a value refined by [_ >= n]: the result contract
   depends on the first argument.  At the call [at_least 2 5] the
   argument obligation is [5 >= 2] and the body must meet [x >= n]. *)

let at_least (n : int) (x : int{ _ >= n }) : int{ _ >= n } = x

let use = at_least 2 5

(* Contracts that mention earlier arguments.

   The second parameter's refinement [int{ _ = n }] refers to the first
   parameter [n].  At the call [dep 3 3] the obligation is [3 = 3]. *)

let dep (n : int) (a : int{ _ = n }) = a

let use_dep = dep 3 3

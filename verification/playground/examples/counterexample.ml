(* A claim that does not hold is rejected with a counterexample: values of
   the variables in scope for which it fails.

   OCaml's int is a 63-bit machine integer, so x + 1 > x is not a theorem:
   it fails at max_int, where x + 1 wraps around. [next] states the
   precondition that rules this out; [succ] does not. *)

let (next @ total) (x : {x : int | x < max_int}) : {r : int | r > x} = x + 1

let (succ @ total) (x : int) : {r : int | r > x} = x + 1

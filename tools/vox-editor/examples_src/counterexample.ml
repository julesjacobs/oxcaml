(* The docs/vox demo page's "when you're wrong" example: verification
   fails and the solver hands back a concrete counterexample -- goal,
   hypotheses, witness. *)

let rec total_ fib n =
  if n <= 0 then 0 else if n = 1 then 1 else fib (n - 1) + fib (n - 2)
[@@vox.decreases n]

(* Off by one: fib is not n+1.  The compiler says so, with a witness.
   The result refinement is written directly on the annotation. *)
let wrong (n : int) : int{ _ = fib n } = n + 1

(* The docs/vox demo page's "when you're wrong" example, kept here so
   CI verifies exactly the failure output the page shows: goal,
   hypotheses, counterexample. *)

let rec total_ fib n =
  if n <= 0 then 0 else if n = 1 then 1 else fib (n - 1) + fib (n - 2)
[@@vox.decreases n]

(* Off by one: fib is not n+1.  The compiler says so, with a witness. *)
let wrong : (n : int) -> int{ _ = fib n } = fun n -> refine_ (n + 1)

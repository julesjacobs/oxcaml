(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* The docs/vox demo page's "when you're wrong" example, kept here so
   CI verifies exactly the failure output the page shows: goal,
   hypotheses, counterexample. *)

let rec total_ fib n =
  if n <= 0 then 0 else if n = 1 then 1 else fib (n - 1) + fib (n - 2)
[@@vox.decreases n]

(* Off by one: fib is not n+1.  The compiler says so, with a witness. *)
let wrong : (n : int) -> int{ _ = fib n } = fun n -> refine_ (n + 1)
[%%expect{|
val fib : int -> int = <fun>
Line 6, characters 61-68:
6 | let wrong : (n : int) -> int{ _ = fib n } = fun n -> refine_ (n + 1)
                                                                 ^^^^^^^
Error: vox: verification failed (lean).
       Goal: (n + 1) = (fib n)
Hypotheses: <none>
Possible counterexample:
  n = 0
  fib n = 0
(lean: error: `grind` failed)
|}]

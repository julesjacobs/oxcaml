(* TEST
 expect;
*)

(* ============================================================= *)
(* ACCEPTANCE CORPUS: recursion (induction hypothesis)            *)
(*                                                                *)
(* plan.html "How checking works" (afterwards pass): "a recursive *)
(* call's refined result serves as the induction hypothesis". A   *)
(* recursive function with a refined RESULT type may assume, at    *)
(* each recursive call, that the call's own result satisfies the   *)
(* result refinement -- exactly an induction hypothesis.           *)
(*                                                                *)
(* This mirrors vox1's demo/lean_fib.ml shape (refined result,     *)
(* recursive call as IH) but uses ANNOTATIONS ONLY: vox2 has no    *)
(* intro forms (refine_ / assume_ do not exist), so the spec lives *)
(* entirely in the parameter contracts and the result annotation.  *)
(*                                                                *)
(* Marker legend: see binder_facts.ml.                            *)
(* ============================================================= *)

(* @acc id=rec_fib_nonneg final=ACCEPT today=REJECT stable=no unlocks=integration+verification
   Fibonacci with a non-negativity result contract. Each recursive
   call [fib (n - 1)] / [fib (n - 2)] returns [int{ _ >= 0 }] (the IH);
   the sum of two non-negatives is non-negative, discharging the
   result obligation. The parameter contract [n >= 0] flows to the
   recursive-call arguments as contract obligations.
   FINAL: accepts. TODAY: rejected -- with [n] not yet skeleton-typed,
   [n <= 1] rigidly clashes the literal [1] against [n]'s refined type. *)
let rec rec_fib_nonneg (n : int{ _ >= 0 }) : int{ _ >= 0 } =
  if n <= 1 then n else rec_fib_nonneg (n - 1) + rec_fib_nonneg (n - 2)
[%%expect {|
Line 2, characters 17-18:
2 |   if n <= 1 then n else rec_fib_nonneg (n - 1) + rec_fib_nonneg (n - 2)
                     ^
Error: The value "n" has type "int" but an expression was expected of type
         "int{ (app[Stdlib!.>=] _ 0) }"
|}]

(* @acc id=rec_sum_to final=ACCEPT today=REJECT stable=no unlocks=integration+verification
   Triangular sum, a cleaner IH witness: the recursive call
   [rec_sum_to (n - 1)] carries a CONTRACT obligation [n - 1 >= 0]
   (provable from [n >= 0] and [n <> 0]) and an IH [result >= 0], and
   [n + <ih>] is then >= 0.
   FINAL: accepts. TODAY: rejected at the recursive-call argument /
   comparison against the refined [n]. *)
let rec rec_sum_to (n : int{ _ >= 0 }) : int{ _ >= 0 } =
  if n = 0 then 0 else n + rec_sum_to (n - 1)
[%%expect {|
Line 2, characters 16-17:
2 |   if n = 0 then 0 else n + rec_sum_to (n - 1)
                    ^
Error: The constant "0" has type "int" but an expression was expected of type
         "int{ (app[Stdlib!.>=] _ 0) }"
|}]

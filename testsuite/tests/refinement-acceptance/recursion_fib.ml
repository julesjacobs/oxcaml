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

(* @acc id=rec_fib_nonneg final=ACCEPT today=ACCEPT stable=no unlocks=integration+verification
   Fibonacci with a non-negativity result contract. Each recursive
   call [fib (n - 1)] / [fib (n - 2)] returns [int{ _ >= 0 }] (the IH);
   the sum of two non-negatives is non-negative, discharging the
   result obligation. The parameter contract [n >= 0] flows to the
   recursive-call arguments as contract obligations.
   FINAL and TODAY: accepts. *)
let rec rec_fib_nonneg (n : int{ _ >= 0 }) : int{ _ >= 0 } =
  if n <= 1 then n else rec_fib_nonneg (n - 1) + rec_fib_nonneg (n - 2)
[%%expect {|
val rec_fib_nonneg : int{ _ >= 0 } -> int{ _ >= 0 } = <fun>
|}]

(* @acc id=rec_sum_to final=ACCEPT today=ACCEPT stable=no unlocks=integration+verification
   Triangular sum, a cleaner IH witness: the recursive call
   [rec_sum_to (n - 1)] carries a CONTRACT obligation [n - 1 >= 0]
   (provable from [n >= 0] and [n <> 0]) and an IH [result >= 0], and
   [n + <ih>] is then >= 0.
   FINAL and TODAY: accepts. *)
let rec rec_sum_to (n : int{ _ >= 0 }) : int{ _ >= 0 } =
  if n = 0 then 0 else n + rec_sum_to (n - 1)
[%%expect {|
val rec_sum_to : int{ _ >= 0 } -> int{ _ >= 0 } = <fun>
|}]

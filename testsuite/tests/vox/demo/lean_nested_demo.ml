(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Demo: nested refined expressions with NO intervening let-binding.  Each
   stage's postcondition feeds directly into the next stage's precondition or
   dependent result, so a pipeline reads as [f (g x)] instead of the
   let-chain [let a = g x in let b = f a in ..] that vox used to require (the
   campaign's "C1" friction).  The verifier names each non-nameable
   subexpression by a synthetic loc-keyed ident and threads its own result
   refinement -- exactly what the let-binder contributed -- so every stage
   still proves.  All obligations are discharged by the solver. *)

[@@@warning "-26-27-32"]

(* [abs] returns a nonnegative value; [half] a value no larger than its input. *)
let abs (x : int) : int{ _ >= 0 } = if x >= 0 then refine_ x else refine_ (0 - x)
let half (n : int{ n >= 0 }) : int{ _ >= 0 && _ <= n } = refine_ (n / 2)

(* PRECONDITION threading, no let: [abs x >= 0] discharges [half]'s
   precondition directly at the nested call. *)
let pipeline (x : int) : int{ _ >= 0 } = half (abs x)

(* CHAIN, no let: [half (half (abs x))] -- each stage's nonneg postcondition
   satisfies the next stage's nonneg precondition, transitively. *)
let pipeline2 (x : int) : int{ _ >= 0 } = half (half (abs x))

(* DEPENDENT result over a nested call: [succ]'s result mentions its argument;
   opening substitutes the nested value's synthetic name, and [abs x >= 0]
   threads to prove the pipeline's own [_ >= 1] contract. *)
let succ (n : int) : int{ _ = n + 1 } = refine_ (n + 1)
let bump_abs (x : int) : int{ _ >= 1 } = succ (abs x)

(* A refined VARIABLE argument still works unchanged (nameable, no synthetic
   name); the feature only adds the non-nameable case. *)
let direct (n : int{ n >= 0 }) : int{ _ >= 0 } = half n

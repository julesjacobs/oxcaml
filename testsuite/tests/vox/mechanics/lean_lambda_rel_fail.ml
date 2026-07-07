(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* vox: LAMBDA-reflected relations must FAIL CLOSED.  The positive twin
   is demo/lean_lambda_rel.ml.  A relation supplied as a lambda is
   reflected to a Lean [fun .. => ..] and substituted at the binder; a
   producer that violates it, or a client goal the relation does not
   entail, is refuted by grind -- never a silent pass. *)

[@@@warning "-6"]

[%%vox.lean {lean|
def IntRel := Int -> Int -> Prop
@[grind] def rHolds (r : IntRel) (a b : Int) : Prop := r a b
|lean}]

let apply_step :
      (r : (int -> int -> bool)) ->
      (f : ((x : int) -> int{ rHolds r x _ })) ->
      (x : int) -> int{ rHolds r x _ } =
  fun r f x -> ignore r; f x
[%%expect{|
val apply_step :
  (r : int -> int -> bool) ->
  ((x : int) -> int{ rHolds r x _ }) -> (x : int) -> int{ rHolds r x _ } =
  <fun>
|}]

(* The producer DECREMENTS while the relation claims [<=], so its
   per-step obligation [a <= a - 1] is false -- rejected at the
   argument, not silently accepted. *)
let bad_producer (x : int) : int{ x <= _ } =
  apply_step (fun p q -> p <= q) (fun a -> a - 1) x
[%%expect{|
Line 2, characters 43-48:
2 |   apply_step (fun p q -> p <= q) (fun a -> a - 1) x
                                               ^^^^^
Error: vox: verification failed -- NOT PROVED (automation gave up; no counterexample was found, so the property may still hold).
       Goal: rHolds (fun p q -> p <= q) a (a - 1)
Hypotheses: <none>
(lean: error: `grind` failed)
|}]

(* The client goal [x <= result] does not follow from the STRICT-GREATER
   relation the lambda denotes -- refuted. *)
let bad_goal (x : int) : int{ x <= _ } =
  apply_step (fun p q -> p > q) (fun a -> a - 1) x
[%%expect{|
Line 2, characters 2-50:
2 |   apply_step (fun p q -> p > q) (fun a -> a - 1) x
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: vox: verification failed -- NOT PROVED (automation gave up; no counterexample was found, so the property may still hold).
       Goal: x <= *unknown4*
Hypotheses:
  rHolds (fun p q -> p > q) x *unknown4*
(lean: error: `grind` failed)
|}]

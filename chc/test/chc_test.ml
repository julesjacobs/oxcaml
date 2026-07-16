(* CHC solver self-tests: a graded suite of hand-written linear-LIA HORN problems, solved
   through the full parse -> PDR/BMC pipeline.

   Grading policy (soundness first):
   - [Safe]/[Unsafe] expected: reporting the OPPOSITE definite verdict is a SOUNDNESS
     failure (hard, exit 1). Reporting [unknown] is an incompleteness miss (soft).
   - [Must]: problems the v1 engine is expected to actually decide; an [unknown] here is a
     hard failure (guards against a regression that silently stops solving).
   - [Unknown] expected (out-of-fragment): must be [unknown]. *)

module Engine = Oxsmt_chc.Chc_engine
module Parse = Oxsmt_chc.Chc_parse

type expect =
  | Safe_must
  | Unsafe_must
  | Safe_ok (* safe, but unknown tolerated *)
  | Unsafe_ok
  | Unknown_expected

let solve src =
  match Parse.parse src with
  | sys -> Engine.solve ~budget:4000 ~max_frames:40 sys
  | exception Parse.Unsupported m -> { Engine.verdict = Engine.Unknown m; detail = m }
  | exception Parse.Malformed m ->
    { Engine.verdict = Engine.Unknown ("malformed: " ^ m); detail = m }
;;

let failures = ref 0
let soft = ref 0

let check name expect src =
  let r = solve src in
  let v = r.Engine.verdict in
  let smt = Engine.verdict_to_smtlib v in
  let ok, tag =
    match expect, v with
    | (Safe_must | Safe_ok), Engine.Safe -> true, "OK"
    | (Unsafe_must | Unsafe_ok), Engine.Unsafe -> true, "OK"
    | Unknown_expected, Engine.Unknown _ -> true, "OK"
    (* soundness violations: opposite definite verdict *)
    | (Safe_must | Safe_ok), Engine.Unsafe -> false, "UNSOUND(safe reported unsafe)"
    | (Unsafe_must | Unsafe_ok), Engine.Safe -> false, "UNSOUND(unsafe reported safe)"
    | Unknown_expected, (Engine.Safe | Engine.Unsafe) ->
      false, "UNEXPECTED-DEFINITE(should be unknown)"
    (* incompleteness misses *)
    | (Safe_must | Unsafe_must), Engine.Unknown _ -> false, "MISS(must-solve got unknown)"
    | (Safe_ok | Unsafe_ok), Engine.Unknown _ -> true, "soft-miss(unknown)"
  in
  if not ok then incr failures;
  (match expect, v with
   | (Safe_ok | Unsafe_ok), Engine.Unknown _ -> incr soft
   | _ -> ());
  Printf.printf "%-28s %-8s %s\n" name smt tag
;;

(* ---- graded problems ---- *)

let () =
  (* 1. simple increasing counter, safety x>=0 holds *)
  check
    "inc-nonneg"
    Safe_must
    {|(set-logic HORN)
      (declare-fun P (Int) Bool)
      (assert (forall ((x Int)) (=> (= x 0) (P x))))
      (assert (forall ((x Int)(y Int)) (=> (and (P x)(= y (+ x 1))) (P y))))
      (assert (forall ((x Int)) (=> (and (P x)(< x 0)) false)))|};
  (* 2. counter reaches 5, bad x>=5 reachable -> unsafe *)
  check
    "inc-reaches-5"
    Unsafe_must
    {|(set-logic HORN)
      (declare-fun P (Int) Bool)
      (assert (forall ((x Int)) (=> (= x 0) (P x))))
      (assert (forall ((x Int)(y Int)) (=> (and (P x)(= y (+ x 1))) (P y))))
      (assert (forall ((x Int)) (=> (and (P x)(>= x 5)) false)))|};
  (* 3. two-var RELATIONAL invariant x = y always, bad x<>y -> safe. Needs a relational
     lemma (x - y = 0); v1's per-variable interval generalization cannot express it, so
     [unknown] is the expected (soft) outcome — the headline lever for a next stage. *)
  check
    "two-var-eq"
    Safe_ok
    {|(set-logic HORN)
      (declare-fun P (Int Int) Bool)
      (assert (forall ((x Int)(y Int)) (=> (and (= x 0)(= y 0)) (P x y))))
      (assert (forall ((x Int)(y Int)(a Int)(b Int))
        (=> (and (P x y)(= a (+ x 1))(= b (+ y 1))) (P a b))))
      (assert (forall ((x Int)(y Int)) (=> (and (P x y)(not (= x y))) false)))|};
  (* 4. bounded counter 0..10 wrap, invariant 0<=x<=10, bad x>10 -> safe *)
  check
    "bounded-0-10"
    Safe_ok
    {|(set-logic HORN)
      (declare-fun P (Int) Bool)
      (assert (forall ((x Int)) (=> (= x 0) (P x))))
      (assert (forall ((x Int)(y Int)) (=> (and (P x)(< x 10)(= y (+ x 1))) (P y))))
      (assert (forall ((x Int)(y Int)) (=> (and (P x)(>= x 10)(= y x)) (P y))))
      (assert (forall ((x Int)) (=> (and (P x)(> x 10)) false)))|};
  (* 5. decreasing from 10, invariant x<=10, bad x>10 -> safe *)
  check
    "dec-le-10"
    Safe_must
    {|(set-logic HORN)
      (declare-fun P (Int) Bool)
      (assert (forall ((x Int)) (=> (= x 10) (P x))))
      (assert (forall ((x Int)(y Int)) (=> (and (P x)(= y (- x 1))) (P y))))
      (assert (forall ((x Int)) (=> (and (P x)(> x 10)) false)))|};
  (* 6. immediate violation: init already bad -> unsafe at depth 0 *)
  check
    "init-bad"
    Unsafe_must
    {|(set-logic HORN)
      (declare-fun P (Int) Bool)
      (assert (forall ((x Int)) (=> (= x 3) (P x))))
      (assert (forall ((x Int)) (=> (and (P x)(= x 3)) false)))|};
  (* 7. sum of two increments stays even difference... invariant x-y=0, safe (with let) *)
  check
    "let-binding-safe"
    Safe_ok
    {|(set-logic HORN)
      (declare-fun P (Int Int) Bool)
      (assert (forall ((x Int)(y Int)) (=> (and (= x 0)(= y 0)) (P x y))))
      (assert (forall ((x Int)(y Int)(a Int)(b Int))
        (=> (let ((d (- x y))) (and (P x y)(= d 0)(= a (+ x 2))(= b (+ y 2)))) (P a b))))
      (assert (forall ((x Int)(y Int)) (=> (and (P x y)(> (- x y) 0)) false)))|};
  (* 8. increasing by 2 from 0, x always even so x<>1 -> safe; bad x=1 *)
  check
    "step2-not-1"
    Safe_ok
    {|(set-logic HORN)
      (declare-fun P (Int) Bool)
      (assert (forall ((x Int)) (=> (= x 0) (P x))))
      (assert (forall ((x Int)(y Int)) (=> (and (P x)(= y (+ x 2))) (P y))))
      (assert (forall ((x Int)) (=> (and (P x)(= x 1)) false)))|};
  (* 9. unsafe: from 0, +1 each step, bad x=7 reachable *)
  check
    "reach-7"
    Unsafe_must
    {|(set-logic HORN)
      (declare-fun P (Int) Bool)
      (assert (forall ((x Int)) (=> (= x 0) (P x))))
      (assert (forall ((x Int)(y Int)) (=> (and (P x)(= y (+ x 1))) (P y))))
      (assert (forall ((x Int)) (=> (and (P x)(= x 7)) false)))|};
  (* 10. out of fragment: two predicates -> unknown in v1 *)
  check
    "two-preds-unknown"
    Unknown_expected
    {|(set-logic HORN)
      (declare-fun P (Int) Bool)
      (declare-fun Q (Int) Bool)
      (assert (forall ((x Int)) (=> (= x 0) (P x))))
      (assert (forall ((x Int)) (=> (P x) (Q x))))
      (assert (forall ((x Int)) (=> (and (Q x)(< x 0)) false)))|};
  (* 11. safe: x stays >=0 with a guard that can decrement but not below 0 *)
  check
    "guarded-nonneg"
    Safe_ok
    {|(set-logic HORN)
      (declare-fun P (Int) Bool)
      (assert (forall ((x Int)) (=> (= x 0) (P x))))
      (assert (forall ((x Int)(y Int)) (=> (and (P x)(> x 0)(= y (- x 1))) (P y))))
      (assert (forall ((x Int)(y Int)) (=> (and (P x)(= y (+ x 1))) (P y))))
      (assert (forall ((x Int)) (=> (and (P x)(< x 0)) false)))|};
  (* 12. unsafe two-var: x can exceed y bound *)
  check
    "two-var-unsafe"
    Unsafe_ok
    {|(set-logic HORN)
      (declare-fun P (Int Int) Bool)
      (assert (forall ((x Int)(y Int)) (=> (and (= x 0)(= y 5)) (P x y))))
      (assert (forall ((x Int)(y Int)(a Int)) (=> (and (P x y)(= a (+ x 1))) (P a y))))
      (assert (forall ((x Int)(y Int)) (=> (and (P x y)(> x y)) false)))|};
  Printf.printf "\n%d hard failure(s), %d soft miss(es)\n" !failures !soft;
  if !failures > 0 then exit 1
;;

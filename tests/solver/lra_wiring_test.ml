(* Focused end-to-end acceptance for exact real arithmetic through the shipped Session.
   The gate is set before its lazy accessor is forced. *)

let () = Unix.putenv "OXSMT_LRA" "1"

open Oxsmt_core
module Session = Oxsmt_interface.Session
module Model_check = Oxsmt_interface.Model_check
module Rational = Oxsmt_lia.Rational

let () = if not (Lra_config.enabled ()) then failwith "OXSMT_LRA gate did not enable"
let checks = ref 0
let failures = ref 0

let check name condition =
  incr checks;
  if not condition
  then (
    incr failures;
    Printf.printf "  FAIL %s\n" name)
;;

let verdict_string = function
  | Session.Sat -> "sat"
  | Session.Unsat -> "unsat"
  | Session.Unknown -> "unknown"
;;

let expect_verdict name expected actual =
  check (Printf.sprintf "%s (got %s)" name (verdict_string actual)) (expected = actual)
;;

let bigint = Bigint.of_string

let real_const ctx num den =
  Context.real_const_big ctx ~num:(bigint num) ~den:(bigint den)
;;

let rational num den = Rational.of_big_frac ~num:(bigint num) ~den:(bigint den)

let real_var session name =
  let ctx = Session.context session in
  Context.const ctx (Session.declare_const session name Sort.real)
;;

let find_real name (_, bindings) =
  List.find_map
    (function
      | Session.Const (name', Session.VReal value) when String.equal name name' ->
        Some value
      | Session.Const _ | Session.Fun _ -> None)
    bindings
;;

let test_strict_unsat () =
  let s = Session.create () in
  let ctx = Session.context s in
  let x = real_var s "strict-x" in
  let y = real_var s "strict-y" in
  Session.assert_term s (Context.lt ctx x y);
  Session.assert_term s (Context.lt ctx y x);
  expect_verdict "strict cycle x<y and y<x is unsat" Session.Unsat (Session.check_sat s)
;;

let test_open_interval_model () =
  let s = Session.create () in
  let ctx = Session.context s in
  let x = real_var s "open-x" in
  let zero = real_const ctx "0" "1" in
  let one = real_const ctx "1" "1" in
  let lower = Context.lt ctx zero x in
  let upper = Context.lt ctx x one in
  Session.assert_term s lower;
  Session.assert_term s upper;
  expect_verdict "open interval 0<x<1 is sat" Session.Sat (Session.check_sat s);
  match Session.get_model s with
  | None -> check "open interval has a checked model" false
  | Some model ->
    (match find_real "open-x" model with
     | None -> check "open interval model contains VReal open-x" false
     | Some value ->
       check
         "open interval model is strictly interior"
         (Rational.compare value Rational.zero > 0
          && Rational.compare value Rational.one < 0));
    check "open interval model passes R1" (Model_check.check model [ lower; upper ])
;;

let test_guarded_disequality_survives_pop () =
  let s = Session.create () in
  let ctx = Session.context s in
  let x = real_var s "pop-x" in
  let zero = real_const ctx "0" "1" in
  let equality = Context.eq ctx x zero in
  Session.push s;
  Session.assert_term s (Context.not_ ctx equality);
  expect_verdict
    "pushed Real disequality triggers a satisfiable trichotomy"
    Session.Sat
    (Session.check_sat s);
  Session.pop s;
  (* Theory split clauses are permanent. The sound clause is [eq; lt; gt], so after the
     premise is popped [eq] must still satisfy it. The unsound [lt; gt] mutant makes this
     post-pop query unsatisfiable. *)
  Session.assert_term s equality;
  expect_verdict
    "learned disequality split retains equality guard after pop"
    Session.Sat
    (Session.check_sat s)
;;

let test_uflra_model_agreement () =
  let s = Session.create () in
  let ctx = Session.context s in
  let f = Session.declare_fun s "real-f" (Rank.create [ Sort.real ] Sort.real) in
  let x = real_var s "uf-x" in
  let one = real_const ctx "1" "1" in
  let two = real_const ctx "2" "1" in
  let three = real_const ctx "3" "1" in
  let fx = Context.app ctx f [ x ] in
  let f_one = Context.app ctx f [ one ] in
  (* Each child alone is satisfiable: EUF may identify [two] and [three], while LRA may
     treat [f x] and [f 1] independently. Together x=1 makes congruence identify the two
     applications, and exact LRA fixes their results to distinct values. *)
  Session.assert_term s (Context.eq ctx x one);
  Session.assert_term s (Context.eq ctx fx two);
  Session.assert_term s (Context.eq ctx f_one three);
  expect_verdict
    "UFLRA agrees on Real function arguments and results"
    Session.Unsat
    (Session.check_sat s)
;;

let test_large_rational_model () =
  let s = Session.create () in
  let ctx = Session.context s in
  let x = real_var s "large-x" in
  let num = "92233720368547758081234567890123456789" in
  let den = "100000000000000000000000000000000000003" in
  let value = real_const ctx num den in
  let assertion = Context.eq ctx x value in
  Session.assert_term s assertion;
  expect_verdict "rational beyond int63 is sat" Session.Sat (Session.check_sat s);
  match Session.get_model s with
  | None -> check "large rational has a checked model" false
  | Some model ->
    (match find_real "large-x" model with
     | Some actual ->
       check "large rational model is exact" (Rational.equal actual (rational num den))
     | None -> check "large rational model contains VReal large-x" false);
    check "large rational model passes R1" (Model_check.check model [ assertion ])
;;

let replace_const name replacement (cards, bindings) =
  let found = ref false in
  let bindings =
    List.map
      (function
        | Session.Const (name', _) when String.equal name name' ->
          found := true;
          Session.Const (name', replacement)
        | binding -> binding)
      bindings
  in
  if not !found then failwith ("missing model binding: " ^ name);
  cards, bindings
;;

let test_r1_model_mutations () =
  let s = Session.create () in
  let ctx = Session.context s in
  let x = real_var s "mutation-x" in
  let zero = real_const ctx "0" "1" in
  let one = real_const ctx "1" "1" in
  let third = real_const ctx "1" "3" in
  let assertions =
    [ Context.lt ctx zero x; Context.lt ctx x one; Context.eq ctx x third ]
  in
  List.iter (Session.assert_term s) assertions;
  expect_verdict "mutation source query is sat" Session.Sat (Session.check_sat s);
  match Session.get_model s with
  | None -> check "mutation source has a checked model" false
  | Some model ->
    check "honest Real model passes R1" (Model_check.check model assertions);
    let boundary = replace_const "mutation-x" (Session.VReal Rational.zero) model in
    check
      "R1 rejects a boundary value for a strict constraint"
      (not (Model_check.check boundary assertions));
    let wrong_numerator =
      match find_real "mutation-x" model with
      | None -> failwith "missing VReal mutation-x"
      | Some value ->
        let changed =
          Rational.of_big_frac
            ~num:(Bigint.add (Rational.num_bigint value) Bigint.one)
            ~den:(Rational.den_bigint value)
        in
        replace_const "mutation-x" (Session.VReal changed) model
    in
    check
      "R1 rejects a wrong Real numerator"
      (not (Model_check.check wrong_numerator assertions));
    let mislabeled = replace_const "mutation-x" (Session.VUninterp 0) model in
    check
      "R1 rejects VUninterp for a Real term"
      (not (Model_check.check mislabeled assertions))
;;

(* F1 (LRA review bounce, SOUNDNESS): a stale [arithmetic_blocked] must not survive a
   registry-change reset. A datatype is live; asserting a Real atom is
   [real-with-datatype] ⇒ blocked + degraded and the atom is dropped;
   [set_datatypes empty] resets the query (clears [degraded]); then [(assert false)] MUST
   reach the solver ⇒ [unsat]. Before the fix the stale [arithmetic_blocked] dropped the
   [false] assertion with [degraded=false], so [check_sat] solved an empty query and
   returned a wrong [sat]. The 40-file five-logic spot cannot express this incremental
   Session sequence, so this synthetic case is the gate. *)
let test_incremental_registry_reset_soundness () =
  let s = Session.create () in
  let ctx = Session.context s in
  let nat = Sort.datatype_ (Session.declare_sort s "f1-nat") in
  let _dnat =
    Session.declare_datatype
      s
      nat
      [ { Session.ctor_name = "f1-succ"; fields = [ "f1-pred", nat ] }
      ; { Session.ctor_name = "f1-zero"; fields = [] }
      ]
  in
  (* Instantiate the DT theory (push DT eq, check, pop) so the later [set_datatypes empty]
     actually takes the registry-reset path — [set_datatypes] only invalidates when a
     theory is already instantiated. *)
  let a = Context.const ctx (Session.declare_const s "f1-a" nat) in
  let b = Context.const ctx (Session.declare_const s "f1-b" nat) in
  Session.push s;
  Session.assert_term s (Context.not_ ctx (Context.eq ctx a b));
  ignore (Session.check_sat s : Session.verdict);
  Session.pop s;
  let x = real_var s "f1-x" in
  let zero = real_const ctx "0" "1" in
  (* Real atom while a datatype is registered ⇒ blocked + degraded; the atom is dropped. *)
  Session.assert_term s (Context.lt ctx x zero);
  (* Registry-change reset (theory instantiated + registry non-empty ⇒ invalidate fires):
     clears [degraded] (and, with the fix, [arithmetic_blocked]). *)
  Session.set_datatypes s Datatype_defs.empty;
  (* [(assert false)] must NOT be silently dropped by a stale blocked flag. *)
  Session.assert_term s (Context.bool_const ctx false);
  expect_verdict
    "F1: (assert false) after a registry reset is unsat (no stale-blocked wrong-sat)"
    Session.Unsat
    (Session.check_sat s)
;;

let () =
  print_endline "LRA Session wiring self-test:";
  test_strict_unsat ();
  test_open_interval_model ();
  test_guarded_disequality_survives_pop ();
  test_uflra_model_agreement ();
  test_large_rational_model ();
  test_r1_model_mutations ();
  test_incremental_registry_reset_soundness ();
  Printf.printf
    "\nLRA Session wiring self-test: %d checks, %d failure(s)\n"
    !checks
    !failures;
  if !failures > 0 then exit 1
;;

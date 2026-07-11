open Oxsmt_core
open Oxsmt_eval

(* N-version model-evaluator self-test (task #74). Deterministic, stdlib-only, nonzero
   exit on any failed check. Coverage:
   - one satisfying + one falsifying model per Term node kind (reader -> model -> eval);
   - the gate's real sat cases + .model sidecars all MODEL-SATISFIES (auto-discovered);
   - a deliberately-corrupted model MODEL-FAILS;
   - euclidean div/mod sign matrix (4 combos, hand-computed);
   - integer overflow raises (never wraps). *)

let failures = ref 0
let checks = ref 0

let report name ok detail =
  incr checks;
  if not ok
  then (
    incr failures;
    Printf.printf "FAIL  %s: %s\n" name detail)
;;

(* --- reader -> model -> eval helpers --------------------------------------------- *)

let outcome_of smt2 model_src =
  let q = Reader.read_string smt2 in
  let m = Eval_model.of_string q.Reader.decls model_src in
  Eval.check m q.Reader.assertions
;;

let expect_satisfies name smt2 model_src =
  match outcome_of smt2 model_src with
  | Eval.Satisfies -> report name true ""
  | Eval.Fails { index; _ } ->
    report name false (Printf.sprintf "expected SATISFIES, got FAILS %d" index)
  | exception e -> report name false ("raised " ^ Printexc.to_string e)
;;

let expect_fails ?index name smt2 model_src =
  match outcome_of smt2 model_src with
  | Eval.Fails { index = i; _ } ->
    (match index with
     | Some want ->
       report name (i = want) (Printf.sprintf "failed at %d, wanted %d" i want)
     | None -> report name true "")
  | Eval.Satisfies -> report name false "expected FAILS, got SATISFIES"
  | exception e -> report name false ("raised " ^ Printexc.to_string e)
;;

let expect_raises name f =
  match f () with
  | _ -> report name false "expected an exception, none raised"
  | exception _ -> report name true ""
;;

(* --- per-node satisfying + falsifying pairs -------------------------------------- *)

let node_cases () =
  (* Bool_const / Not *)
  expect_satisfies "bool_const/sat" "(assert true)" "(model)";
  expect_fails "bool_const/fail" "(assert false)" "(model)";
  expect_satisfies
    "not/sat"
    "(declare-const p Bool)(assert (not p))"
    "(model (const p false))";
  expect_fails
    "not/fail"
    "(declare-const p Bool)(assert (not p))"
    "(model (const p true))";
  (* Int_const / Eq / Le *)
  expect_satisfies
    "eq-int/sat"
    "(declare-const x Int)(assert (= x 3))"
    "(model (const x 3))";
  expect_fails "eq-int/fail" "(declare-const x Int)(assert (= x 3))" "(model (const x 4))";
  expect_satisfies
    "le/sat"
    "(declare-const x Int)(assert (<= x 0))"
    "(model (const x -1))";
  expect_fails "le/fail" "(declare-const x Int)(assert (<= x 0))" "(model (const x 1))";
  (* Arith *)
  expect_satisfies
    "arith/sat"
    "(declare-const x Int)(declare-const y Int)(assert (= (+ x y) 5))"
    "(model (const x 2) (const y 3))";
  expect_fails
    "arith/fail"
    "(declare-const x Int)(declare-const y Int)(assert (= (+ x y) 5))"
    "(model (const x 2) (const y 2))";
  (* mul_const linear form *)
  expect_satisfies
    "mul_const/sat"
    "(declare-const x Int)(assert (= (* 2 x) 4))"
    "(model (const x 2))";
  expect_fails
    "mul_const/fail"
    "(declare-const x Int)(assert (= (* 2 x) 4))"
    "(model (const x 3))";
  (* And *)
  expect_satisfies
    "and/sat"
    "(declare-const x Int)(assert (and (<= x 0) (>= x 0)))"
    "(model (const x 0))";
  expect_fails
    "and/fail"
    "(declare-const x Int)(assert (and (<= x 0) (>= x 0)))"
    "(model (const x 1))";
  (* Or *)
  expect_satisfies
    "or/sat"
    "(declare-const x Int)(assert (or (<= x 0) (>= x 10)))"
    "(model (const x -1))";
  expect_fails
    "or/fail"
    "(declare-const x Int)(assert (or (<= x 0) (>= x 10)))"
    "(model (const x 5))";
  (* Ite (Int-sorted branches, evaluated directly, no preprocessing) *)
  expect_satisfies
    "ite/sat"
    "(declare-const x Int)(assert (= (ite (>= x 0) 1 0) 1))"
    "(model (const x 5))";
  expect_fails
    "ite/fail"
    "(declare-const x Int)(assert (= (ite (>= x 0) 1 0) 1))"
    "(model (const x -1))";
  (* App: uninterpreted function + congruence-shaped model *)
  let euf_decls = "(declare-sort S 0)(declare-fun f (S) S)(declare-const a S)" in
  expect_satisfies
    "app-fun/sat"
    (euf_decls ^ "(assert (= (f a) a))")
    "(model (sort S 1) (const a 0) (fun f (default 0) (case (0) 0)))";
  expect_fails
    "app-fun/fail"
    (euf_decls ^ "(assert (= (f a) a))")
    "(model (sort S 2) (const a 0) (fun f (default 1) (case (0) 1)))";
  (* App nullary of an uninterpreted sort + distinct/Eq over Uninterp values *)
  let uf2 = "(declare-sort S 0)(declare-const a S)(declare-const b S)" in
  expect_satisfies
    "distinct-uninterp/sat"
    (uf2 ^ "(assert (distinct a b))")
    "(model (sort S 2) (const a 0) (const b 1))";
  expect_fails
    "distinct-uninterp/fail"
    (uf2 ^ "(assert (distinct a b))")
    "(model (sort S 2) (const a 0) (const b 0))";
  (* Bool-Eq is iff *)
  expect_satisfies
    "iff/sat"
    "(declare-const p Bool)(declare-const q Bool)(assert (= p q))"
    "(model (const p true) (const q true))";
  expect_fails
    "iff/fail"
    "(declare-const p Bool)(declare-const q Bool)(assert (= p q))"
    "(model (const p true) (const q false))";
  (* let binding *)
  expect_satisfies
    "let/sat"
    "(declare-const x Int)(assert (let ((t (+ x 1))) (= t 2)))"
    "(model (const x 1))";
  expect_fails
    "let/fail"
    "(declare-const x Int)(assert (let ((t (+ x 1))) (= t 2)))"
    "(model (const x 5))";
  (* div/mod through the reader (euclidean) *)
  expect_satisfies
    "div-reader/sat"
    "(declare-const x Int)(assert (= (div x 3) 2))"
    "(model (const x 7))";
  expect_fails
    "div-reader/fail"
    "(declare-const x Int)(assert (= (div x 3) 2))"
    "(model (const x 10))";
  expect_satisfies
    "mod-reader/sat"
    "(declare-const x Int)(assert (= (mod x 3) 1))"
    "(model (const x 7))"
;;

(* --- euclidean div/mod sign matrix (hand-computed) -------------------------------- *)

let div_mod_matrix () =
  let env = Env.create () in
  let ctx = Context.create env in
  let no_model _ = None in
  let evi t =
    match Eval.eval_term ~env:no_model t with
    | Value.Int n -> n
    | _ -> failwith "expected Int"
  in
  let combos = [ 7, 3, 2, 1; -7, 3, -3, 2; 7, -3, -2, 1; -7, -3, 3, 2 ] in
  List.iter
    (fun (x, d, q, r) ->
       let xt = Context.int_const ctx x
       and dt = Context.int_const ctx d in
       let gotq = evi (Context.div ctx xt dt) in
       let gotr = evi (Context.mod_ ctx xt dt) in
       report
         (Printf.sprintf "euclid div %d/%d" x d)
         (gotq = q)
         (Printf.sprintf "got q=%d, want %d" gotq q);
       report
         (Printf.sprintf "euclid mod %d/%d" x d)
         (gotr = r)
         (Printf.sprintf "got r=%d, want %d" gotr r);
       (* sanity: x = d*q + r with 0 <= r < |d| *)
       report
         (Printf.sprintf "euclid identity %d/%d" x d)
         ((d * gotq) + gotr = x && gotr >= 0 && gotr < abs d)
         "identity/range violated")
    combos
;;

(* --- min_int boundary: div wraps must abstain, representable mod must not (codex E1) - *)

let div_mod_boundary () =
  let env = Env.create () in
  let ctx = Context.create env in
  let no_model _ = None in
  let evi t =
    match Eval.eval_term ~env:no_model t with
    | Value.Int n -> n
    | _ -> failwith "expected Int"
  in
  let dt d = Context.int_const ctx d in
  let xmin = Context.int_const ctx min_int in
  (* E1: min_int / -1 is non-representable — must RAISE (abstain), never wrap to min_int. *)
  expect_raises "min_int div -1 raises" (fun () -> evi (Context.div ctx xmin (dt (-1))));
  (* the exact codex E1 trigger, through the full reader -> model -> eval pipeline: with
     the old wrap this MODEL-SATISFIES (wrong); now it must raise/abstain. *)
  expect_raises "E1 trigger abstains" (fun () ->
    outcome_of
      "(declare-const x Int)(assert (= (div x (- 1)) x))"
      (Printf.sprintf "(model (const x %d))" min_int));
  (* mod min_int -1 IS representable (0) and must NOT raise (the MEDIUM finding: mod must
     not compute the quotient). *)
  report "min_int mod -1 = 0" (evi (Context.mod_ ctx xmin (dt (-1))) = 0) "expected 0";
  (* div/mod min_int by +1: exact, quotient representable. *)
  report "min_int div 1 = min_int" (evi (Context.div ctx xmin (dt 1)) = min_int) "q";
  report "min_int mod 1 = 0" (evi (Context.mod_ ctx xmin (dt 1)) = 0) "r";
  (* div/mod min_int by 2: exact (min_int even). *)
  report "min_int div 2" (evi (Context.div ctx xmin (dt 2)) = min_int / 2) "q";
  report "min_int mod 2 = 0" (evi (Context.mod_ ctx xmin (dt 2)) = 0) "r";
  (* div/mod min_int by 3: the representable-remainder case (MEDIUM). Hand-computed: q =
     -1537228672809129302, r = 2 (identity 3*q+r overflows native int, so it is NOT
     asserted here — that intermediate is exactly why the old x-r formula was wrong). *)
  report "min_int div 3" (evi (Context.div ctx xmin (dt 3)) = -1537228672809129302) "q";
  report "min_int mod 3 = 2" (evi (Context.mod_ ctx xmin (dt 3)) = 2) "r";
  (* divisor min_int abstains (abs min_int unrepresentable). *)
  expect_raises "div by min_int abstains" (fun () -> evi (Context.div ctx (dt 5) xmin));
  expect_raises "mod by min_int abstains" (fun () -> evi (Context.mod_ ctx (dt 5) xmin))
;;

(* --- overflow raises (never wraps) ------------------------------------------------ *)

let overflow_cases () =
  (* x := max_int; the sum/product below leaves native range at EVAL time (the constant
     folds at construction do not, so the overflow is genuinely the evaluator's). *)
  let model = Printf.sprintf "(model (const x %d))" max_int in
  expect_raises "overflow-add" (fun () ->
    outcome_of "(declare-const x Int)(assert (= (+ x 1) 0))" model);
  expect_raises "overflow-mul" (fun () ->
    outcome_of "(declare-const x Int)(assert (= (* 2 x) 0))" model)
;;

(* --- reject-don't-guess (reader / model errors) ----------------------------------- *)

let rejection_cases () =
  expect_raises "reject-unsupported-logic" (fun () ->
    Reader.read_string "(set-logic QF_BV)");
  expect_raises "reject-quantifier" (fun () ->
    Reader.read_string "(declare-const x Int)(assert (forall ((y Int)) (>= y x)))");
  expect_raises "reject-nonlinear" (fun () ->
    Reader.read_string "(declare-const x Int)(assert (= (* x x) 4))");
  expect_raises "reject-undeclared" (fun () -> Reader.read_string "(assert (= zzz 3))");
  expect_raises "reject-bad-model-value" (fun () ->
    outcome_of "(declare-const p Bool)(assert p)" "(model (const p 3))")
;;

(* --- the gate's real sat cases: every .model sidecar must MODEL-SATISFIES ---------- *)

let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> really_input_string ic (in_channel_length ic))
;;

let gate_cases dir =
  if Sys.file_exists dir && Sys.is_directory dir
  then (
    let entries = Sys.readdir dir in
    Array.sort compare entries;
    Array.iter
      (fun name ->
         if Filename.check_suffix name ".model"
         then (
           let base = Filename.chop_suffix name ".model" in
           let smt2 = Filename.concat dir (base ^ ".smt2") in
           let model = Filename.concat dir name in
           if Sys.file_exists smt2
           then (
             let name = "gate-case " ^ base in
             match outcome_of (read_file smt2) (read_file model) with
             | Eval.Satisfies -> report name true ""
             | Eval.Fails { index; _ } ->
               report name false (Printf.sprintf "MODEL-FAILS %d" index)
             | exception e -> report name false ("raised " ^ Printexc.to_string e))))
      entries)
  else Printf.printf "note: gate cases dir %s absent; skipping\n" dir
;;

let () =
  let dir = if Array.length Sys.argv > 1 then Sys.argv.(1) else "tests/cases" in
  node_cases ();
  div_mod_matrix ();
  div_mod_boundary ();
  overflow_cases ();
  rejection_cases ();
  gate_cases dir;
  (* explicit deliberately-corrupted models: a wrong value must MODEL-FAIL *)
  expect_fails
    "corrupt-lia"
    "(declare-const x Int)(assert (>= x 0))(assert (<= x 5))(assert (= x 3))"
    "(model (const x 5))" (* passes both bounds, falsifies (= x 3) at index 2 *)
    ~index:2;
  (* corrupted euf model: f maps a and b to DIFFERENT elements, breaking (= (f a) (f b)) *)
  expect_fails
    "corrupt-euf"
    "(declare-sort S 0)(declare-fun f (S) S)(declare-const a S)(declare-const b S)\n\
     (assert (distinct a b))(assert (= (f a) (f b)))"
    "(model (sort S 2) (const a 0) (const b 1)\n\
    \  (fun f (default 0) (case (0) 0) (case (1) 1)))"
    ~index:1;
  Printf.printf "\n%d checks, %d failures\n" !checks !failures;
  if !failures > 0 then exit 1
;;

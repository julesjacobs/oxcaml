(* Unit + discrimination tests for the online symmetry-breaking presolve
   ({!Oxsmt_interface.Presolve.symmetry_break}, task #25). Run via `make symbreak-test`
   with OXSMT_SYMBREAK=1 in the environment (the product path is env-gated dark).

   Coverage (all RED-verified — see the block comments):
   1. DETECTOR FIRES — on a symmetric quasigroup the pass returns a non-empty set of
      breaking constraints (the interchangeable element class is found).
   2. DETECTOR REJECTS — on a benchmark whose symmetry is genuinely broken (only a cyclic,
      NOT a transposition, symmetry) the pass returns [] (no class): the exact
      transposition check is not fooled. RED if detection ignored the exact check.
   3. SAT-PRESERVATION + MUTANT DISCRIMINATION — the shipped (sound lex-leader) path keeps
      a satisfiable symmetric instance SAT; a TEST-ONLY value-precedence mutant makes the
      SAME instance UNSAT (a real SAT->UNSAT flip). The mutant proves the SAT-preservation
      test would go RED if the product ever regressed to value precedence.
   4. UNSAT GOLDEN — the sound path keeps an unsatisfiable symmetric instance UNSAT
      (adding breaking constraints preserves UNSAT; guards against a crash/degrade). *)

open Oxsmt_core
module Session = Oxsmt_interface.Session
module Presolve = Oxsmt_interface.Presolve
module Parser = Oxsmt_smtlib_parser.Parser

let checks = ref 0
let failures = ref 0

let fail fmt =
  Printf.ksprintf
    (fun s ->
       incr failures;
       print_string ("  FAIL " ^ s ^ "\n"))
    fmt
;;

let ok name =
  incr checks;
  Printf.printf "  ok %s\n" name
;;

(* --- goldens (inline; minimal so the unit test is fast) ------------------------------- *)

(* A satisfiable order-3 quasigroup (Latin square) with anti-idempotency (op(ei,ei) <>
   ei). e0,e1,e2 are fully interchangeable; SAT (an idempotent-free order-3 Latin square
   exists). Value precedence is UNSOUND here (it forces the first cell to the first value,
   which collides with anti-idempotency across the orbit) — z3 confirms sat->unsat under
   VP. *)
let sat_symmetric =
  {|
(set-logic QF_UF)
(declare-sort I 0)
(declare-fun op (I I) I)
(declare-fun e0 () I)
(declare-fun e1 () I)
(declare-fun e2 () I)
(assert (distinct e0 e1 e2))
(assert (or (= (op e0 e0) e0) (= (op e0 e0) e1) (= (op e0 e0) e2)))
(assert (or (= (op e0 e1) e0) (= (op e0 e1) e1) (= (op e0 e1) e2)))
(assert (or (= (op e0 e2) e0) (= (op e0 e2) e1) (= (op e0 e2) e2)))
(assert (or (= (op e1 e0) e0) (= (op e1 e0) e1) (= (op e1 e0) e2)))
(assert (or (= (op e1 e1) e0) (= (op e1 e1) e1) (= (op e1 e1) e2)))
(assert (or (= (op e1 e2) e0) (= (op e1 e2) e1) (= (op e1 e2) e2)))
(assert (or (= (op e2 e0) e0) (= (op e2 e0) e1) (= (op e2 e0) e2)))
(assert (or (= (op e2 e1) e0) (= (op e2 e1) e1) (= (op e2 e1) e2)))
(assert (or (= (op e2 e2) e0) (= (op e2 e2) e1) (= (op e2 e2) e2)))
(assert (distinct (op e0 e0) (op e0 e1) (op e0 e2)))
(assert (distinct (op e1 e0) (op e1 e1) (op e1 e2)))
(assert (distinct (op e2 e0) (op e2 e1) (op e2 e2)))
(assert (distinct (op e0 e0) (op e1 e0) (op e2 e0)))
(assert (distinct (op e0 e1) (op e1 e1) (op e2 e1)))
(assert (distinct (op e0 e2) (op e1 e2) (op e2 e2)))
(assert (not (= (op e0 e0) e0)))
(assert (not (= (op e1 e1) e1)))
(assert (not (= (op e2 e2) e2)))
(check-sat)
|}
;;

(* An order-2 anti-idempotent Latin square: UNSAT (both 2x2 Latin squares have a diagonal
   fixed point, which anti-idempotency forbids). e0,e1 are interchangeable, so the
   detector fires; adding the sound break must keep it UNSAT. *)
let unsat_symmetric =
  {|
(set-logic QF_UF)
(declare-sort I 0)
(declare-fun op (I I) I)
(declare-fun e0 () I)
(declare-fun e1 () I)
(assert (distinct e0 e1))
(assert (or (= (op e0 e0) e0) (= (op e0 e0) e1)))
(assert (or (= (op e0 e1) e0) (= (op e0 e1) e1)))
(assert (or (= (op e1 e0) e0) (= (op e1 e0) e1)))
(assert (or (= (op e1 e1) e0) (= (op e1 e1) e1)))
(assert (distinct (op e0 e0) (op e0 e1)))
(assert (distinct (op e1 e0) (op e1 e1)))
(assert (distinct (op e0 e0) (op e1 e0)))
(assert (distinct (op e0 e1) (op e1 e1)))
(assert (not (= (op e0 e0) e0)))
(assert (not (= (op e1 e1) e1)))
(check-sat)
|}
;;

(* Asymmetric: a 3-cycle f(e0)=e1, f(e1)=e2, f(e2)=e0. Rotationally symmetric but NO
   transposition maps the assertion set to itself, so the detector must find no class. *)
let asymmetric =
  {|
(set-logic QF_UF)
(declare-sort I 0)
(declare-fun f (I) I)
(declare-fun e0 () I)
(declare-fun e1 () I)
(declare-fun e2 () I)
(assert (distinct e0 e1 e2))
(assert (= (f e0) e1))
(assert (= (f e1) e2))
(assert (= (f e2) e0))
(check-sat)
|}
;;

(* --- helpers -------------------------------------------------------------------------- *)

(* Parse into a fresh cap-bearing env/ctx (NOT a session) so we can call [symmetry_break]
   directly and inspect the returned constraints. *)
let detect src =
  let env, cap = Env.create_with_cap () in
  let ctx = Context.create env in
  let parsed = Parser.parse_into env ctx src in
  Presolve.symmetry_break cap env ctx parsed.Parser.assertions
;;

(* Solve a source through the shipped loader path (same as oxsmt_cli). With
   OXSMT_SYMBREAK=1 in the environment this exercises the product symmetry-breaking. *)
let solve ?(presolve = true) src =
  let s = Session.create () in
  match Parser.parse_into (Session.env s) (Session.context s) src with
  | exception (Parser.Malformed _ | Parser.Unsupported _) -> Session.Unknown, s
  | parsed ->
    if Oxsmt_query_loader.assert_all ~presolve s parsed
    then Session.check_sat s, s
    else Session.Unknown, s
;;

(* --- test-only VALUE-PRECEDENCE mutant (the unsound break; NOT in the product) --------
   Harvest the single uninterpreted sort's constants and application cells and emit
   Law-Lee value precedence over the op-cell sequence: (not (= cell_0 c_j)) for j >= 1 (=
   cell_p c_j) => OR_[{q<p}] (= cell_q c_[{j-1}]) This is the transform proven unsound for
   index+value symmetry (SAT->UNSAT flips). *)
let value_precedence_mutant ctx (assertions : Term.t list) =
  let consts = ref Term.Set.empty in
  let cells = ref Term.Set.empty in
  let seen = Term.Table.create 256 in
  let rec walk (t : Term.t) =
    if not (Term.Table.mem seen t)
    then (
      Term.Table.add seen t ();
      match t.Term.node with
      | App (_, args) when Iarr.length args = 0 ->
        if not (Sort.equal t.Term.sort Sort.bool) then consts := Term.Set.add t !consts
      | App (_, args) ->
        if not (Sort.equal t.Term.sort Sort.bool) then cells := Term.Set.add t !cells;
        Iarr.iter walk args
      | Le a | Not a -> walk a
      | Eq (a, b) ->
        walk a;
        walk b
      | And xs | Or xs -> Iarr.iter walk xs
      | Ite (c, a, b) ->
        walk c;
        walk a;
        walk b
      | Arith lin -> Iarr.iter (fun (tm, _) -> walk tm) lin.Term.coeffs
      | Bool_const _ | Int_const _ -> ())
  in
  List.iter walk assertions;
  let cs = Array.of_list (Term.Set.elements !consts) in
  let cells = Term.Set.elements !cells in
  let out = ref [] in
  let k = Array.length cs in
  let seq = Array.of_list cells in
  let m = Array.length seq in
  for j = 1 to k - 1 do
    for p = 0 to m - 1 do
      let lhs = Context.eq ctx seq.(p) cs.(j) in
      if p = 0
      then out := Context.not_ ctx lhs :: !out
      else (
        let ors = List.init p (fun q -> Context.eq ctx seq.(q) cs.(j - 1)) in
        out := Context.or_ ctx (Context.not_ ctx lhs :: ors) :: !out)
    done
  done;
  !out
;;

(* --- tests ---------------------------------------------------------------------------- *)

let test_detector_fires () =
  let cs = detect sat_symmetric in
  if List.length cs > 0
  then ok "detector fires on symmetric quasigroup"
  else
    fail "detector emitted NO constraints on a symmetric instance (should find a class)"
;;

let test_detector_rejects () =
  let cs = detect asymmetric in
  if List.length cs = 0
  then ok "detector rejects broken (cyclic-only) symmetry"
  else
    fail
      "detector emitted %d constraints on an instance with no transposition symmetry"
      (List.length cs)
;;

let test_sat_preserved () =
  match fst (solve ~presolve:true sat_symmetric) with
  | Session.Sat -> ok "sound break preserves SAT (product path)"
  | Session.Unsat ->
    fail
      "sound break made a SAT instance UNSAT — SAT-preservation VIOLATED (unsound break!)"
  | Session.Unknown -> fail "sound break: got unknown on a small SAT instance"
;;

(* Discrimination: the test-only value-precedence mutant flips the SAME SAT instance to
   UNSAT. Proves [test_sat_preserved] is RED if the product regressed to value precedence. *)
let test_vp_mutant_flips () =
  let s = Session.create () in
  let parsed = Parser.parse_into (Session.env s) (Session.context s) sat_symmetric in
  (* assert the ORIGINAL raw (no product breaking) ... *)
  if not (Oxsmt_query_loader.assert_all ~presolve:false s parsed)
  then fail "vp-mutant: loader rejected the base assertions"
  else (
    (* ... then add the UNSOUND value-precedence constraints and solve. *)
    let vp = value_precedence_mutant (Session.context s) parsed.Parser.assertions in
    List.iter (Session.assert_term s) vp;
    match Session.check_sat s with
    | Session.Unsat ->
      ok "value-precedence MUTANT flips SAT->UNSAT (discrimination witness)"
    | Session.Sat ->
      fail
        "value-precedence mutant did NOT flip this instance — the discrimination golden \
         no longer discriminates; strengthen it"
    | Session.Unknown -> fail "vp-mutant: got unknown")
;;

let test_unsat_preserved () =
  match fst (solve ~presolve:true unsat_symmetric) with
  | Session.Unsat -> ok "sound break preserves UNSAT"
  | Session.Sat -> fail "unsat instance reported SAT (unsound!)"
  | Session.Unknown -> fail "unsat golden: got unknown"
;;

let () =
  print_string "symbreak_test:\n";
  test_detector_fires ();
  test_detector_rejects ();
  test_sat_preserved ();
  test_vp_mutant_flips ();
  test_unsat_preserved ();
  Printf.printf "symbreak_test: %d checks, %d failures\n" !checks !failures;
  if !failures > 0 then exit 1
;;

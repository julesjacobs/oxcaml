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
  Presolve.symmetry_break ~counter:(ref 0) cap env ctx parsed.Parser.assertions
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

(* Two interchangeable-constant sorts in one problem — exercises the Sort.equal grouping
   (F3: a Sort.hash-only bucket could pair cross-sort constants and crash in Context.eq). *)
let two_sort_symmetric =
  {|
(set-logic QF_UF)
(declare-sort I 0)
(declare-sort J 0)
(declare-fun op (I I) I)
(declare-fun g (J J) J)
(declare-fun a0 () I)
(declare-fun a1 () I)
(declare-fun b0 () J)
(declare-fun b1 () J)
(assert (distinct a0 a1))
(assert (distinct b0 b1))
(assert (or (= (op a0 a0) a0) (= (op a0 a0) a1)))
(assert (or (= (op a1 a1) a0) (= (op a1 a1) a1)))
(assert (or (= (g b0 b0) b0) (= (g b0 b0) b1)))
(assert (or (= (g b1 b1) b0) (= (g b1 b1) b1)))
(check-sat)
|}
;;

(* Collect the reserved [.oxsmt.sym.*] aux-var names referenced by a term list. *)
let sym_aux_names terms =
  let names = ref [] in
  let seen = Term.Table.create 64 in
  let rec walk (t : Term.t) =
    if not (Term.Table.mem seen t)
    then (
      Term.Table.add seen t ();
      match t.Term.node with
      | App (sym, args) ->
        let n = Symbol.name sym in
        if String.length n > 10 && String.sub n 0 10 = ".oxsmt.sym"
        then names := n :: !names;
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
  List.iter walk terms;
  !names
;;

(* F1: symmetry breaking is NON-MONOTONIC — an assertion AFTER the emission can break the
   detected symmetry, and the permanent lex clauses would then wrongly refute a SAT model
   (SAT->UNSAT). The fix retracts the lex clauses on any post-emission assertion. This
   exact sequence is RED (SAT->UNSAT) with the retraction disabled; [(= (op e0 e1) e0)] is
   genuinely SAT with the base problem (z3-confirmed) yet the stale lex clauses refute it. *)
let test_f1_incremental () =
  let s = Session.create () in
  let env = Session.env s
  and ctx = Session.context s in
  let parsed = Parser.parse_into env ctx sat_symmetric in
  (* name -> Symbol.t, by walking the parsed assertions *)
  let symtab = Hashtbl.create 16 in
  let rec w (t : Term.t) =
    match t.Term.node with
    | App (sym, args) ->
      Hashtbl.replace symtab (Symbol.name sym) sym;
      Iarr.iter w args
    | Le a | Not a -> w a
    | Eq (a, b) ->
      w a;
      w b
    | And xs | Or xs -> Iarr.iter w xs
    | Ite (c, a, b) ->
      w c;
      w a;
      w b
    | Arith lin -> Iarr.iter (fun (tm, _) -> w tm) lin.Term.coeffs
    | Bool_const _ | Int_const _ -> ()
  in
  List.iter w parsed.Parser.assertions;
  let c n = Context.app ctx (Hashtbl.find symtab n) [] in
  let cell a b = Context.app ctx (Hashtbl.find symtab "op") [ c a; c b ] in
  if not (Oxsmt_query_loader.assert_all ~presolve:true s parsed)
  then fail "F1: loader rejected base"
  else (
    match Session.check_sat s with
    | Session.Sat ->
      (* A single asymmetric, base-satisfiable pin that breaks the value symmetry. *)
      Session.assert_term s (Context.eq ctx (cell "e0" "e1") (c "e0"));
      (match Session.check_sat s with
       | Session.Sat -> ok "F1: incremental assertion keeps SAT (lex clauses retracted)"
       | Session.Unsat ->
         fail "F1 WRONG-UNSAT: stale lex clauses survived an incremental assertion"
       | Session.Unknown -> fail "F1: unknown after incremental assert")
    | _ -> fail "F1: base problem not SAT")
;;

(* F2: the aux-var name counter must persist across [symmetry_break] calls on one env — a
   per-call reset reuses [.oxsmt.sym.0] with a conflicting definition. Two calls sharing a
   counter must emit DISJOINT aux-var names. RED (overlap) with a per-call counter. *)
let test_f2_counter () =
  let env, cap = Env.create_with_cap () in
  let ctx = Context.create env in
  let parsed = Parser.parse_into env ctx sat_symmetric in
  let counter = ref 0 in
  let n1 =
    sym_aux_names (Presolve.symmetry_break ~counter cap env ctx parsed.Parser.assertions)
  in
  let n2 =
    sym_aux_names (Presolve.symmetry_break ~counter cap env ctx parsed.Parser.assertions)
  in
  let overlap = List.filter (fun n -> List.mem n n2) n1 in
  if n1 <> [] && n2 <> [] && overlap = []
  then ok "F2: aux-var names disjoint across calls (persistent counter)"
  else
    fail
      "F2: aux-var name reuse across calls (n1=%d n2=%d overlap=%d)"
      (List.length n1)
      (List.length n2)
      (List.length overlap)
;;

(* F3: a multi-sort input must not pair constants across sorts (a Sort.hash-only grouping
   could, driving Context.eq to Term.Sort_error). Detector runs cleanly and the verdict is
   correct. *)
let test_f3_multisort () =
  match detect two_sort_symmetric with
  | exception e ->
    fail "F3: detector raised %s on a two-sort input" (Printexc.to_string e)
  | _cs ->
    (match fst (solve ~presolve:true two_sort_symmetric) with
     | Session.Sat -> ok "F3: two-sort input handled, verdict SAT (no cross-sort crash)"
     | Session.Unsat -> fail "F3: two-sort input wrongly UNSAT"
     | Session.Unknown -> fail "F3: two-sort input unknown")
;;

(* R2/B1: symmetry breaking must NOT emit under a pushed frame — the lex clauses are
   guarded by the activation selector, not the frame selector, so they would survive the
   [pop] that retracts the assertions making the batch symmetric. The restriction skips
   emission when any frame is open. RED (emits) with the restriction disabled. *)
let test_b1_no_emit_under_frame () =
  let s = Session.create () in
  let parsed = Parser.parse_into (Session.env s) (Session.context s) sat_symmetric in
  Session.push s;
  ignore (Oxsmt_query_loader.assert_all ~presolve:true s parsed : bool);
  if Session.symbreak_active_for_test s
  then fail "B1: emitted symmetry breaking under a pushed frame (restriction bypassed)"
  else ok "B1: no emission under a pushed frame"
;;

(* R2/B2: symmetry breaking must NOT emit when a lemma is registered — a during-solve
   instance would extend the formula and break the symmetry un-retractably (check_sat
   fixes its assumptions once). RED (emits) with the restriction disabled. *)
let test_no_emit_with_lemmas () =
  let s = Session.create () in
  let ctx = Session.context s in
  ignore
    (Session.assert_lemma
       s
       ~qvars:[ "x", Sort.int ]
       ~build:(fun _qv -> { Session.body = Context.bool_const ctx true; triggers = [] })
     : Session.lemma);
  let parsed = Parser.parse_into (Session.env s) ctx sat_symmetric in
  ignore (Oxsmt_query_loader.assert_all ~presolve:true s parsed : bool);
  if Session.symbreak_active_for_test s
  then fail "B2: emitted symmetry breaking with a lemma registered (restriction bypassed)"
  else ok "B2: no emission when a lemma is registered"
;;

(* Rider 3 / F3: a REAL Sort.hash collision. [Sort.hash] is non-injective — an
   uninterpreted sort whose symbol hashes to [h] collides with [BitVec w] when
   [3h+2 = 7w+5] (h ≡ 1 mod 7, e.g. h=8 ↔ w=3). Constants of the two colliding sorts, with
   EMPTY occurrence signatures, land in one hash bucket AND one signature bucket under the
   buggy grouping, so a cross-sort candidate pair drives [Context.eq] to
   [Term.Sort_error]. [Sort.equal] grouping keeps them apart. RED (raises) with the hash
   grouping restored. *)
let test_f3_hash_collision () =
  let env, cap = Env.create_with_cap () in
  let ctx = Context.create env in
  let rec find i =
    let sym = Env.declare_sort env (Printf.sprintf ".symbreak.scoll.%d" i) in
    let h = Symbol.hash sym in
    if h >= 8 && h mod 7 = 1 then Sort.uninterpreted sym, (3 * h) - 3 else find (i + 1)
  in
  let usort, num = find 0 in
  let w = num / 7 in
  let bvsort = Sort.bitvec w in
  if Sort.hash usort <> Sort.hash bvsort
  then
    fail "F3-collision: constructed sorts do not actually hash-collide (test setup bug)"
  else (
    let mk name sort =
      Context.app ctx (Env.declare_fun env name (Rank.create [] sort)) []
    in
    let a0 = mk ".symbreak.a0" usort
    and a1 = mk ".symbreak.a1" usort in
    let b0 = mk ".symbreak.b0" bvsort
    and b1 = mk ".symbreak.b1" bvsort in
    let asserts =
      [ Context.not_ ctx (Context.eq ctx a0 a1); Context.not_ ctx (Context.eq ctx b0 b1) ]
    in
    match Presolve.symmetry_break ~counter:(ref 0) cap env ctx asserts with
    | _ ->
      ok "F3-collision: hash-colliding sorts handled by Sort.equal grouping (no crash)"
    | exception e ->
      fail
        "F3-collision: symmetry_break raised %s (cross-sort pairing)"
        (Printexc.to_string e))
;;

(* B1 verdict-level property guard (not a RED test): codex's exact intermediate-frame
   sequence — push; symmetric assert_presolved; pop; check_sat — must report the correct
   verdict on the REAL code. The restriction + pop-deactivation both keep it correct; this
   catches a future change that regresses BOTH at once (stale active lex clauses surviving
   a pop), which no single-mutation RED test can express. *)
let test_b1_verdict_guard () =
  let s = Session.create () in
  let parsed = Parser.parse_into (Session.env s) (Session.context s) sat_symmetric in
  Session.push s;
  ignore (Oxsmt_query_loader.assert_all ~presolve:true s parsed : bool);
  Session.pop s;
  match Session.check_sat s with
  | Session.Sat | Session.Unknown ->
    ok "B1 guard: push/assert_presolved/pop/check verdict correct"
  | Session.Unsat ->
    fail "B1 guard: WRONG-UNSAT after push/assert_presolved/pop (stale lex clauses)"
;;

let () =
  print_string "symbreak_test:\n";
  test_detector_fires ();
  test_detector_rejects ();
  test_sat_preserved ();
  test_vp_mutant_flips ();
  test_unsat_preserved ();
  test_f1_incremental ();
  test_f2_counter ();
  test_f3_multisort ();
  test_b1_no_emit_under_frame ();
  test_b1_verdict_guard ();
  test_no_emit_with_lemmas ();
  test_f3_hash_collision ();
  Printf.printf "symbreak_test: %d checks, %d failures\n" !checks !failures;
  if !failures > 0 then exit 1
;;

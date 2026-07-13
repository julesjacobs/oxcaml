(* End-to-end acceptance for the QF_BV lane THROUGH THE REAL DISPATCH: parse each
   tests/bv-goldens/*.smt2 with the (test-only) SMT-LIB parser, load it into the shipped
   Session, and run Session.check_sat — which routes a pure-QF_BV assertion set to the
   eager bit-blaster (Bv_dispatch/Bv_solve) and leaves anything mixed on the combinator's
   fail-closed degrade path. The produced verdict must equal the file's declared
   [:status]; a sat additionally must surface a model (Bv_solve has already re-checked
   that model with the independent evaluator before returning Sat, so a surfaced sat is
   self-certified).

   Coverage: two sat files (bvadd inverse, concat/extract), two unsat files (bvand-zero,
   bvult-irreflexive), and one DOOR TEST — a QF_UFBV file with a genuine uninterpreted
   function over bitvectors, which the conservative pure-BV gate must reject so it
   degrades to a sound unknown (never a fabricated verdict). Self-contained: no z3, no
   external oracle. Nonzero exit on any mismatch. *)

module Session = Oxsmt_interface.Session
module Parser = Oxsmt_smtlib_parser.Parser
module Context = Oxsmt_core.Context
module Sort = Oxsmt_core.Sort
module Bv = Oxsmt_core.Bv
module Bigint = Oxsmt_core.Bigint
module Rank = Oxsmt_core.Rank
module Qvar = Oxsmt_ematch.Qvar

let failures = ref 0

let read_file path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s
;;

(* pull the token after "(set-info :status " — sat | unsat | unknown *)
let status_of src =
  let marker = ":status" in
  match
    let re_len = String.length marker in
    let rec find i =
      if i + re_len > String.length src
      then None
      else if String.sub src i re_len = marker
      then Some (i + re_len)
      else find (i + 1)
    in
    find 0
  with
  | None -> "unknown"
  | Some j ->
    let n = String.length src in
    let k = ref j in
    while !k < n && (src.[!k] = ' ' || src.[!k] = '\t') do
      incr k
    done;
    let start = !k in
    while !k < n && src.[!k] <> ' ' && src.[!k] <> ')' && src.[!k] <> '\n' do
      incr k
    done;
    String.sub src start (!k - start)
;;

let solve src =
  let s = Session.create () in
  match Parser.parse_into (Session.env s) (Session.context s) src with
  | exception (Parser.Malformed _ | Parser.Unsupported _) -> "unknown", None
  | parsed ->
    if not (Oxsmt_query_loader.assert_all ~presolve:true s parsed)
    then "unknown", None
    else (
      match Session.check_sat s with
      | Session.Sat -> "sat", Session.get_model s
      | Session.Unsat -> "unsat", None
      | Session.Unknown -> "unknown", None)
;;

(* F1 soundness (codex BLOCKER): [is_pure_bv] inspects only the ground [t.asserted], so a
   live [forall] lemma is invisible to it. WITHOUT the [not (has_live_lemma)] gate, a
   pure-BV SAT ground set plus a live lemma routes to the bit-blaster, which ignores the
   quantifier and reports [Sat] — a model that ignores a quantifier (wrong-SAT). WITH the
   gate, a live lemma forces the combinator path where THE SOUNDNESS RULE degrades to
   [Unknown]. This test builds exactly that session and asserts [Unknown]; it FAILS
   (reports Sat) against the pre-fix code. *)
let run_f1_soundness () =
  let s = Session.create () in
  let ctx = Session.context s in
  let env = Session.env s in
  let x = Context.const ctx (Session.declare_const s "f1x" (Sort.bitvec 8)) in
  (* pure-BV, satisfiable on its own: x <u 3 *)
  Session.assert_term
    s
    (Bv.binop ctx env Bv.Bvult x (Bv.const ctx env ~value:(Bigint.of_int 3) ~width:8));
  (* a live lemma (forall y. y + 0 = y), kept live by a non-refuting instance *)
  let a = Context.const ctx (Session.declare_const s "f1a" Sort.int) in
  let lemma =
    Session.assert_lemma
      s
      ~qvars:[ "y", Sort.int ]
      ~build:(fun qv ->
        let y = Qvar.to_term qv.(0) in
        { Session.body = Context.eq ctx (Context.add ctx y (Context.int_const ctx 0)) y
        ; triggers = []
        })
  in
  Session.instantiate s lemma [| a |];
  let v = Session.check_sat s in
  match v with
  | Session.Unknown ->
    Printf.printf "  ok   F1 live-lemma + pure-BV -> unknown (no wrong-Sat)\n"
  | Session.Sat ->
    incr failures;
    Printf.printf "  FAIL F1 live-lemma + pure-BV -> SAT (wrong: quantifier ignored)\n"
  | Session.Unsat ->
    incr failures;
    Printf.printf "  FAIL F1 live-lemma + pure-BV -> unsat (unexpected)\n"
;;

(* F2 soundness (codex BLOCKER): the '\bv|...' marker is lexer-proof on the PARSER path,
   but the programmatic declaration door must also refuse it. WITHOUT the guard, a
   [declare_fun "\bv|bvadd|1"] forges a symbol [Bv.view] decodes as a real bvadd, and the
   bit-blaster encodes a user's opaque function as bit-vector addition (a wrong-UNSAT /
   wrong verdict). This test forges exactly that name (what Bv would mint for bvadd at
   width 1) through the SESSION declaration door and asserts it RAISES; it FAILS (the
   declare succeeds) against the pre-fix code. *)
let run_f2_forge_rejected () =
  let s = Session.create () in
  let bv1 = Sort.bitvec 1 in
  let forged = "\\bv|bvadd|1" in
  (match Session.declare_fun s forged (Rank.create [ bv1; bv1 ] bv1) with
   | exception Invalid_argument _ ->
     Printf.printf "  ok   F2 programmatic bv-marker forge rejected (declare_fun)\n"
   | _ ->
     incr failures;
     Printf.printf "  FAIL F2 forge NOT rejected: declare_fun admitted a '\\bv|' name\n");
  (* the sort door is guarded by the same check *)
  match Session.declare_sort s "\\bv|sortforge" with
  | exception Invalid_argument _ ->
    Printf.printf "  ok   F2 programmatic bv-marker forge rejected (declare_sort)\n"
  | _ ->
    incr failures;
    Printf.printf "  FAIL F2 forge NOT rejected: declare_sort admitted a '\\bv|' name\n"
;;

let () =
  let dir = if Array.length Sys.argv > 1 then Sys.argv.(1) else "tests/bv-goldens" in
  let files =
    Sys.readdir dir
    |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".smt2")
    |> List.sort String.compare
  in
  print_endline "bv-goldens self-test (real dispatch):";
  List.iter
    (fun f ->
       let path = Filename.concat dir f in
       let src = read_file path in
       let expected = status_of src in
       let got, model = solve src in
       let ok = String.equal got expected in
       (* a sat must also surface a (self-checked) model *)
       let model_ok =
         (not (String.equal got "sat"))
         ||
         match model with
         | Some (_, (_ :: _ as _binds)) -> true
         | _ -> false
       in
       if not ok
       then (
         incr failures;
         Printf.printf "  FAIL %-40s expected %s, got %s\n" f expected got)
       else if not model_ok
       then (
         incr failures;
         Printf.printf "  FAIL %-40s sat but no surfaced model\n" f)
       else Printf.printf "  ok   %-40s %s\n" f got)
    files;
  run_f1_soundness ();
  run_f2_forge_rejected ();
  Printf.printf
    "\nbv-goldens self-test: %d file(s), %d failure(s)\n"
    (List.length files)
    !failures;
  if !failures > 0 then exit 1
;;

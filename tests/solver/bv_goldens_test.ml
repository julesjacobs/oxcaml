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
module Env = Oxsmt_core.Env
module Internal_minter = Oxsmt_core.Internal_minter
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
  match
    Parser.parse_into
      ~internal_mint:(Session.parse_minter s)
      (Session.env s)
      (Session.context s)
      src
  with
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
  let mint = Internal_minter.mint (Session.parse_minter s) in
  let x = Context.const ctx (Session.declare_const s "f1x" (Sort.bitvec 8)) in
  (* pure-BV, satisfiable on its own: x <u 3 *)
  Session.assert_term
    s
    (Bv.binop ctx mint Bv.Bvult x (Bv.const ctx mint ~value:(Bigint.of_int 3) ~width:8));
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

(* Programmatic bit-vector-marker forge is rejected at BOTH declaration doors (board #58
   migration + the F2 codex BLOCKER byte guard).

   The bit-vector builders now mint into the reserved [.oxsmt.bv.*] namespace via the
   cap-gated [Env.declare_reserved]. Were a caller able to declare a marker-shaped name at
   a public door, [Bv.view] would decode their opaque function as a real [bvadd] and the
   bit-blaster would encode it as bit-vector addition (a wrong verdict). Two independent
   barriers close every programmatic door:

   (1) SESSION door: [Session.declare_fun]/[declare_sort] reject the name — the real
       marker [.oxsmt.bv|bvadd|1] by the [.oxsmt.] reserved prefix (the PRIMARY board-#58
       guard), and the legacy [\bv|...] form by the '\'/'|' byte class (F2
       defense-in-depth).

   (2) RAW Env door: [Session.env |> Env.declare_fun] — the door the Session-level F2
       guard could NOT close (it sits above [Env]). Board #58's byte-class + reserved
       rejection at the root [Env] door closes it: forging the real marker
       [.oxsmt.bv|bvadd|1] there raises [Env.Reserved_symbol]. This check goes RED against
       pre-migration code, where the bit-vector builders minted [\bv|...] names straight
       through [Env.declare_fun] and the raw-Env door admitted a forged marker (the
       documented residual). *)
let run_f2_forge_rejected () =
  let s = Session.create () in
  let bv1 = Sort.bitvec 1 in
  let rank = Rank.create [ bv1; bv1 ] bv1 in
  (* the real board-#58 marker for bvadd at width 1 (what Bv.const/binop now mint) *)
  let marker_fun = ".oxsmt.bv|bvadd|1" in
  let check_session_rejects name what =
    match Session.declare_fun s name rank with
    | exception Invalid_argument _ ->
      Printf.printf "  ok   forge rejected at Session.declare_fun (%s)\n" what
    | _ ->
      incr failures;
      Printf.printf "  FAIL Session.declare_fun admitted a forged marker (%s)\n" what
  in
  (* PRIMARY guard: the real reserved marker name. DEFENSE-IN-DEPTH: the legacy byte form. *)
  check_session_rejects marker_fun "reserved .oxsmt.bv marker";
  check_session_rejects "\\bv|bvadd|1" "legacy '\\bv|' byte class";
  (match Session.declare_sort s ".oxsmt.bv|sortforge" with
   | exception Invalid_argument _ ->
     Printf.printf "  ok   forge rejected at Session.declare_sort (reserved marker)\n"
   | _ ->
     incr failures;
     Printf.printf "  FAIL Session.declare_sort admitted a forged marker\n");
  (* RAW Env door (the residual the Session guard could not close): board #58 closes it. *)
  let env = Session.env s in
  (match Env.declare_fun env marker_fun rank with
   | exception Env.Reserved_symbol _ ->
     Printf.printf "  ok   forge rejected at raw Env.declare_fun (reserved marker)\n"
   | _ ->
     incr failures;
     Printf.printf "  FAIL raw Env.declare_fun admitted a forged .oxsmt.bv marker\n");
  match Env.declare_sort env ".oxsmt.bv|sortforge" with
  | exception Env.Reserved_symbol _ ->
    Printf.printf "  ok   forge rejected at raw Env.declare_sort (reserved marker)\n"
  | _ ->
    incr failures;
    Printf.printf "  FAIL raw Env.declare_sort admitted a forged .oxsmt.bv marker\n"
;;

(* Rank cross-check hardening (board #58, codex CRITICAL follow-up).
   [Session.internal_minter] is a PUBLIC cap-backed reserved minter, so a caller can mint
   a bit-vector MARKER name over a rank that DISAGREES with the name's decoded
   operator/widths — e.g. the width-1 [bvadd] marker [.oxsmt.bv|bvadd|1] ranked over
   [BitVec 2]. Without the cross-check, [Bv.view] decodes it by name as width-1 [bvadd]
   and the bit-blaster imposes width-1 addition on the caller's opaque width-2 operands:
   [f(0,0)] "=" 0, so [f(0,0)=1] returns a WRONG [unsat] (as an opaque function [f(0,0)]
   may be 1). [Bv.view] now verifies the decoded op's operand and result sorts against the
   term's ACTUAL sorts, so a name/rank mismatch is NOT a bit- vector op: it falls to the
   combinator's fail-closed path (bit-vector-sorted -> [unknown]).

   This test forges exactly that mismatch and asserts the verdict is NOT [unsat]; it FAILS
   (wrong [unsat]) against the pre-hardening [Bv.view]. (A marker minted with the MATCHING
   rank IS the real [bvadd] — byte-identical to a legitimate parser mint — and stays
   [unsat]; that residual is a property of the public minter, not the consuming side.) *)
let run_rank_crosscheck () =
  let s = Session.create () in
  let ctx = Session.context s in
  (* board #58 O-MINTER: the width-1 bvadd marker is ADMITTED by parse_minter's bv
     grammar, so the mint SUCCEEDS — the rank cross-check (Bv.view), not the admit gate,
     is what keeps this name<>rank mint inert. *)
  let mint = Internal_minter.mint (Session.parse_minter s) in
  let bv2 = Sort.bitvec 2 in
  (* the width-1 bvadd marker name, minted over a width-2 rank (name <> rank) *)
  let f = mint ".oxsmt.bv|bvadd|1" (Rank.create [ bv2; bv2 ] bv2) in
  let z = Context.const ctx (Session.declare_const s "rz" bv2) in
  Session.assert_term s (Context.eq ctx z (Bv.const ctx mint ~value:Bigint.zero ~width:2));
  let f00 = Context.app ctx f [ z; z ] in
  Session.assert_term
    s
    (Context.eq ctx f00 (Bv.const ctx mint ~value:(Bigint.of_int 1) ~width:2));
  match Session.check_sat s with
  | Session.Unsat ->
    incr failures;
    Printf.printf
      "  FAIL rank-crosscheck: name<>rank marker reinterpreted as bvadd -> wrong unsat\n"
  | Session.Sat | Session.Unknown ->
    Printf.printf
      "  ok   rank-crosscheck: name<>rank marker not reinterpreted (no wrong unsat)\n"
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
  run_rank_crosscheck ();
  Printf.printf
    "\nbv-goldens self-test: %d file(s), %d failure(s)\n"
    (List.length files)
    !failures;
  if !failures > 0 then exit 1
;;

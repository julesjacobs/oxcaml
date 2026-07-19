(* Sat-direction gate for the datatypes model-construction lane (GOALS Datatypes: "every
   sat answer includes a model with actual constructor trees that the evaluator checks").
   Two parts:

   1. GOLDENS — drive every [:status sat] .smt2 under tests/dt-goldens-sat/ through the
      real {!Session} (the exact loader path the shipped CLI uses) and assert the verdict
      is a CHECKED [Sat]: check_sat only returns [Sat] after [Dt_model_check] validated
      the constructor-tree model against the original assertions (session.ml commit_sat).
      These files degraded to [unknown] before this lane. Kept OUT of the harness
      (tests/dt-goldens), whose sat path runs the DT-unaware external eval CLI — the
      tree-model transport to that N-version evaluator is a named follow-up; v1 soundness
      is the in-process DT checker.

   2. DISCRIMINATION — the banked review rule: a model self-check that only ever passes is
      worthless. Prove {!Dt_model_check} is RED against a deliberately WRONG constructor
      tree (and GREEN on the right one), so a bad extracted model can only ever degrade a
      sat to [unknown], never certify a wrong sat. *)

open Oxsmt_core
module Session = Oxsmt_interface.Session
module Dt_model_check = Oxsmt_interface.Dt_model_check
module Dt = Oxsmt_dt.Dt
module Parser = Oxsmt_smtlib_parser.Parser
module Defs = Datatype_defs

let checks = ref 0
let failures = ref 0

let fail fmt =
  Printf.ksprintf
    (fun s ->
      incr failures;
      print_string ("  FAIL " ^ s ^ "\n"))
    fmt
;;

let expect_sat name verdict =
  incr checks;
  match verdict with
  | Session.Sat -> ()
  | Session.Unsat -> fail "%s: got unsat, want checked-sat" name
  | Session.Unknown -> fail "%s: got unknown, want checked-sat" name
;;

let expect_bool name got want =
  incr checks;
  if Bool.equal got want then () else fail "%s: got %b, want %b" name got want
;;

let read_file path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s
;;

(* Solve one .smt2 file through the shared loader (same path as oxsmt_cli /
   corpus_classify) and return the session verdict. *)
let solve src =
  let s = Session.create () in
  match Parser.parse_into (Session.env s) (Session.context s) src with
  | exception (Parser.Malformed _ | Parser.Unsupported _) -> Session.Unknown
  | parsed ->
    if Oxsmt_query_loader.assert_all s parsed
    then Session.check_sat s
    else Session.Unknown
;;

let run_goldens dir =
  let files =
    Sys.readdir dir
    |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".smt2")
    |> List.sort String.compare
  in
  List.iter
    (fun f -> expect_sat ("golden " ^ f) (solve (read_file (Filename.concat dir f))))
    files
;;

(* Build a tiny [nat = succ(pred:nat) | zero] and prove the checker discriminates a right
   from a wrong constructor-tree model on the assertion [(_ is succ) n]. *)
let run_discrimination () =
  let s = Session.create () in
  let nat = Sort.datatype_ (Session.declare_sort s "nat") in
  let dt =
    Session.declare_datatype
      s
      nat
      [ { Session.ctor_name = "succ"; fields = [ "pred", nat ] }
      ; { Session.ctor_name = "zero"; fields = [] }
      ]
  in
  let reg = Defs.add Defs.empty dt in
  let ctx = Session.context s in
  let n = Context.const ctx (Session.declare_const s "n" nat) in
  let is_succ = (List.nth dt.Defs.constructors 0).Defs.tester in
  let assertion = Context.app ctx is_succ [ n ] in
  (* the RIGHT model: n = succ(zero) — is-succ n holds *)
  let good : (Term.t * Dt.ctor_tree) list =
    [ n, Dt.Ctor ("succ", [ Dt.Ctor ("zero", []) ]) ]
  in
  (* a WRONG model: n = zero — is-succ n is FALSE; the checker must reject it *)
  let wrong : (Term.t * Dt.ctor_tree) list = [ n, Dt.Ctor ("zero", []) ] in
  expect_bool
    "discrimination: right model accepted"
    (Dt_model_check.check reg good [ assertion ])
    true;
  expect_bool
    "discrimination: wrong model rejected"
    (Dt_model_check.check reg wrong [ assertion ])
    false;
  (* a model MISSING the needed binding must fail closed (never a spurious accept) *)
  expect_bool
    "discrimination: missing binding fails closed"
    (Dt_model_check.check reg [] [ assertion ])
    false
;;

(* codex B1 discrimination: a value that does NOT inhabit its position's sort must be
   rejected by the checker's sort-inhabitance validation. This is the direct unit-level
   proof of the B1 fix: an [Uninterp] leaf in a BOOL datatype field is the exact
   ill-formed model the old checker accepted (admitting a 2-element sort as unbounded =>
   wrong-sat). Proven RED against the pre-fix checker (which had no inhabitance check). *)
let run_bool_inhabitance () =
  let s = Session.create () in
  let box_sort = Sort.datatype_ (Session.declare_sort s "BoxD") in
  let dt =
    Session.declare_datatype
      s
      box_sort
      [ { Session.ctor_name = "box"; fields = [ "val", Sort.bool ] } ]
  in
  let reg = Defs.add Defs.empty dt in
  let ctx = Session.context s in
  let p = Context.const ctx (Session.declare_const s "p" box_sort) in
  let q = Context.const ctx (Session.declare_const s "q" box_sort) in
  let r = Context.const ctx (Session.declare_const s "r" box_sort) in
  let assertion = Context.distinct ctx [ p; q; r ] in
  (* the B1 ill-formed model: three boxes whose Bool fields are distinct Uninterp ids — a
     Bool position may not hold an Uninterp. The checker must REJECT it (=> unknown),
     never admit three pairwise-distinct 2-valued fields as sat. *)
  let bad_uninterp_in_bool : (Term.t * Dt.ctor_tree) list =
    [ p, Dt.Ctor ("box", [ Dt.Leaf (Model.Uninterp 7) ])
    ; q, Dt.Ctor ("box", [ Dt.Leaf (Model.Uninterp 2) ])
    ; r, Dt.Ctor ("box", [ Dt.Leaf (Model.Uninterp 4) ])
    ]
  in
  expect_bool
    "B1 inhabitance: Uninterp in a Bool field rejected"
    (Dt_model_check.check reg bad_uninterp_in_bool [ assertion ])
    false;
  (* a well-formed 2-box model (distinct Bool fields) IS accepted — the fix does not
     over-reject legitimate Bool-field models *)
  let good_two : (Term.t * Dt.ctor_tree) list =
    [ p, Dt.Ctor ("box", [ Dt.Leaf (Model.Bool false) ])
    ; q, Dt.Ctor ("box", [ Dt.Leaf (Model.Bool true) ])
    ]
  in
  expect_bool
    "B1 inhabitance: well-formed 2-box (distinct Bools) accepted"
    (Dt_model_check.check reg good_two [ Context.distinct ctx [ p; q ] ])
    true;
  (* an out-of-sort tree (a fabricated constructor name in a datatype position) is
     rejected — closing codex B2 (the checker validates constructor legality, not just
     structure). The assertion is a TAUTOLOGY [(= p p)], so structural evaluation alone
     would ACCEPT the bogus model; only the sort-inhabitance validation rejects it, making
     this a true inhabitance discriminator. *)
  let bad_bogus_ctor : (Term.t * Dt.ctor_tree) list =
    [ p, Dt.Ctor ("bogus", [ Dt.Leaf (Model.Bool false) ]) ]
  in
  expect_bool
    "B2 inhabitance: fabricated constructor name rejected"
    (Dt_model_check.check reg bad_bogus_ctor [ Context.eq ctx p p ])
    false
;;

(* F1 obligation (logs/dt-models-review-fable.md): PIN the commit_sat -> Dt_model_check
   WIRING, which run_discrimination alone does not (it exercises the checker in
   isolation). The reviewer showed a checker-bypass ([| Some _ -> Sat]) stays green
   because the goldens are genuinely sat and the conflict cases are caught by the THEORY.
   This drives a genuinely-sat query through the FULL product path (parse -> loader ->
   Session.check_sat), substitutes the checker (Session.For_test.set_dt_checker), and
   asserts the DT verdict tracks it: a reject-all stub forces [Unknown] (a bypass would
   ignore the stub and report [Sat] — RED), and the stub is observed INVOKED by
   commit_sat; an accept-all stub yields [Sat]. Each restores [None] (the real checker,
   the only production state) before asserting so a failure cannot leak the stub into
   later checks. [src] is a genuinely-sat DT golden. *)
let run_fault_injection src =
  (* baseline: the real checker (None) certifies this genuinely-sat query *)
  Session.For_test.set_dt_checker None;
  expect_sat "fault-injection baseline: real checker -> sat" (solve src);
  (* reject-all stub: commit_sat MUST consult it and degrade to unknown *)
  let rejects = ref 0 in
  Session.For_test.set_dt_checker
    (Some
       (fun _ _ _ ->
         incr rejects;
         false));
  let v_reject = solve src in
  Session.For_test.set_dt_checker None;
  incr checks;
  (match v_reject with
   | Session.Unknown -> ()
   | Session.Sat ->
     fail
       "fault-injection: reject-all stub did NOT force unknown — commit_sat bypasses the \
        checker verdict (F1 regression)"
   | Session.Unsat -> fail "fault-injection: reject-all stub produced unsat (unexpected)");
  expect_bool
    "fault-injection: reject-all stub was invoked by commit_sat"
    (!rejects > 0)
    true;
  (* accept-all stub: the true-branch is wired (a passing checker yields sat) *)
  let accepts = ref 0 in
  Session.For_test.set_dt_checker
    (Some
       (fun _ _ _ ->
         incr accepts;
         true));
  let v_accept = solve src in
  Session.For_test.set_dt_checker None;
  expect_sat "fault-injection: accept-all stub -> sat" v_accept;
  expect_bool
    "fault-injection: accept-all stub was invoked by commit_sat"
    (!accepts > 0)
    true
;;

(* DAG-blowup regression (codex/fable dt-spine finding 2). The model BUILDER memoizes
   [base_tree] per sort ({!Oxsmt_dt.Dt}), so a candidate value can be a shared DIAMOND
   DAG: a chain of sorts [S_i = c_i(S_(i+1), S_(i+1))] bottoming out at a nullary [end]
   has only [N] distinct physical [Ctor] nodes but [2^N] root-to-leaf paths (both fields
   of every level point at the SAME physical sub-tree). Feed the checker exactly such a
   value and require it to (a) ACCEPT a well-formed diamond, (b) REJECT an ill-formed one
   whose ROOT names no constructor of the sort, and (c) REJECT one whose shared BOTTOM has
   the wrong arity — proving the physical-identity memoization did not turn the checker
   into a rubber-stamp. Before the memoization the ACCEPT case re-derived over all [2^N]
   paths and hung the sat authority on a trivially-satisfiable input; this test therefore
   does not complete at all against the un-memoized checker (RED = the whole gate times
   out), and is ~instant with it (one visit per distinct node). *)
let run_dag_blowup () =
  let depth = 60 in
  let env = Env.create () in
  let sort_syms =
    Array.init (depth + 1) (fun i -> Env.declare_sort env (Printf.sprintf "S%d" i))
  in
  let sorts = Array.map Sort.datatype_ sort_syms in
  let reg = ref Defs.empty in
  for i = 0 to depth - 1 do
    let name s = Printf.sprintf "%s%d" s i in
    let ci =
      Env.declare_fun
        env
        (name "c")
        (Rank.create [ sorts.(i + 1); sorts.(i + 1) ] sorts.(i))
    in
    let li = Env.declare_fun env (name "l") (Rank.create [ sorts.(i) ] sorts.(i + 1)) in
    let ri = Env.declare_fun env (name "r") (Rank.create [ sorts.(i) ] sorts.(i + 1)) in
    let is_ci = Env.declare_fun env (name "is-c") (Rank.create [ sorts.(i) ] Sort.bool) in
    reg
    := Defs.add
         !reg
         { Defs.sort_sym = sort_syms.(i)
         ; constructors =
             [ { Defs.sym = ci
               ; selectors =
                   [ { Defs.sym = li; index = 0; field_sort = sorts.(i + 1) }
                   ; { Defs.sym = ri; index = 1; field_sort = sorts.(i + 1) }
                   ]
               ; tester = is_ci
               }
             ]
         }
  done;
  let endsym = Env.declare_fun env "end" (Rank.create [] sorts.(depth)) in
  let is_end = Env.declare_fun env "is-end" (Rank.create [ sorts.(depth) ] Sort.bool) in
  reg
  := Defs.add
       !reg
       { Defs.sort_sym = sort_syms.(depth)
       ; constructors = [ { Defs.sym = endsym; selectors = []; tester = is_end } ]
       };
  let reg = !reg in
  let ctx = Context.create env in
  let x = Context.const ctx (Env.declare_fun env "x" (Rank.create [] sorts.(0))) in
  (* A SHARED diamond: [sub] is bound once per level and placed in BOTH fields, so the two
     children are the SAME physical object — the exact shape the builder's [base_tree]
     memo produces. *)
  let rec diamond i =
    if i = depth
    then Dt.Ctor ("end", [])
    else (
      let sub = diamond (i + 1) in
      Dt.Ctor (Printf.sprintf "c%d" i, [ sub; sub ]))
  in
  let refl = Context.eq ctx x x in
  incr checks;
  if not (Dt_model_check.check reg [ x, diamond 0 ] [ refl ])
  then fail "dag-blowup: a well-formed shared diamond must be a CHECKED sat";
  (* discrimination A: a bogus ROOT constructor name is rejected (rubber-stamp would
     accept) *)
  let bogus_root =
    match diamond 0 with
    | Dt.Ctor (_, fields) -> Dt.Ctor ("nope", fields)
    | leaf -> leaf
  in
  incr checks;
  if Dt_model_check.check reg [ x, bogus_root ] [ refl ]
  then fail "dag-blowup: a bogus-root-constructor tree must be rejected";
  (* discrimination B: an ill-arity shared BOTTOM is rejected — the memo must still
     validate the (shared) leaf, not skip it after the first visit *)
  let rec diamond_bad_bottom i =
    if i = depth
    then
      Dt.Ctor ("end", [ Dt.Leaf (Model.Int Bigint.zero) ])
      (* [end] is nullary: wrong arity *)
    else (
      let sub = diamond_bad_bottom (i + 1) in
      Dt.Ctor (Printf.sprintf "c%d" i, [ sub; sub ]))
  in
  incr checks;
  if Dt_model_check.check reg [ x, diamond_bad_bottom 0 ] [ refl ]
  then fail "dag-blowup: an ill-arity shared bottom must be rejected"
;;

(* task #47: DIRECT coverage that the model-based interface split does REAL work on the
   DT+LIA combined stack (now THE load-bearing combination mechanism, since CombinedDt
   takes the classic no-fabric path). [dtlia_order_unsat] is the shape where DT entails
   [key t = k] (selector eval) while LIA's initial candidate model disagrees ([k>0],
   [key t<=0]); the combinator's [find_disagreement] must return that shared Int pair and
   emit the ℤ-trichotomy split. Assert BOTH the verdict AND [Session.splits > 0] —
   end-verdict alone would pass even if a future change resolved it by some other path, so
   the split count is the direct probe. *)
let solve_with_splits src =
  let s = Session.create () in
  match Parser.parse_into (Session.env s) (Session.context s) src with
  | exception (Parser.Malformed _ | Parser.Unsupported _) -> Session.Unknown, 0, 0
  | parsed ->
    if Oxsmt_query_loader.assert_all s parsed
    then (
      let v = Session.check_sat s in
      v, Session.splits s, Session.fabric_edges_injected s)
    else Session.Unknown, 0, 0
;;

(* Lever-aware (Stage C): [dtlia_order_unsat] is the shape where DT entails [key t = k]
   (selector eval) while LIA's initial candidate model disagrees. On the CLASSIC path
   ([OXSMT_COMBINE_INSEARCH] unset) the combinator resolves it with the ℤ-trichotomy
   [Split] — assert [splits > 0], the load-bearing classic mechanism (the OFF regression
   asset, unchanged). With the lever ON, mechanism I resolves the SAME disagreement by
   IN-SEARCH fabric propagation (the DT congruence merge is notified to LIA), so no split
   fires. Do NOT merely tolerate 0 splits — that would pass a broken lever that silently
   fell back to no-op; instead DISCRIMINATE by requiring a POSITIVE engagement signal:
   [fabric_edges_injected > 0], i.e. the fabric actually propagated an edge. Both modes
   also assert the verdict. *)
let run_combination_split () =
  let lever_on =
    match Sys.getenv_opt "OXSMT_COMBINE_INSEARCH" with
    | Some ("1" | "true" | "yes") -> true
    | _ -> false
  in
  let verdict, splits, edges =
    solve_with_splits (read_file "tests/cases/dtlia_order_unsat.smt2")
  in
  incr checks;
  (match verdict with
   | Session.Unsat -> ()
   | Session.Sat | Session.Unknown ->
     fail "combination split: dtlia_order_unsat got non-unsat");
  incr checks;
  if lever_on
  then (
    if edges <= 0
    then
      fail
        "combination split (OXSMT_COMBINE_INSEARCH ON): dtlia_order_unsat closed with %d \
         fabric edges and %d splits — mechanism I in-search propagation must ENGAGE \
         (edges_injected > 0); 0 edges means the lever fell back to no-op"
        edges
        splits)
  else if splits <= 0
  then
    fail
      "combination split: dtlia_order_unsat closed with %d splits — the model-based \
       interface trichotomy split must fire (it is the load-bearing DT+LIA classic \
       mechanism)"
      splits
;;

let () =
  let dir = if Array.length Sys.argv > 1 then Sys.argv.(1) else "tests/dt-goldens-sat" in
  run_goldens dir;
  run_combination_split ();
  run_discrimination ();
  run_bool_inhabitance ();
  run_dag_blowup ();
  run_fault_injection (read_file (Filename.concat dir "dt_recursive_diseq_sat.smt2"));
  Printf.printf "Dt sat-gate: %d checks, %d failures\n" !checks !failures;
  if !failures > 0 then exit 1
;;

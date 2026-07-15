(* Regression gate for the multi-datatype-per-Session bug (task #51). A batched
   refinement-type VC workload reuses ONE Session/process: each VC is a self-contained
   query that declares its own datatype(s) and is checked under a push/pop scope. The DT
   theory is instantiated LAZILY at the first theory-atom intern and cached for the whole
   session; it used to SNAPSHOT the datatype registry by value, so a datatype declared for
   a LATER VC was invisible to the cached theory -> its terms failed classification ->
   Option.get None in the model builder -> the poison firewall bricked the session to
   sticky [unknown] (every subsequent DT solve too). The corpus never sees this (one file
   = one Session, all datatypes declared up front). Fix: the DT theory holds the registry
   by REFERENCE and reads it live (Dt.create : ... -> Datatype_defs.t ref).

   RED on trunk (lst-VC and nat-VC-again returned unknown); GREEN with the live-ref fix. *)

module Session = Oxsmt_interface.Session
module Sort = Oxsmt_core.Sort
module Context = Oxsmt_core.Context
module Defs = Oxsmt_core.Datatype_defs
module Parser = Oxsmt_smtlib_parser.Parser

let failures = ref 0

let expect name got want =
  let vstr = function
    | Session.Sat -> "sat"
    | Session.Unsat -> "unsat"
    | Session.Unknown -> "unknown"
  in
  if got = want
  then Printf.printf "  ok   %s: %s\n%!" name (vstr got)
  else (
    incr failures;
    Printf.printf "  FAIL %s: got %s, want %s\n%!" name (vstr got) (vstr want))
;;

(* Build [x = C(sel..(x))]-free VCs via the ACCUMULATING declare_datatype product API
   (registry := add, the path the .smt2 loader/product uses; NOT the test set_datatypes). *)
let ctor_sym dt i = (List.nth dt.Defs.constructors i).Defs.sym

let run_multi_datatype () =
  Printf.printf "multi-datatype in one Session (accumulate, push-scoped):\n%!";
  let s = Session.create () in
  let ctx = Session.context s in
  let nat = Sort.datatype_ (Session.declare_sort s "nat") in
  let dnat =
    Session.declare_datatype
      s
      nat
      [ { Session.ctor_name = "succ"; fields = [ "pred", nat ] }
      ; { Session.ctor_name = "zero"; fields = [] }
      ]
  in
  let succ = ctor_sym dnat 0 in
  let nat_vc name =
    Session.push s;
    let x = Context.const ctx (Session.declare_const s name nat) in
    Session.assert_term
      s
      (Context.not_ ctx (Context.eq ctx x (Context.app ctx succ [ x ])));
    let v = Session.check_sat s in
    Session.pop s;
    v
  in
  expect "nat-VC (first datatype)" (nat_vc "x") Session.Sat;
  (* second, DISTINCT datatype declared AFTER the first solve *)
  let lst = Sort.datatype_ (Session.declare_sort s "lst") in
  let _dlst =
    Session.declare_datatype
      s
      lst
      [ { Session.ctor_name = "cons"; fields = [ "hd", nat; "tl", lst ] }
      ; { Session.ctor_name = "nil"; fields = [] }
      ]
  in
  Session.push s;
  let a = Context.const ctx (Session.declare_const s "a" lst) in
  let b = Context.const ctx (Session.declare_const s "b" lst) in
  Session.assert_term s (Context.not_ ctx (Context.eq ctx a b));
  expect "lst-VC (second datatype, was RED)" (Session.check_sat s) Session.Sat;
  Session.pop s;
  expect "nat-VC-again (after 2nd datatype, was RED)" (nat_vc "y") Session.Sat
;;

(* KNOWN GAP (task #54, OUT of scope here): theory-CHOICE staleness. When the FIRST solve
   has no datatype (the combined EUF/LIA theory is cached), a datatype declared for a
   later VC cannot be handled by the already-chosen theory. This is the
   arguably-most-common product pattern (early VCs pure logic, datatypes later) and needs
   a lifecycle/rebuild charter, not this rider. Documented here as EXPECTED-degrade so the
   gap is visible in-tree; NOT counted as a failure. *)
let run_none_then_dt_known_gap () =
  Printf.printf "none-then-DT (task #54, EXPECTED gap, not a failure):\n%!";
  let s = Session.create () in
  let ctx = Session.context s in
  (* first solve: an Int (LIA) atom, no datatype -> the COMBINED theory is instantiated
     and cached (a pure-Bool VC would not instantiate any theory, so it must be
     arithmetic/UF to trigger the theory-CHOICE staleness). *)
  let n = Context.const ctx (Session.declare_const s "n" Sort.int) in
  Session.push s;
  Session.assert_term s (Context.le ctx (Context.int_const ctx 0) n);
  ignore (Session.check_sat s : Session.verdict);
  Session.pop s;
  let nat = Sort.datatype_ (Session.declare_sort s "nat") in
  let _ =
    Session.declare_datatype
      s
      nat
      [ { Session.ctor_name = "succ"; fields = [ "pred", nat ] }
      ; { Session.ctor_name = "zero"; fields = [] }
      ]
  in
  Session.push s;
  (* a DT-discriminating SAT query: two distinct nat values (needs the DT theory's
     constructor-model machinery; pure EUF would also call it sat but via an uninterpreted
     reading — the point is whether the stale non-DT theory choice mishandles it). *)
  let x = Context.const ctx (Session.declare_const s "x" nat) in
  let y = Context.const ctx (Session.declare_const s "y" nat) in
  Session.assert_term s (Context.not_ ctx (Context.eq ctx x y));
  let v = Session.check_sat s in
  Session.pop s;
  let vstr =
    match v with
    | Session.Sat -> "sat"
    | Session.Unsat -> "unsat"
    | Session.Unknown -> "unknown (known gap #54)"
  in
  Printf.printf "  info dt-after-pure-logic-VC: %s\n%!" vstr
;;

(* SOUNDNESS RED (codex CRITICAL on fb605dd1cc). The SMT-LIB LOADER path:
   Session.set_datatypes REPLACES the registry per query. Codex's exact trigger — a symbol
   re-ranked from CONSTRUCTOR to SELECTOR at the SAME rank (which #63 same-rank write-once
   accepts) — made the by-ref read match a stale session-lifetime constructor
   classification (ctor_terms/seen_cat are NOT popped) against the new registry and emit a
   FALSE constructor-clash -> WRONG unsat, where trunk only degraded to unknown. VC1: A =
   f(fsel:B), B = b0; assert x = f b0; pop. VC2 OVERWRITES: A = a0|a1, B = mkB(f:A) (so
   `f` is now a SELECTOR B->A, same rank), b0:B; assert (f b0) = a0 — SAT (b0 = mkB a0).
   The non-monotonicity guard (set_datatypes replacing after the theory is instantiated)
   fail-closes the session to unknown, restoring trunk's safety. REQUIRED: this MUST NOT
   be unsat (unsat = the codex wrong-verdict regression); unknown is the guard's sound
   fail-closed. Correct sat is the #54 reset-per-query contract. *)
let run_loader_overwrite_soundness_red () =
  Printf.printf
    "loader/set_datatypes-overwrite re-rank (codex CRITICAL, guard fail-closed):\n%!";
  let s = Session.create () in
  let load_check src =
    Session.push s;
    let v =
      match Parser.parse_into (Session.env s) (Session.context s) src with
      | exception _ -> Session.Unknown
      | parsed ->
        if Oxsmt_query_loader.assert_all s parsed
        then Session.check_sat s
        else Session.Unknown
    in
    Session.pop s;
    v
  in
  ignore
    (load_check
       "(declare-datatypes ((A 0) (B 0)) (((f (fsel B))) ((b0))))\n\
        (declare-const x A)\n\
        (assert (= x (f b0)))\n"
     : Session.verdict);
  let v2 =
    load_check
      "(declare-datatypes ((A 0) (B 0)) (((a0) (a1)) ((mkB (f A)))))\n\
       (declare-const b0 B)\n\
       (assert (= (f b0) a0))\n"
  in
  match v2 with
  | Session.Unsat ->
    incr failures;
    Printf.printf
      "  FAIL overwrite-rerank VC2: WRONG unsat (codex CRITICAL regressed)\n%!"
  | Session.Unknown ->
    Printf.printf "  ok   overwrite-rerank VC2: unknown (guard fail-closed)\n%!"
  | Session.Sat ->
    Printf.printf "  ok   overwrite-rerank VC2: sat (correct — #54 reset landed?)\n%!"
;;

(* DT-GUARD-ISOLATION RED (codex delta LOW rider). The soundness RED above goes through
   the loader's [assert_all], which calls set_datatypes AND set_arrays(empty); BOTH now
   carry the non-monotonicity guard, so the set_arrays(empty) guard alone already yields
   unknown — removing ONLY the set_datatypes guard would still pass. This variant drives
   the SAME role-reuse re-rank but installs the registry with set_datatypes ONLY (no
   set_arrays call), so the set_datatypes guard is the SOLE thing standing between the
   stale ctor_terms and the false constructor-clash. Stashing just the set_datatypes guard
   makes THIS case wrong-unsat → the gate fails, isolating the DT guard. REQUIRED: must
   not be unsat. *)
let run_loader_overwrite_dt_isolated_red () =
  Printf.printf "set_datatypes-ONLY overwrite re-rank (DT guard in isolation):\n%!";
  let s = Session.create () in
  let load_dt_only src =
    Session.push s;
    let v =
      match Parser.parse_into (Session.env s) (Session.context s) src with
      | exception _ -> Session.Unknown
      | parsed ->
        (* set_datatypes ONLY — deliberately NOT set_arrays, so no array-guard masking *)
        Session.set_datatypes s parsed.Parser.datatypes;
        List.iter (Session.assert_term s) parsed.Parser.assertions;
        Session.check_sat s
    in
    Session.pop s;
    v
  in
  ignore
    (load_dt_only
       "(declare-datatypes ((A 0) (B 0)) (((f (fsel B))) ((b0))))\n\
        (declare-const x A)\n\
        (assert (= x (f b0)))\n"
     : Session.verdict);
  let v2 =
    load_dt_only
      "(declare-datatypes ((A 0) (B 0)) (((a0) (a1)) ((mkB (f A)))))\n\
       (declare-const b0 B)\n\
       (assert (= (f b0) a0))\n"
  in
  match v2 with
  | Session.Unsat ->
    incr failures;
    Printf.printf
      "  FAIL dt-isolated VC2: WRONG unsat (set_datatypes guard not load-bearing)\n%!"
  | Session.Unknown ->
    Printf.printf "  ok   dt-isolated VC2: unknown (set_datatypes guard fail-closed)\n%!"
  | Session.Sat ->
    Printf.printf "  ok   dt-isolated VC2: sat (correct — #54 reset landed?)\n%!"
;;

(* DISJOINT-datatype loader case (info-only). Two self-contained VCs over DISJOINT
   datatypes (nat then lst — no shared/re-ranked symbol). Retained deliberately to show it
   is INSUFFICIENT as a soundness RED: with no role reuse the stale ctor_terms never
   collide with a new-registry constructor, so even WITHOUT the guard this degrades to
   unknown (never wrong-unsat) — a disjoint scenario MASKS the codex/fable CRITICAL. The
   role-reuse RED above is what discriminates. Both live in the gate so the distinction is
   visible in-tree. *)
let run_loader_overwrite_disjoint_info () =
  Printf.printf "loader/overwrite DISJOINT datatypes (info-only, masks role reuse):\n%!";
  let s = Session.create () in
  let load_check src =
    Session.push s;
    let v =
      match Parser.parse_into (Session.env s) (Session.context s) src with
      | exception _ -> Session.Unknown
      | parsed ->
        if Oxsmt_query_loader.assert_all s parsed
        then Session.check_sat s
        else Session.Unknown
    in
    Session.pop s;
    v
  in
  ignore
    (load_check
       "(declare-datatypes ((nat 0)) (((succ (pred nat)) (zero))))\n\
        (declare-const x nat)\n\
        (assert (not (= x (succ x))))\n"
     : Session.verdict);
  let v2 =
    load_check
      "(declare-datatypes ((lst 0)) (((cons (head Int) (tail lst)) (nil))))\n\
       (declare-const a lst)\n\
       (declare-const b lst)\n\
       (assert (not (= a b)))\n"
  in
  let vstr = function
    | Session.Sat -> "sat"
    | Session.Unsat -> "unsat"
    | Session.Unknown -> "unknown"
  in
  Printf.printf
    "  info disjoint VC2: %s (would be unknown even unguarded — no clash)\n%!"
    (vstr v2)
;;

let () =
  run_multi_datatype ();
  run_none_then_dt_known_gap ();
  run_loader_overwrite_soundness_red ();
  run_loader_overwrite_dt_isolated_red ();
  run_loader_overwrite_disjoint_info ();
  if !failures > 0
  then (
    Printf.printf "dt-multi-query gate: %d failure(s)\n%!" !failures;
    exit 1)
  else Printf.printf "dt-multi-query gate: all required checks passed\n%!"
;;

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

(* THEORY-CHOICE staleness (task #54, NOW FIXED — was an EXPECTED-degrade under #51). When
   the FIRST solve has no datatype (the combined EUF/LIA theory is cached), a datatype
   declared for a later VC could not be handled by the already-chosen theory — the
   arguably-most-common product pattern (early VCs pure logic, datatypes later). #54's
   reset-per-query invalidation drops the stale combined theory when the later
   [declare_datatype] mutates the registry, so the next intern rebuilds the DT theory. NOW
   REQUIRED sat (two distinct nat values are satisfiable). *)
let run_none_then_dt () =
  Printf.printf "none-then-DT (task #54 reset-per-query, was EXPECTED-degrade):\n%!";
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
  expect "dt-after-pure-logic-VC" v Session.Sat
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

(* DISJOINT-datatype loader case (task #54, NOW REQUIRED sat — was info-only under #51).
   Two self-contained VCs over DISJOINT datatypes (nat then lst — no shared/re-ranked
   symbol). Under #51 this degraded to unknown regardless of the guard (no role reuse to
   clash), so it was kept info-only to show it could not serve as the soundness RED. Under
   #54's reset-per-query the second overwrite rebuilds the DT theory against [{lst}], so
   VC2 (a <> b over lst) is correctly SAT. *)
let run_loader_overwrite_disjoint () =
  Printf.printf "loader/overwrite DISJOINT datatypes (task #54 reset-per-query):\n%!";
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
  expect "disjoint VC2 (lst a<>b)" v2 Session.Sat
;;

(* FAIL-LOUD RED (task #54 contract-A). A registry replacement is sound only BETWEEN
   self-contained queries. If the caller declares a NEW datatype while the prior query's
   assertions are STILL LIVE (no [pop] between the check_sat and the redeclare), a reset
   would strand in-flight atoms bound to the dropped bijection — the #51 wrong-answer
   path. The contract-A ruling is to fail LOUD (documented [Invalid_argument]) rather than
   reset under live state or silently rebuild. REQUIRED: [set_datatypes] with live
   assertions + an instantiated theory RAISES. (Discriminates the guard: the pre-check
   [asserted <> []] arm is the sole thing turning this into a raise rather than a wrong
   reset.) *)
let run_registry_replace_live_assertions_raises () =
  Printf.printf
    "fail-loud: registry replace with live assertions raises (contract-A):\n%!";
  let s = Session.create () in
  let load src =
    match Parser.parse_into (Session.env s) (Session.context s) src with
    | exception _ -> ()
    | parsed ->
      Session.set_datatypes s parsed.Parser.datatypes;
      List.iter (Session.assert_term s) parsed.Parser.assertions
  in
  (* VC1 at BASE (no push), asserted, checked -> theory instantiated AND [asserted <> []]. *)
  load
    "(declare-datatypes ((A 0) (B 0)) (((f (fsel B))) ((b0))))\n\
     (declare-const x A)\n\
     (assert (= x (f b0)))\n";
  ignore (Session.check_sat s : Session.verdict);
  (* Now REPLACE the registry with live VC1 assertions still active (no pop): must raise. *)
  match
    Session.set_datatypes
      s
      (match
         Parser.parse_into
           (Session.env s)
           (Session.context s)
           "(declare-datatypes ((A 0) (B 0)) (((a0) (a1)) ((mkB (f A)))))\n"
       with
       | parsed -> parsed.Parser.datatypes
       | exception _ -> Defs.empty)
  with
  | () ->
    incr failures;
    Printf.printf
      "  FAIL live-assertion replace: returned (expected Invalid_argument)\n%!"
  | exception Invalid_argument _ ->
    Printf.printf "  ok   live-assertion replace: raised Invalid_argument (fail-loud)\n%!"
;;

(* CRITICAL RED (codex + fable legs on 8ba4609d56): the lemma Manager is a live per-era
   channel OUTSIDE [asserted]. A base-frame (un-pushed) universally-quantified lemma is
   NOT added to [asserted] and survives [pop], so the [asserted = []] fail-loud did not
   cover it. The Manager is USER-INPUT state (the ADR-0012 store fed by [assert_lemma]),
   NOT a derived consequence, so silently dropping it in the new era would be a
   wrong-[sat] channel (master's revised ruling). REMEDY = fail-LOUD: a live lemma is
   treated exactly like a live assertion, so a registry change with a live base-frame
   lemma RAISES [Invalid_argument] rather than resetting under it. REQUIRED: raises.
   Discrimination: drop the [Manager.has_live_lemma] disjunct from the invalidate guard
   and this stops raising (the registry change returns). *)
let run_base_lemma_blocks_reset_red () =
  Printf.printf "base-frame lemma blocks reset — fail-loud (codex/fable CRITICAL):\n%!";
  let s = Session.create () in
  (* Register a BASE-frame forall lemma (no [push]) WITHOUT a [check_sat] — a lemma-only
     check_sat would run the matcher before any theory is chosen. The lemma is now in the
     Manager at the base frame (survives every [pop]); [asserted] is still []. *)
  (match
     Parser.parse_into
       (Session.env s)
       (Session.context s)
       "(declare-fun f (Int) Int)\n\
        (assert (forall ((x Int)) (! (> (f x) 0) :pattern ((f x)))))\n"
   with
   | exception _ -> incr failures
   | parsed -> ignore (Oxsmt_query_loader.assert_all s parsed : bool));
  (* Instantiate the (combined) theory with a GROUND atom in a PUSHED frame, then [pop]:
     the frame's ground atom is dropped so [asserted] returns to [], but the base lemma
     survives — the CRITICAL's precondition (theory instantiated + [asserted] = [] + a
     live base-frame lemma). A [parse_into] only sees the declarations in its OWN text, so
     this ground query is self-contained (its own [g]/[b], NOT the lemma's [f]). *)
  Session.push s;
  (match
     Parser.parse_into
       (Session.env s)
       (Session.context s)
       "(declare-fun g (Int) Int)\n(declare-const b Int)\n(assert (> (g b) 0))\n"
   with
   | exception _ -> incr failures
   | parsed ->
     if Oxsmt_query_loader.assert_all s parsed
     then ignore (Session.check_sat s : Session.verdict)
     else incr failures);
  Session.pop s;
  (* A registry change (declare_datatype) now the theory is instantiated: [asserted] = []
     but a live base-frame lemma remains -> the fail-loud guard MUST raise. *)
  let nat = Sort.datatype_ (Session.declare_sort s "natL") in
  match
    Session.declare_datatype
      s
      nat
      [ { Session.ctor_name = "sL"; fields = [ "pL", nat ] }
      ; { Session.ctor_name = "zL"; fields = [] }
      ]
  with
  | _ ->
    incr failures;
    Printf.printf
      "  FAIL live-lemma reset: returned (expected Invalid_argument — mgr guard missing)\n\
       %!"
  | exception Invalid_argument _ ->
    Printf.printf "  ok   live-lemma reset: raised Invalid_argument (fail-loud)\n%!"
;;

(* MEDIUM RED (codex leg): a DT-triggered reset must NOT drop a still-valid array mode. An
   array query instantiates the arrays theory with a nonempty array registry; after a pop
   (so [asserted] = []) a [declare_datatype] triggers a reset. The array registry is
   unchanged by a datatype mutation, so [uses_arrays] (the public reader of [has_arrays])
   must remain true — else [ensure_theory] still picks the arrays theory while
   [has_arrays] is false and [commit_sat] takes the non-array branch (spurious Unknown;
   completeness only). REQUIRED: [uses_arrays] true after the DT-triggered reset.
   Discrimination: force [has_arrays <- false] in invalidate and this flips false. *)
let run_has_arrays_survives_dt_reset_red () =
  Printf.printf "has_arrays survives a DT-triggered reset (codex MEDIUM):\n%!";
  let s = Session.create () in
  Session.push s;
  (* Arrays need the session's cap-backed minter for the reserved [.oxsmt.arr.*] op
     symbols ([~internal_mint]), exactly as the array-sat gate parses them. *)
  (match
     Parser.parse_into
       ~internal_mint:(Session.parse_minter s)
       (Session.env s)
       (Session.context s)
       "(declare-sort I 0)\n\
        (declare-sort E 0)\n\
        (declare-fun a () (Array I E))\n\
        (declare-fun i () I)\n\
        (declare-fun v () E)\n\
        (assert (not (= (select (store a i v) i) v)))\n"
   with
   | exception _ -> incr failures
   | parsed ->
     if Oxsmt_query_loader.assert_all s parsed
     then ignore (Session.check_sat s : Session.verdict)
     else incr failures);
  Session.pop s;
  (* asserted = [] now; declare a datatype -> reset. The array registry is untouched. *)
  let nat = Sort.datatype_ (Session.declare_sort s "natA") in
  ignore
    (Session.declare_datatype
       s
       nat
       [ { Session.ctor_name = "sA"; fields = [ "pA", nat ] }
       ; { Session.ctor_name = "zA"; fields = [] }
       ]
     : Defs.datatype);
  if Session.uses_arrays s
  then Printf.printf "  ok   uses_arrays after DT reset: true (array mode preserved)\n%!"
  else (
    incr failures;
    Printf.printf
      "  FAIL uses_arrays after DT reset: false (array mode dropped — codex MEDIUM)\n%!")
;;

(* CONTENT-GATE two-sidedness (fable NICE). The reset fires on a registry mutation that is
   nonempty on EITHER side — including [set_datatypes empty] AFTER a nonempty datatype
   registry (the "empty-after-nonempty" side), not only nonempty-after-empty. VC1 declares
   a datatype and solves (TDt cached); after a pop, [set_datatypes empty] clears the
   registry and must RESET the cached DT theory so a subsequent pure-logic VC is served by
   the rebuilt combined theory (correct sat) and [uses_datatypes] reads false. A one-sided
   gate (fire only when the NEW defs are nonempty) would skip this reset and leave the
   stale TDt theory to mishandle the Int VC. *)
let run_set_datatypes_empty_after_nonempty () =
  Printf.printf
    "content-gate two-sided: set_datatypes(empty) after nonempty (fable NICE):\n%!";
  let s = Session.create () in
  let ctx = Session.context s in
  let nat = Sort.datatype_ (Session.declare_sort s "natC") in
  let dnat =
    Session.declare_datatype
      s
      nat
      [ { Session.ctor_name = "sC"; fields = [ "pC", nat ] }
      ; { Session.ctor_name = "zC"; fields = [] }
      ]
  in
  let succ = ctor_sym dnat 0 in
  Session.push s;
  let x = Context.const ctx (Session.declare_const s "xc" nat) in
  Session.assert_term s (Context.not_ ctx (Context.eq ctx x (Context.app ctx succ [ x ])));
  ignore (Session.check_sat s : Session.verdict);
  Session.pop s;
  (* empty-after-nonempty: clears the datatype registry; the gate must still reset. *)
  Session.set_datatypes s Defs.empty;
  if Session.uses_datatypes s
  then (
    incr failures;
    Printf.printf "  FAIL uses_datatypes after set_datatypes(empty): true\n%!")
  else Printf.printf "  ok   uses_datatypes after set_datatypes(empty): false\n%!";
  (* the rebuilt combined theory serves a pure-logic VC. *)
  Session.push s;
  let n = Context.const ctx (Session.declare_const s "nc" Sort.int) in
  Session.assert_term s (Context.le ctx (Context.int_const ctx 0) n);
  let v = Session.check_sat s in
  Session.pop s;
  expect "pure-logic VC after set_datatypes(empty)" v Session.Sat
;;

let () =
  run_multi_datatype ();
  run_none_then_dt ();
  run_loader_overwrite_soundness_red ();
  run_loader_overwrite_dt_isolated_red ();
  run_loader_overwrite_disjoint ();
  run_registry_replace_live_assertions_raises ();
  run_base_lemma_blocks_reset_red ();
  run_has_arrays_survives_dt_reset_red ();
  run_set_datatypes_empty_after_nonempty ();
  if !failures > 0
  then (
    Printf.printf "dt-multi-query gate: %d failure(s)\n%!" !failures;
    exit 1)
  else Printf.printf "dt-multi-query gate: all required checks passed\n%!"
;;

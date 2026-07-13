(* Lemma-tier tranche-2 acceptance (ADR-0012 L2/L3/R4): the E-matching matcher. Two
   layers:

   (1) UNIT tests of {!Matcher.substitutions} against a HAND-ROLLED {!Egraph_view}
       (mirroring how the combinator is tested against hand-rolled children) — pins the
       matcher's backtracking, modulo-congruence argument matching, multi-trigger
       conjunction, qvar coverage, determinism, and the R4 in-enumeration budget in
       isolation.

   (2) END-TO-END tests through the REAL Session + Cdclt + Combine + EUF stack (no mocks,
       no manual [instantiate]): the matcher alone must find the instance that closes a
       goal.

   Discrimination is stated per test: each FAILS against the code without the tranche-2
   matcher (a tranche-1 build with only the manual seed path answers [unknown] where a
   working matcher answers [unsat]). Determinism (I6): every verdict is a pure function of
   the input. *)

open Oxsmt_core
module Session = Oxsmt_interface.Session
module Qvar = Oxsmt_ematch.Qvar
module Lemma = Oxsmt_ematch.Lemma
module Manager = Oxsmt_ematch.Manager
module Matcher = Oxsmt_ematch.Matcher
module Egraph_view = Oxsmt_ematch.Egraph_view
module Trigger = Oxsmt_ematch.Trigger
module Sat = Oxsmt_solver.Sat

let failures = ref 0
let passes = ref 0

let check name cond =
  if cond
  then (
    incr passes;
    Printf.printf "ok   %s\n" name)
  else (
    incr failures;
    Printf.printf "FAIL %s\n" name)
;;

let verdict_str = function
  | Session.Sat -> "sat"
  | Session.Unsat -> "unsat"
  | Session.Unknown -> "unknown"
;;

let int_to_int = Rank.create [ Sort.int ] Sort.int
let int_int_to_int = Rank.create [ Sort.int; Sort.int ] Sort.int
let int_to_bool = Rank.create [ Sort.int ] Sort.bool

(* ================================================================== *)
(* UNIT LAYER — Matcher.substitutions against a hand-rolled Egraph_view. *)
(* ================================================================== *)

(* A tiny scaffold: an Env/Context to mint qvars + build terms, and a hand-rolled view
   whose [app_terms_by_symbol] / [class_members] are supplied per test. *)
type scaffold =
  { env : Env.t
  ; cap : Env.reserved_cap
  ; ctx : Context.t
  ; frame : Sat.var
  }

let scaffold () =
  let env, cap = Env.create_with_cap () in
  let ctx = Context.create env in
  let sat = Sat.create () in
  { env; cap; ctx; frame = Sat.new_var sat }
;;

(* Mint [n] qvars for [lemma_id] and build a Lemma.t from [build] (which sees the qvar
   term array and returns (body, triggers)). *)
let make_lemma sc ~id ~arity build =
  let qv =
    Array.init arity (fun k ->
      Qvar.mint sc.cap sc.env sc.ctx ~lemma_id:id ~index:k Sort.int)
  in
  let body, triggers = build (Array.map Qvar.to_term qv) in
  { Lemma.qvars = qv; body; triggers; id; frame = sc.frame; origin = Lemma.Anonymous }
;;

(* Membership-by-tag hand-rolled view: [apps] maps a head symbol to its registered ground
   App terms; [classes] is a list of congruence classes (each a Term.t list). A term
   absent from every class is its own singleton. Deterministic. *)
let view_of ~apps ~classes : Egraph_view.t =
  let class_of term =
    List.find_opt (fun cls -> List.exists (Term.equal term) cls) classes
  in
  { app_terms_by_symbol =
      (fun sym ->
        List.concat_map (fun (s, terms) -> if Symbol.equal s sym then terms else []) apps)
  ; find_class_opt = (fun _ -> None)
  ; equal_if_registered =
      (fun a b ->
        match class_of a, class_of b with
        | Some ca, Some _ -> List.exists (Term.equal b) ca
        | _ -> Term.equal a b)
  ; class_members =
      (fun term ->
        match class_of term with
        | Some cls -> cls
        | None -> [ term ])
  }
;;

let subst_to_names sigmas =
  List.map
    (fun arr ->
       Array.to_list
         (Array.map
            (fun (t : Term.t) ->
               match t.node with
               | App (s, _) -> Symbol.name s
               | Int_const n -> "#" ^ Bigint.to_string n
               | _ -> "?")
            arr))
    sigmas
;;

(* U-FIND: trigger f(x) against a lone registered f(a) binds x |-> a. Discrimination: a
   matcher that never enumerates candidates returns [] here. *)
let u_find () =
  let sc = scaffold () in
  let f = Env.declare_fun sc.env "f" int_to_int in
  let a = Context.const sc.ctx (Env.declare_fun sc.env "a" (Rank.create [] Sort.int)) in
  let fa = Context.app sc.ctx f [ a ] in
  let lemma =
    make_lemma sc ~id:0 ~arity:1 (fun q ->
      let x = q.(0) in
      ( Context.gt sc.ctx (Context.app sc.ctx f [ x ]) (Context.int_const sc.ctx 0)
      , [ [ Context.app sc.ctx f [ x ] ] ] ))
  in
  let view = view_of ~apps:[ f, [ fa ] ] ~classes:[] in
  let sigmas = Matcher.substitutions view lemma ~budget:(ref 1000) in
  check "U-FIND: f(x) matches f(a) -> [x|->a]" (subst_to_names sigmas = [ [ "a" ] ])
;;

(* U-CONGRUENCE: nested trigger f(g(x)) against registered f(c), where c and g(a) are in
   one congruence class. The argument match must go MODULO congruence (via class_members),
   binding x |-> a. Discrimination: tag-only argument matching (no class_members) yields
   [] because c is not structurally g(_). *)
let u_congruence () =
  let sc = scaffold () in
  let f = Env.declare_fun sc.env "f" int_to_int in
  let g = Env.declare_fun sc.env "g" int_to_int in
  let a = Context.const sc.ctx (Env.declare_fun sc.env "a" (Rank.create [] Sort.int)) in
  let c = Context.const sc.ctx (Env.declare_fun sc.env "c" (Rank.create [] Sort.int)) in
  let ga = Context.app sc.ctx g [ a ] in
  let fc = Context.app sc.ctx f [ c ] in
  let lemma =
    make_lemma sc ~id:0 ~arity:1 (fun q ->
      let x = q.(0) in
      let fgx = Context.app sc.ctx f [ Context.app sc.ctx g [ x ] ] in
      Context.gt sc.ctx fgx (Context.int_const sc.ctx 0), [ [ fgx ] ])
  in
  (* c and g(a) congruent; f(c) registered under head f. *)
  let view = view_of ~apps:[ f, [ fc ]; g, [ ga ] ] ~classes:[ [ c; ga ] ] in
  let sigmas = Matcher.substitutions view lemma ~budget:(ref 1000) in
  check
    "U-CONGRUENCE: f(g(x)) matches f(c) with c=g(a) -> [x|->a]"
    (subst_to_names sigmas = [ [ "a" ] ])
;;

(* U-MULTI: a conjunctive multi-trigger [{f(x), g(y)}] over f(a) and g(b) must bind BOTH
   qvars under one substitution. Discrimination: a matcher that ignores the second
   conjunct leaves y unbound -> the substitution is incomplete -> dropped -> []. *)
let u_multi () =
  let sc = scaffold () in
  let f = Env.declare_fun sc.env "f" int_to_int in
  let g = Env.declare_fun sc.env "g" int_to_int in
  let a = Context.const sc.ctx (Env.declare_fun sc.env "a" (Rank.create [] Sort.int)) in
  let b = Context.const sc.ctx (Env.declare_fun sc.env "b" (Rank.create [] Sort.int)) in
  let fa = Context.app sc.ctx f [ a ] in
  let gb = Context.app sc.ctx g [ b ] in
  let lemma =
    make_lemma sc ~id:0 ~arity:2 (fun q ->
      let x = q.(0)
      and y = q.(1) in
      let fx = Context.app sc.ctx f [ x ]
      and gy = Context.app sc.ctx g [ y ] in
      Context.eq sc.ctx fx gy, [ [ fx; gy ] ])
  in
  let view = view_of ~apps:[ f, [ fa ]; g, [ gb ] ] ~classes:[] in
  let sigmas = Matcher.substitutions view lemma ~budget:(ref 1000) in
  check
    "U-MULTI: {f(x),g(y)} binds both -> [x|->a; y|->b]"
    (subst_to_names sigmas = [ [ "a"; "b" ] ])
;;

(* U-EMPTY-TRIGGER: a lemma with no triggers yields no matcher substitutions
   (auto-selection is tranche 3). *)
let u_empty_trigger () =
  let sc = scaffold () in
  let f = Env.declare_fun sc.env "f" int_to_int in
  let a = Context.const sc.ctx (Env.declare_fun sc.env "a" (Rank.create [] Sort.int)) in
  let lemma =
    make_lemma sc ~id:0 ~arity:1 (fun q ->
      Context.gt sc.ctx (Context.app sc.ctx f [ q.(0) ]) (Context.int_const sc.ctx 0), [])
  in
  let view = view_of ~apps:[ f, [ Context.app sc.ctx f [ a ] ] ] ~classes:[] in
  check
    "U-EMPTY-TRIGGER: no triggers -> no substitutions"
    (Matcher.substitutions view lemma ~budget:(ref 1000) = [])
;;

(* U-CAP: the generation budget is debited INSIDE enumeration (R4). With N candidates and
   a budget below N, matching raises [Budget_exhausted] mid-enumeration rather than
   materializing all N substitutions. Discrimination: a post-round cap (debit only after a
   full round) would return N substitutions here without raising. *)
let u_cap () =
  let sc = scaffold () in
  let f = Env.declare_fun sc.env "f" int_to_int in
  let mk name =
    Context.const sc.ctx (Env.declare_fun sc.env name (Rank.create [] Sort.int))
  in
  let cands =
    List.init 20 (fun i -> Context.app sc.ctx f [ mk (Printf.sprintf "a%d" i) ])
  in
  let lemma =
    make_lemma sc ~id:0 ~arity:1 (fun q ->
      ( Context.gt sc.ctx (Context.app sc.ctx f [ q.(0) ]) (Context.int_const sc.ctx 0)
      , [ [ Context.app sc.ctx f [ q.(0) ] ] ] ))
  in
  let view = view_of ~apps:[ f, cands ] ~classes:[] in
  let raised =
    match Matcher.substitutions view lemma ~budget:(ref 3) with
    | _ -> false
    | exception Matcher.Budget_exhausted -> true
  in
  check "U-CAP: tiny budget raises Budget_exhausted mid-enumeration" raised
;;

(* U-DET: two identical matcher runs produce byte-identical substitution sequences (I6). *)
let u_det () =
  let run () =
    let sc = scaffold () in
    let f = Env.declare_fun sc.env "f" int_to_int in
    let mk name =
      Context.const sc.ctx (Env.declare_fun sc.env name (Rank.create [] Sort.int))
    in
    let cands =
      List.init 5 (fun i -> Context.app sc.ctx f [ mk (Printf.sprintf "a%d" i) ])
    in
    let lemma =
      make_lemma sc ~id:0 ~arity:1 (fun q ->
        ( Context.gt sc.ctx (Context.app sc.ctx f [ q.(0) ]) (Context.int_const sc.ctx 0)
        , [ [ Context.app sc.ctx f [ q.(0) ] ] ] ))
    in
    let view = view_of ~apps:[ f, cands ] ~classes:[] in
    subst_to_names (Matcher.substitutions view lemma ~budget:(ref 1000))
  in
  check "U-DET: two runs identical (5 candidates)" (run () = run ())
;;

(* U-MANAGER-CAP: Manager.round with a tiny gen_budget stops early and sets
   budget_exhausted (R4 at the manager boundary). Discrimination: an unbounded round emits
   all instances and never flags exhaustion. *)
let u_manager_cap () =
  let sc = scaffold () in
  let f = Env.declare_fun sc.env "f" int_to_int in
  let mk name =
    Context.const sc.ctx (Env.declare_fun sc.env name (Rank.create [] Sort.int))
  in
  let cands =
    List.init 20 (fun i -> Context.app sc.ctx f [ mk (Printf.sprintf "a%d" i) ])
  in
  let lemma =
    make_lemma sc ~id:0 ~arity:1 (fun q ->
      ( Context.gt sc.ctx (Context.app sc.ctx f [ q.(0) ]) (Context.int_const sc.ctx 0)
      , [ [ Context.app sc.ctx f [ q.(0) ] ] ] ))
  in
  let mgr = Manager.create ~gen_budget:3 sc.ctx sc.env in
  Manager.add_lemma mgr lemma;
  Manager.begin_check mgr;
  let view = view_of ~apps:[ f, cands ] ~classes:[] in
  let insts = Manager.round mgr view in
  check "U-MANAGER-CAP: budget_exhausted set" (Manager.budget_exhausted mgr);
  check "U-MANAGER-CAP: at most budget instances emitted" (List.length insts <= 3)
;;

(* ================================================================== *)
(* END-TO-END LAYER — real Session stack, matcher alone (no instantiate). *)
(* ================================================================== *)

(* E-FIND: forall x. f(x) > 0 with trigger f(x); ground f(a) < 0. The MATCHER (no manual
   seed) must find x|->a and close the goal. Discrimination: a tranche-1 build (manual
   seeds only) generates no instance -> ground core sat, live lemma -> [unknown]. [unsat]
   here proves the matcher fired end-to-end. *)
let e_find () =
  let s = Session.create () in
  let ctx = Session.context s in
  let f = Session.declare_fun s "f" int_to_int in
  let a = Context.const ctx (Session.declare_const s "a" Sort.int) in
  let fa = Context.app ctx f [ a ] in
  ignore
    (Session.assert_lemma
       s
       ~qvars:[ "x", Sort.int ]
       ~build:(fun qv ->
         let x = Qvar.to_term qv.(0) in
         { Session.body =
             Context.gt ctx (Context.app ctx f [ x ]) (Context.int_const ctx 0)
         ; triggers = [ [ Context.app ctx f [ x ] ] ]
         })
     : Session.lemma);
  Session.assert_term s (Context.lt ctx fa (Context.int_const ctx 0));
  let v = Session.check_sat s in
  check
    (Printf.sprintf
       "E-FIND: matcher closes goal (no seed) -> unsat (got %s)"
       (verdict_str v))
    (v = Session.Unsat);
  let st = Session.lemma_stats s in
  check "E-FIND: exactly one instance generated" (st.instances = 1)
;;

(* E-NESTED: forall x. f(g(x)) > 0 with the NESTED trigger f(g(x)); ground f(g(a)) < 0.
   The matcher must recurse through the trigger's structure — root App f, then the
   argument pattern g(x) against the registered g(a) — to bind x|->a, and the instance
   f(g(a))>0 closes the goal. Discrimination: a matcher that only matches a flat top-level
   App (no recursion into arguments) never binds x -> [unknown].

   (Modulo-EUF-CONGRUENCE argument matching — matching a pattern arg against a term
   reached via an asserted equality rather than present structurally — is pinned at the
   unit level by U-CONGRUENCE with a hand-rolled view carrying a non-trivial class. It is
   NOT tested end-to-end here because the outer loop queries the e-graph at decision level
   0, where the frame-selector assumptions have been backtracked away, so
   asserted-equality merges are not reflected in the batch view. That is a sound
   completeness scope, §3/M3; the richer in-search congruence is the deferred O2 path. See
   logs/lemma-tranche2-log.md.) *)
let e_nested () =
  let s = Session.create () in
  let ctx = Session.context s in
  let f = Session.declare_fun s "f" int_to_int in
  let g = Session.declare_fun s "g" int_to_int in
  let a = Context.const ctx (Session.declare_const s "a" Sort.int) in
  let fga = Context.app ctx f [ Context.app ctx g [ a ] ] in
  ignore
    (Session.assert_lemma
       s
       ~qvars:[ "x", Sort.int ]
       ~build:(fun qv ->
         let x = Qvar.to_term qv.(0) in
         let fgx = Context.app ctx f [ Context.app ctx g [ x ] ] in
         { Session.body = Context.gt ctx fgx (Context.int_const ctx 0)
         ; triggers = [ [ fgx ] ]
         })
     : Session.lemma);
  Session.assert_term s (Context.lt ctx fga (Context.int_const ctx 0));
  let v = Session.check_sat s in
  check
    (Printf.sprintf
       "E-NESTED: nested trigger f(g(x)) closes goal -> unsat (got %s)"
       (verdict_str v))
    (v = Session.Unsat)
;;

(* E-MULTI: forall x y. f(x) = g(y) with conjunctive trigger [{f(x), g(y)}]; ground f(a) =
   u, g(b) = v, u <> v. Both qvars must bind under one substitution; the instance
   f(a)=g(b) then forces u=v, contradiction. Pure EUF. Discrimination: dropping the second
   conjunct leaves y unbound -> no complete instance -> [unknown]. *)
let e_multi () =
  let s = Session.create () in
  let ctx = Session.context s in
  let f = Session.declare_fun s "f" int_to_int in
  let g = Session.declare_fun s "g" int_to_int in
  let a = Context.const ctx (Session.declare_const s "a" Sort.int) in
  let b = Context.const ctx (Session.declare_const s "b" Sort.int) in
  let u = Context.const ctx (Session.declare_const s "u" Sort.int) in
  let v = Context.const ctx (Session.declare_const s "v" Sort.int) in
  let fa = Context.app ctx f [ a ] in
  let gb = Context.app ctx g [ b ] in
  ignore
    (Session.assert_lemma
       s
       ~qvars:[ "x", Sort.int; "y", Sort.int ]
       ~build:(fun qv ->
         let x = Qvar.to_term qv.(0)
         and y = Qvar.to_term qv.(1) in
         let fx = Context.app ctx f [ x ]
         and gy = Context.app ctx g [ y ] in
         { Session.body = Context.eq ctx fx gy; triggers = [ [ fx; gy ] ] })
     : Session.lemma);
  Session.assert_term s (Context.eq ctx fa u);
  Session.assert_term s (Context.eq ctx gb v);
  Session.assert_term s (Context.not_ ctx (Context.eq ctx u v));
  let verdict = Session.check_sat s in
  check
    (Printf.sprintf
       "E-MULTI: conjunctive trigger closes goal -> unsat (got %s)"
       (verdict_str verdict))
    (verdict = Session.Unsat)
;;

(* E-SOUND: the matcher must NOT over-generate a refutation where none exists. forall x.
   f(x) > 0 with trigger f(x); ground f(a) = 5 (consistent with the lemma). The matcher
   generates f(a)>0, consistent, no refutation -> ground sat -> live lemma degrades to
   [unknown]. A matcher that fabricated an unsound instance, or a liveness bug, would flip
   this. (H-SOUND's matcher-driven dual.) *)
let e_sound () =
  let s = Session.create () in
  let ctx = Session.context s in
  let f = Session.declare_fun s "f" int_to_int in
  let a = Context.const ctx (Session.declare_const s "a" Sort.int) in
  ignore
    (Session.assert_lemma
       s
       ~qvars:[ "x", Sort.int ]
       ~build:(fun qv ->
         let x = Qvar.to_term qv.(0) in
         { Session.body =
             Context.gt ctx (Context.app ctx f [ x ]) (Context.int_const ctx 0)
         ; triggers = [ [ Context.app ctx f [ x ] ] ]
         })
     : Session.lemma);
  Session.assert_term
    s
    (Context.eq ctx (Context.app ctx f [ a ]) (Context.int_const ctx 5));
  let v = Session.check_sat s in
  check
    (Printf.sprintf
       "E-SOUND: matcher instance consistent -> unknown (got %s)"
       (verdict_str v))
    (v = Session.Unknown)
;;

(* E-NO-TRIGGER-NO-FIRE: forall x. f(x) > 0 with EMPTY triggers; ground f(a) < 0. Without
   a trigger the matcher generates nothing (auto-selection is tranche 3), so the live
   lemma leaves the sat core unrefuted -> [unknown] (NOT unsat). Confirms the matcher
   fires only on stated triggers, and pairs with E-FIND (same goal, trigger present ->
   unsat). *)
let e_no_trigger_no_fire () =
  let s = Session.create () in
  let ctx = Session.context s in
  let f = Session.declare_fun s "f" int_to_int in
  let a = Context.const ctx (Session.declare_const s "a" Sort.int) in
  ignore
    (Session.assert_lemma
       s
       ~qvars:[ "x", Sort.int ]
       ~build:(fun qv ->
         let x = Qvar.to_term qv.(0) in
         { Session.body =
             Context.gt ctx (Context.app ctx f [ x ]) (Context.int_const ctx 0)
         ; triggers = []
         })
     : Session.lemma);
  Session.assert_term
    s
    (Context.lt ctx (Context.app ctx f [ a ]) (Context.int_const ctx 0));
  let v = Session.check_sat s in
  check
    (Printf.sprintf
       "E-NO-TRIGGER: empty trigger does not fire -> unknown (got %s)"
       (verdict_str v))
    (v = Session.Unknown)
;;

(* E-DET: E-FIND run twice is byte-identical in verdict + instance/round counts (I6). *)
let e_det () =
  let run () =
    let s = Session.create () in
    let ctx = Session.context s in
    let f = Session.declare_fun s "f" int_to_int in
    let a = Context.const ctx (Session.declare_const s "a" Sort.int) in
    ignore
      (Session.assert_lemma
         s
         ~qvars:[ "x", Sort.int ]
         ~build:(fun qv ->
           let x = Qvar.to_term qv.(0) in
           { Session.body =
               Context.gt ctx (Context.app ctx f [ x ]) (Context.int_const ctx 0)
           ; triggers = [ [ Context.app ctx f [ x ] ] ]
           })
       : Session.lemma);
    Session.assert_term
      s
      (Context.lt ctx (Context.app ctx f [ a ]) (Context.int_const ctx 0));
    let v = Session.check_sat s in
    let st = Session.lemma_stats s in
    verdict_str v, st.instances, st.rounds
  in
  check "E-DET: two runs identical" (run () = run ())
;;

(* E-STALE-POP (staleness across backtracking, team-lead req B; assertion tightened per
   the reviewer's delta pass, item 6). A lemma L is asserted in a PUSHED frame with
   trigger f(x); ground f(a)>0 registers f(a). First check: the matcher generates f(a)=5
   under L's frame, L is live -> unknown. Then pop, then assert f(a)=7 — [f(a)=7] alone is
   SAT.

   THE REAL SOUNDNESS GUARD is that instances are asserted guarded by the LEMMA's frame
   selector, which the pop UNASSUMES — so f(a)=5 deactivates with L's frame regardless of
   any store bookkeeping. (L's store retraction in [Manager.on_pop] is
   completeness/hygiene, not the soundness guard: even if the store kept L, the
   deactivated selector makes its clauses inert.) The registered term f(a) survives the
   pop (the e-graph is grow-only), so a regression that guarded the instance by the WRONG
   (base) selector would strand f(a)=5 past the pop and wrong-UNSAT [f(a)=5 & f(a)=7].

   ASSERTION: [v2 = Sat] (NOT the weaker [v2 <> Unsat]). The reviewer proved [<> Unsat] is
   a non-discriminator: disabling the store retraction flips the verdict sat->unknown (L
   stays live -> THE SOUNDNESS RULE degrades), which [<> Unsat] cannot see; the
   wrong-frame mutation instead flips sat->unsat. [= Sat] catches BOTH (unknown != Sat and
   unsat != Sat). Matcher-driven analogue of H-PUSHPOP. *)
let e_stale_pop () =
  let s = Session.create () in
  let ctx = Session.context s in
  let f = Session.declare_fun s "f" int_to_int in
  let a = Context.const ctx (Session.declare_const s "a" Sort.int) in
  let fa = Context.app ctx f [ a ] in
  Session.push s;
  ignore
    (Session.assert_lemma
       s
       ~qvars:[ "x", Sort.int ]
       ~build:(fun qv ->
         let x = Qvar.to_term qv.(0) in
         { Session.body =
             Context.eq ctx (Context.app ctx f [ x ]) (Context.int_const ctx 5)
         ; triggers = [ [ Context.app ctx f [ x ] ] ]
         })
     : Session.lemma);
  Session.assert_term s (Context.gt ctx fa (Context.int_const ctx 0));
  let v1 = Session.check_sat s in
  check
    (Printf.sprintf "E-STALE-POP: pushed live lemma -> unknown (got %s)" (verdict_str v1))
    (v1 = Session.Unknown);
  Session.pop s;
  Session.assert_term s (Context.eq ctx fa (Context.int_const ctx 7));
  let v2 = Session.check_sat s in
  check
    (Printf.sprintf
       "E-STALE-POP: after pop, popped lemma's instance is inert -> sat (got %s)"
       (verdict_str v2))
    (v2 = Session.Sat)
;;

(* U-DEDUP-ROLLBACK (codex MED, dedup pollution): a budget-aborted round must NOT leave
   its never-asserted instances in the dedup cache — the session discards the aborted
   batch without asserting, so a surviving dedup entry would permanently suppress the
   instance on a later round (a missed refutation, spurious Unknown).

   Driven through the SEED path (empty-trigger lemma), NOT a trigger-based lemma: with a
   trigger the matcher debits the budget INSIDE its own enumeration
   ([Matcher.substitutions] raises {!Matcher.Budget_exhausted} before returning), so
   [process] never runs and NOTHING is added to dedup — the rollback branch is vacuous
   (the earlier version of this test never exercised it). An empty-trigger lemma makes the
   matcher a no-op, so the budget is spent INSIDE [process] as the seed queue drains, and
   dedup entries are genuinely added before the abort.

   Setup: gen_budget 3, five distinct seeds s0..s4. Round 1 processes s0,s1,s2 into dedup
   (spending the budget), then aborts popping s3 — rolling back the s0,s1,s2 dedup entries
   (and restoring all consumed seeds). Round 2 (budget reset, same restored seed set) must
   re-attempt those rolled-back instances and re-hit the budget. DISCRIMINATION: with the
   dedup rollback DISABLED, s0,s1,s2 stay dedup-suppressed, so round 2 skips them (no
   budget spent), drains s3,s4 within budget, and does NOT abort — the round-2 abort flag
   flips. *)
let u_dedup_rollback () =
  let sc = scaffold () in
  let f = Env.declare_fun sc.env "f" int_to_int in
  let mk name =
    Context.const sc.ctx (Env.declare_fun sc.env name (Rank.create [] Sort.int))
  in
  (* empty triggers -> the matcher contributes nothing; the seed drain drives [process]. *)
  let lemma =
    make_lemma sc ~id:0 ~arity:1 (fun q ->
      Context.gt sc.ctx (Context.app sc.ctx f [ q.(0) ]) (Context.int_const sc.ctx 0), [])
  in
  let mgr = Manager.create ~gen_budget:3 sc.ctx sc.env in
  Manager.add_lemma mgr lemma;
  List.iter
    (fun i -> Manager.seed_instance mgr lemma [| mk (Printf.sprintf "a%d" i) |])
    [ 0; 1; 2; 3; 4 ];
  Manager.begin_check mgr;
  let r1 = Manager.round mgr Egraph_view.empty in
  (* Round 1 processed s0,s1,s2 INTO dedup (spending the budget) then aborted on s3 — this
     is the non-vacuous setup the earlier trigger-based version failed to reach. *)
  check
    "U-DEDUP-ROLLBACK: round 1 aborted (budget spent inside process)"
    (Manager.budget_exhausted mgr);
  check "U-DEDUP-ROLLBACK: aborted round 1 asserts nothing" (r1 = []);
  Manager.begin_check mgr;
  let r2 = Manager.round mgr Egraph_view.empty in
  (* WITH the dedup rollback, s0,s1,s2 are re-emittable, so round 2 re-processes them and
     re-hits the budget. WITHOUT it they stay suppressed and round 2 drains s3,s4 cleanly
     — so this abort flag is exactly what the rollback buys. *)
  check
    "U-DEDUP-ROLLBACK: round 2 re-aborts on the rolled-back instances (dedup rolled back)"
    (Manager.budget_exhausted mgr);
  check "U-DEDUP-ROLLBACK: aborted round 2 asserts nothing" (r2 = [])
;;

(* U-SEED-ROLLBACK (codex, popped-seeds sibling of dedup pollution): a manual seed
   consumed by a budget-aborted round must NOT be dropped from the queue. Five distinct
   seeds, gen_budget 3: round 1 pops 4 seeds (emits 3, aborts on the 4th) → all consumed
   seeds are restored to the queue. Discrimination via round 2's abort signal: WITH the
   seed-restore all 5 seeds are back, so round 2 re-aborts (budget_exhausted); WITHOUT it
   round 1 dropped the 4 popped seeds, leaving only 1, and round 2 drains it cleanly (no
   abort). The abort flag is observable where the returned batch is not (an aborted round
   returns []). *)
let u_seed_rollback () =
  let sc = scaffold () in
  let f = Env.declare_fun sc.env "f" int_to_int in
  let mk name =
    Context.const sc.ctx (Env.declare_fun sc.env name (Rank.create [] Sort.int))
  in
  let lemma =
    make_lemma sc ~id:0 ~arity:1 (fun q ->
      Context.gt sc.ctx (Context.app sc.ctx f [ q.(0) ]) (Context.int_const sc.ctx 0), [])
  in
  let mgr = Manager.create ~gen_budget:3 sc.ctx sc.env in
  Manager.add_lemma mgr lemma;
  List.iter
    (fun i -> Manager.seed_instance mgr lemma [| mk (Printf.sprintf "a%d" i) |])
    [ 0; 1; 2; 3; 4 ];
  Manager.begin_check mgr;
  let _ = Manager.round mgr Egraph_view.empty in
  check "U-SEED-ROLLBACK: round 1 aborted" (Manager.budget_exhausted mgr);
  Manager.begin_check mgr;
  let _ = Manager.round mgr Egraph_view.empty in
  check
    "U-SEED-ROLLBACK: consumed seeds restored (round 2 re-aborts on the full set)"
    (Manager.budget_exhausted mgr)
;;

(* E-ZERO-QVAR (codex MED, matcher.ml zero-qvar contract): a [forall (). body] lemma is a
   ground fact and must instantiate ONCE. Body p(a) contradicts the ground ¬p(a): the
   empty substitution must be emitted → p(a) asserted → unsat. Discrimination: the pre-fix
   matcher returned [] for a zero-qvar lemma → the fact was never asserted → unknown. *)
let e_zero_qvar () =
  let s = Session.create () in
  let ctx = Session.context s in
  let p = Session.declare_fun s "p" int_to_bool in
  let a = Context.const ctx (Session.declare_const s "a" Sort.int) in
  let pa = Context.app ctx p [ a ] in
  Session.assert_term s (Context.not_ ctx pa);
  ignore
    (Session.assert_lemma s ~qvars:[] ~build:(fun _qv ->
       { Session.body = pa; triggers = [] })
     : Session.lemma);
  let v = Session.check_sat s in
  check
    (Printf.sprintf
       "E-ZERO-QVAR: forall(). p(a) instantiates its body -> unsat (got %s)"
       (verdict_str v))
    (v = Session.Unsat)
;;

(* E-FRAME (codex coverage gap → required): the wrong-lemma-frame future-soundness guard —
   the only e2e that exercises a matcher instance in a NON-BASE frame. Base f(a)<0; a
   lemma forall x. f(x)>0 (trigger f(x)) is asserted in a PUSHED frame; the matcher
   generates f(a)>0 guarded by the pushed frame, closing the goal there (unsat). After
   pop, the lemma AND its instance retract, so f(a)<0 alone no longer refutes (not unsat).
   DISCRIMINATION: a regression that asserted the instance at the BASE frame instead of
   the lemma's frame would leave f(a)>0 alive after the pop and wrong-UNSAT the final
   check — every other e2e uses the base frame and would miss it. The v2 assertion is the
   strong [= Sat] (not [<> Unsat]): [f(a)<0] alone is SAT, so [= Sat] catches both the
   wrong-frame mutation (-> unsat) and any mutation that instead degrades to unknown
   (reviewer's item-6 lesson: [<> Unsat] cannot see a sat->unknown flip). *)
let e_frame () =
  let s = Session.create () in
  let ctx = Session.context s in
  let f = Session.declare_fun s "f" int_to_int in
  let a = Context.const ctx (Session.declare_const s "a" Sort.int) in
  let fa = Context.app ctx f [ a ] in
  Session.assert_term s (Context.lt ctx fa (Context.int_const ctx 0));
  Session.push s;
  ignore
    (Session.assert_lemma
       s
       ~qvars:[ "x", Sort.int ]
       ~build:(fun qv ->
         let x = Qvar.to_term qv.(0) in
         { Session.body =
             Context.gt ctx (Context.app ctx f [ x ]) (Context.int_const ctx 0)
         ; triggers = [ [ Context.app ctx f [ x ] ] ]
         })
     : Session.lemma);
  let v1 = Session.check_sat s in
  check
    (Printf.sprintf
       "E-FRAME: pushed lemma's instance closes goal in frame>=1 -> unsat (got %s)"
       (verdict_str v1))
    (v1 = Session.Unsat);
  Session.pop s;
  let v2 = Session.check_sat s in
  check
    (Printf.sprintf
       "E-FRAME: after pop, instance retracts with its frame -> sat (got %s)"
       (verdict_str v2))
    (v2 = Session.Sat)
;;

(* E-ARITH-TRIGGER-REJECT (codex MED, ADR-0012 L3): assert_lemma must REJECT an
   arith-headed trigger (not silently ignore it). A trigger x+1 (Arith root) is not an
   uninterpreted application → Invalid_argument. Discrimination: the pre-fix session
   accepted it and the matcher silently found no matches → unknown. *)
let e_arith_trigger_reject () =
  let s = Session.create () in
  let ctx = Session.context s in
  let raises_invalid thunk =
    match thunk () with
    | _ -> false
    | exception Invalid_argument _ -> true
  in
  check
    "E-ARITH-TRIGGER-REJECT: arith-headed trigger rejected at assert_lemma"
    (raises_invalid (fun () ->
       Session.assert_lemma
         s
         ~qvars:[ "x", Sort.int ]
         ~build:(fun qv ->
           let x = Qvar.to_term qv.(0) in
           { Session.body = Context.eq ctx x (Context.int_const ctx 0)
           ; triggers = [ [ Context.add ctx x (Context.int_const ctx 1) ] ]
           })))
;;

(* E-LOOP: a matching-loop lemma hits its generation budget and returns [unknown] — it
   never hangs (GOALS Lemmas; ADR-0012 §1.4/§3). [forall x. f(x) = f(g(x))] with trigger
   [f(x)] is a runaway: matching [f(a)] yields [f(a)=f(g(a))], which registers [f(g(a))],
   which matches the SAME trigger -> [f(g(a))=f(g(g(a)))] -> ... forever, with no
   contradiction (the ground core stays sat). With a finite [lemma_gen_budget] the loop
   stops on the budget and THE SOUNDNESS RULE degrades the live-lemma [Sat] to [Unknown].

   Discrimination is two-pronged: (1) the test RETURNS at all — the loop terminated, did
   not hang; (2) a LARGER budget produces STRICTLY MORE instances — the signature of a
   budget-bounded runaway, unlike natural saturation (which would plateau at the same
   count regardless of budget). A build that dropped the live lemma would wrongly report
   [sat] at both budgets. *)
let e_loop () =
  let solve budget =
    let s = Session.create ~lemma_gen_budget:budget () in
    let ctx = Session.context s in
    let f = Session.declare_fun s "f" int_to_int in
    let g = Session.declare_fun s "g" int_to_int in
    let a = Context.const ctx (Session.declare_const s "a" Sort.int) in
    ignore
      (Session.assert_lemma
         s
         ~qvars:[ "x", Sort.int ]
         ~build:(fun qv ->
           let x = Qvar.to_term qv.(0) in
           let fx = Context.app ctx f [ x ] in
           { Session.body =
               Context.eq ctx fx (Context.app ctx f [ Context.app ctx g [ x ] ])
           ; triggers = [ [ fx ] ]
           })
       : Session.lemma);
    (* [f(a) >= 0] registers [f(a)] (the loop seed) and is consistent with the lemma. *)
    Session.assert_term
      s
      (Context.ge ctx (Context.app ctx f [ a ]) (Context.int_const ctx 0));
    let v = Session.check_sat s in
    v, (Session.lemma_stats s).instances
  in
  let v_small, n_small = solve 10 in
  let v_big, n_big = solve 400 in
  check
    (Printf.sprintf
       "E-LOOP: matching loop -> unknown, small budget (got %s)"
       (verdict_str v_small))
    (v_small = Session.Unknown);
  check
    (Printf.sprintf
       "E-LOOP: matching loop -> unknown, large budget (got %s)"
       (verdict_str v_big))
    (v_big = Session.Unknown);
  (* A larger budget yields STRICTLY MORE instances — the signature of a budget-bounded
     runaway, not natural saturation (which would plateau regardless of budget). *)
  check
    (Printf.sprintf "E-LOOP: budget-bounded, not saturation (%d < %d)" n_small n_big)
    (n_small < n_big)
;;

(* E-PROVENANCE: every generated instance is recorded with its source lemma id and
   substitution (GOALS Lemmas). Reuses the E-FIND goal (one instance, x|->a): the trace
   must hold exactly that one instantiation, tagged with lemma id 0 (the session's first
   lemma) and [subst = [| a |]]. Discrimination: a manager that generated the instance but
   kept no record would leave the trace empty. *)
let e_provenance () =
  let s = Session.create () in
  let ctx = Session.context s in
  let f = Session.declare_fun s "f" int_to_int in
  let a = Context.const ctx (Session.declare_const s "a" Sort.int) in
  ignore
    (Session.assert_lemma
       s
       ~qvars:[ "x", Sort.int ]
       ~build:(fun qv ->
         let x = Qvar.to_term qv.(0) in
         { Session.body =
             Context.gt ctx (Context.app ctx f [ x ]) (Context.int_const ctx 0)
         ; triggers = [ [ Context.app ctx f [ x ] ] ]
         })
     : Session.lemma);
  Session.assert_term
    s
    (Context.lt ctx (Context.app ctx f [ a ]) (Context.int_const ctx 0));
  ignore (Session.check_sat s : Session.verdict);
  match Session.lemma_instantiations s with
  | [ inst ] ->
    check "E-PROVENANCE: one recorded instantiation" true;
    check "E-PROVENANCE: tagged with source lemma id 0" (inst.Session.lemma_id = 0);
    check
      "E-PROVENANCE: substitution binds x|->a"
      (Array.length inst.Session.subst = 1 && Term.equal inst.Session.subst.(0) a)
  | other ->
    check
      (Printf.sprintf
         "E-PROVENANCE: expected exactly one record (got %d)"
         (List.length other))
      false
;;

(* ================================================================== *)
(* TRIGGER INFERENCE — Trigger.infer over hand-built lemma bodies. *)
(* ================================================================== *)

let mk_qvars sc ~id n =
  Array.init n (fun k -> Qvar.mint sc.cap sc.env sc.ctx ~lemma_id:id ~index:k Sort.int)
;;

(* One conjunctive multi-trigger, as a Term set (order-independent Term.equal membership). *)
let trigger_is triggers expected =
  match triggers with
  | [ conj ] ->
    List.length conj = List.length expected
    && List.for_all (fun e -> List.exists (Term.equal e) conj) expected
  | _ -> false
;;

(* TI-SINGLE: body f(x) > 0 -> trigger f(x) (the only UF app covering x). *)
let ti_single () =
  let sc = scaffold () in
  let f = Env.declare_fun sc.env "f" int_to_int in
  let qv = mk_qvars sc ~id:0 1 in
  let x = Qvar.to_term qv.(0) in
  let fx = Context.app sc.ctx f [ x ] in
  let body = Context.gt sc.ctx fx (Context.int_const sc.ctx 0) in
  check "TI-SINGLE: infers [f(x)]" (trigger_is (Trigger.infer ~qvars:qv body) [ fx ])
;;

(* TI-NESTED: body f(g(x)) > 0 -> the SMALLEST covering subterm g(x), not f(g(x)). *)
let ti_nested () =
  let sc = scaffold () in
  let f = Env.declare_fun sc.env "f" int_to_int in
  let g = Env.declare_fun sc.env "g" int_to_int in
  let qv = mk_qvars sc ~id:0 1 in
  let x = Qvar.to_term qv.(0) in
  let gx = Context.app sc.ctx g [ x ] in
  let body =
    Context.gt sc.ctx (Context.app sc.ctx f [ gx ]) (Context.int_const sc.ctx 0)
  in
  check
    "TI-NESTED: infers smallest [g(x)] not [f(g(x))]"
    (trigger_is (Trigger.infer ~qvars:qv body) [ gx ])
;;

(* TI-MULTI: body f(x) = g(y) -> conjunctive trigger {f(x), g(y)} covering both qvars. *)
let ti_multi () =
  let sc = scaffold () in
  let f = Env.declare_fun sc.env "f" int_to_int in
  let g = Env.declare_fun sc.env "g" int_to_int in
  let qv = mk_qvars sc ~id:0 2 in
  let x = Qvar.to_term qv.(0)
  and y = Qvar.to_term qv.(1) in
  let fx = Context.app sc.ctx f [ x ]
  and gy = Context.app sc.ctx g [ y ] in
  let body = Context.eq sc.ctx fx gy in
  check
    "TI-MULTI: infers {f(x), g(y)}"
    (trigger_is (Trigger.infer ~qvars:qv body) [ fx; gy ])
;;

(* TI-UNREACHABLE: body x + 1 <= 0 — x occurs only inside arithmetic, no UF app covers it,
   so no trigger is inferable ([]). This is the soundness-preserving no-fire case: the
   lemma stays live and a ground Sat degrades to unknown, never a dropped forall. *)
let ti_unreachable () =
  let sc = scaffold () in
  let qv = mk_qvars sc ~id:0 1 in
  let x = Qvar.to_term qv.(0) in
  let body =
    Context.le
      sc.ctx
      (Context.add sc.ctx x (Context.int_const sc.ctx 1))
      (Context.int_const sc.ctx 0)
  in
  check
    "TI-UNREACHABLE: qvar only in arithmetic -> no trigger"
    (Trigger.infer ~qvars:qv body = [])
;;

(* TI-ZERO: a zero-qvar body needs no trigger (the matcher fires it once regardless). *)
let ti_zero () =
  let sc = scaffold () in
  let p = Env.declare_fun sc.env "p" (Rank.create [] Sort.bool) in
  check
    "TI-ZERO: zero-qvar -> no trigger"
    (Trigger.infer ~qvars:[||] (Context.const sc.ctx p) = [])
;;

let () =
  ignore int_int_to_int;
  ignore int_to_bool;
  u_find ();
  u_congruence ();
  u_multi ();
  u_empty_trigger ();
  u_cap ();
  u_det ();
  u_manager_cap ();
  u_dedup_rollback ();
  u_seed_rollback ();
  e_find ();
  e_nested ();
  e_multi ();
  e_sound ();
  e_no_trigger_no_fire ();
  e_det ();
  e_stale_pop ();
  e_zero_qvar ();
  e_frame ();
  e_arith_trigger_reject ();
  e_loop ();
  e_provenance ();
  ti_single ();
  ti_nested ();
  ti_multi ();
  ti_unreachable ();
  ti_zero ();
  Printf.printf "\n%d passed, %d failed\n" !passes !failures;
  if !failures > 0 then exit 1
;;

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
               | Int_const n -> Printf.sprintf "#%d" n
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
  e_find ();
  e_nested ();
  e_multi ();
  e_sound ();
  e_no_trigger_no_fire ();
  e_det ();
  Printf.printf "\n%d passed, %d failed\n" !passes !failures;
  if !failures > 0 then exit 1
;;

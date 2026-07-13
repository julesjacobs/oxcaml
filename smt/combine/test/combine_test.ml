(* Unit + integration tests for smt/combine (Nelson–Oppen model-based combination).

   Three layers, all deterministic (INVARIANTS.md I6):

   1. MECHANICS — a programmable MOCK theory (records every seam call; returns
      test-scripted check results / models / explanations) drives the multiplexer through
      routing, push/pop lockstep, propagation merge + explanation routing, model-based
      Sat-vs-Split decisions, sort-directed model merge, and the CONTRACT-POISON
      degradation ({!Combine.Combination_unsound}) incl. its recheck-after-backtrack
      unwind.

   2. INTEGRATION — hand-rolled TOY EUF (a naive congruence closure) and TOY LIA (a
      brute-force bounded-integer procedure), written from spec alone (they share no code
      with the real engines), combined through {!Uflia_router} and driven by a mini
      DPLL(T) loop that stands in for the CDCL(T) seam. Canonical QF_UFLIA goals exercise
      the whole N-O flow against an independent oracle.

   3. THE W1 GATE — the SAME functorized driver bound to the REAL {!Oxsmt_euf.Euf_adapter}
      + {!Oxsmt_lia.Lia_adapter} (no mocks): the master's empirical acceptance criterion
        that x=y ∧ f(x)<f(y) and the nested tower x=y ∧ g(f(x))<g(f(y)) come back UNSAT
        end-to-end (Part 3). The layer-2 driver was made a functor precisely so layer 3
        binds the real engines by swapping the two functor arguments. *)

open Oxsmt_core
module Th = Theory
module Cmb = Oxsmt_combine.Combine

(* Stage-1b: the hand-rolled children are frozen [THEORY]s; they satisfy the richer
   [FABRIC_CHILD]/[FABRIC_CONGRUENCE_CHILD] seam by wrapping their own [check]/[explain]
   (so recorded seam-call sequences are unchanged) and reporting no fixed bounds (so the
   fix-trigger never injects through a toy — the real fabric path is exercised by the
   real-adapter layer). *)
let fabric_of_expl (e : Explanation.t) : Cmb.Fabric_explanation.t =
  { Cmb.Fabric_explanation.premises =
      List.map (fun l -> Cmb.Real l) e.Explanation.premises
  ; rule = e.Explanation.rule
  }
;;

let fabric_of_check (r : Th.check_result) : Cmb.fabric_check_result =
  match r with
  | Th.Sat -> Cmb.Sat
  | Th.Propagations l -> Cmb.Propagations l
  | Th.Conflict e -> Cmb.Conflict (fabric_of_expl e)
  | Th.Split ts -> Cmb.Split ts
;;

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

(* ---- term-building fixtures ------------------------------------------------------- *)

type fixture =
  { env : Env.t
  ; ctx : Context.t
  ; alloc : Atom.allocator
  }

let fixture () =
  let env = Env.create () in
  { env; ctx = Context.create env; alloc = Atom.create_allocator () }
;;

let const f name =
  Context.const f.ctx (Env.declare_fun f.env name (Rank.create [] Sort.int))
;;

let ufun f name = Env.declare_fun f.env name (Rank.create [ Sort.int ] Sort.int)
let fresh_atom f = Atom.fresh f.alloc

(* ================================================================================== *)
(* Part 1 — MECHANICS: a programmable mock theory + a controllable router. *)
(* ================================================================================== *)

type mock_event =
  | E_create of string
  | E_register of string * Atom.t * Term.t
  | E_internalize of string * Term.t
  | E_assert of string * Lit.t
  | E_check of string * Th.effort
  | E_explain of string * Lit.t
  | E_push of string
  | E_pop of string * int
  | E_model of string

let mlog : mock_event list ref = ref []
let record e = mlog := e :: !mlog
let events () = List.rev !mlog
let clear_log () = mlog := []

let default_check : Th.effort -> Th.check_result = function
  | Th.Propagate -> Th.Propagations []
  | Th.Final -> Th.Sat
;;

let trivial_expl = { Explanation.premises = []; rule = Explanation.Rule_tag.Trivial }

(* One mock instance's scripted behaviour. Module-level (a single instance per Combine),
   reset between tests via [reset]. *)
module Make_mock (Tag : sig
    val tag : string
  end) =
struct
  let check_fn = ref default_check
  let model_fn = ref (fun () : (Term.t * Model.value) list -> [])
  let explain_fn = ref (fun (_ : Lit.t) -> trivial_expl)

  let reset () =
    check_fn := default_check;
    (model_fn := fun () -> []);
    explain_fn := fun _ -> trivial_expl
  ;;

  type t = unit

  let create _ctx _env = record (E_create Tag.tag)
  let register_atom () a term = record (E_register (Tag.tag, a, term))
  let internalize_term () term = record (E_internalize (Tag.tag, term))
  let assert_lit () l = record (E_assert (Tag.tag, l))

  let check () eff =
    record (E_check (Tag.tag, eff));
    !check_fn eff
  ;;

  let explain () l =
    record (E_explain (Tag.tag, l));
    !explain_fn l
  ;;

  let push () = record (E_push Tag.tag)
  let pop () n = record (E_pop (Tag.tag, n))

  let model () =
    record (E_model Tag.tag);
    Model.of_alist (!model_fn ())
  ;;

  (* FABRIC seam: delegate to the scripted [check]/[explain] (so recording is unchanged);
     a mock fixes no bounds and merges no fabric edge. *)
  let check_fabric () eff = fabric_of_check (check () eff)
  let explain_fabric () l = fabric_of_expl (explain () l)
  let fixed_bounds () _ = None
  let fabric_verify () _ _ _ _ = false
  let fabric_are_equal () _ _ = false
  let assert_fabric_eq () ~edge_id:_ _ _ = ()
end

module MockA = Make_mock (struct
    let tag = "A"
  end)

module MockB = Make_mock (struct
    let tag = "B"
  end)

(* A router whose ownership is set explicitly per test. (The combinator derives the
   disagreement-comparison domain from the child models, so no interface-term hints.) *)
module Ctrl_router = struct
  type owner =
    | A
    | B
    | Both

  let owner_tbl : owner Term.Table.t = Term.Table.create 16
  let reset () = Term.Table.reset owner_tbl
  let set_owner term o = Term.Table.replace owner_tbl term o

  let owner term =
    match Term.Table.find_opt owner_tbl term with
    | Some o -> o
    | None -> A
  ;;

  (* mirror Uflia's polarity rule: a negative Both routes to A only *)
  let assert_to term ~positive =
    match owner term with
    | Both when not positive -> A
    | o -> o
  ;;

  let equality_split ctx x y =
    [ Context.eq ctx x y; Context.lt ctx x y; Context.gt ctx x y ]
  ;;
end

module Cmock = Cmb.Combine (Ctrl_router) (MockA) (MockB)

let reset_mocks () =
  clear_log ();
  MockA.reset ();
  MockB.reset ();
  Ctrl_router.reset ()
;;

let saw_register tag atom =
  List.exists
    (function
      | E_register (t, a, _) -> String.equal t tag && Atom.equal a atom
      | _ -> false)
    (events ())
;;

let saw_internalize tag term =
  List.exists
    (function
      | E_internalize (t, tm) -> String.equal t tag && Term.equal tm term
      | _ -> false)
    (events ())
;;

let saw_assert tag lit =
  List.exists
    (function
      | E_assert (t, l) -> String.equal t tag && Lit.equal l lit
      | _ -> false)
    (events ())
;;

let saw_check tag =
  List.exists
    (function
      | E_check (t, _) -> String.equal t tag
      | _ -> false)
    (events ())
;;

let saw_explain tag lit =
  List.exists
    (function
      | E_explain (t, l) -> String.equal t tag && Lit.equal l lit
      | _ -> false)
    (events ())
;;

let count_push tag =
  List.length
    (List.filter
       (function
         | E_push t -> String.equal t tag
         | _ -> false)
       (events ()))
;;

let saw_pop tag n =
  List.exists
    (function
      | E_pop (t, k) -> String.equal t tag && k = n
      | _ -> false)
    (events ())
;;

let test_routing () =
  reset_mocks ();
  let f = fixture () in
  let x = const f "x"
  and y = const f "y" in
  let ff = ufun f "f_route" in
  let fx = Context.app f.ctx ff [ x ] in
  let ta = Context.eq f.ctx (const f "u") (const f "v") in
  let tb = Context.le f.ctx x y in
  (* a B-owned atom that DOES carry an uninterpreted application [f x] *)
  let tb_uf = Context.le f.ctx fx y in
  let tboth = Context.eq f.ctx x y in
  Ctrl_router.set_owner ta Ctrl_router.A;
  Ctrl_router.set_owner tb Ctrl_router.B;
  Ctrl_router.set_owner tb_uf Ctrl_router.B;
  Ctrl_router.set_owner tboth Ctrl_router.Both;
  let t = Cmock.create f.ctx f.env in
  let aa = fresh_atom f
  and ab = fresh_atom f
  and ab_uf = fresh_atom f
  and aboth = fresh_atom f in
  Cmock.register_atom t aa ta;
  Cmock.register_atom t ab tb;
  Cmock.register_atom t ab_uf tb_uf;
  Cmock.register_atom t aboth tboth;
  let la = Lit.make aa true
  and lb = Lit.make ab true
  and lboth_pos = Lit.make aboth true
  and lboth_neg = Lit.make aboth false in
  Cmock.assert_lit t la;
  Cmock.assert_lit t lb;
  Cmock.assert_lit t lboth_pos;
  Cmock.assert_lit t lboth_neg;
  check
    "route: A-atom registered only to A"
    (saw_register "A" aa && not (saw_register "B" aa));
  (* MEMBERSHIP RULE (DESIGN A4 erratum): a pure-LIA B-atom [x ≤ y] has NO uninterpreted
     application, so it internalizes NOTHING into the congruence child — no arith-closure
     e-nodes; the "UF-free skip" as the empty instance of the rule. *)
  check
    "route: pure-LIA B-atom registered only to B, internalizes NOTHING into A"
    (saw_register "B" ab
     && (not (saw_register "A" ab))
     && (not (saw_internalize "A" tb))
     && not (saw_internalize "A" x));
  (* but a B-atom carrying an uninterpreted application [f x ≤ y] internalizes exactly
     that application into A (clauses (i)+(ii)) — the W1 boundary node stays visible to
     EUF. *)
  check
    "route: B-atom with an uninterpreted application internalizes [f x] into A"
    (saw_register "B" ab_uf
     && (not (saw_register "A" ab_uf))
     && saw_internalize "A" fx
     && not (saw_internalize "A" tb_uf));
  check
    "route: Both-atom registered to A and B (union, polarity-blind)"
    (saw_register "A" aboth && saw_register "B" aboth);
  check "route: A-lit asserted only to A" (saw_assert "A" la && not (saw_assert "B" la));
  check "route: B-lit asserted only to B" (saw_assert "B" lb && not (saw_assert "A" lb));
  check
    "route: POSITIVE Both-lit asserted to A and B"
    (saw_assert "A" lboth_pos && saw_assert "B" lboth_pos);
  check
    "route: NEGATIVE Both-lit asserted to A ONLY (S1: LIA can't take a diseq)"
    (saw_assert "A" lboth_neg && not (saw_assert "B" lboth_neg))
;;

let test_push_pop_lockstep () =
  reset_mocks ();
  let f = fixture () in
  let t = Cmock.create f.ctx f.env in
  Cmock.push t;
  Cmock.push t;
  Cmock.pop t 2;
  check "push forwarded to both children" (count_push "A" = 2 && count_push "B" = 2);
  check "pop n forwarded to both children" (saw_pop "A" 2 && saw_pop "B" 2)
;;

let test_propagate_merge () =
  reset_mocks ();
  let f = fixture () in
  let t = Cmock.create f.ctx f.env in
  let a1 = fresh_atom f
  and a2 = fresh_atom f in
  let l1 = Lit.make a1 true
  and l2 = Lit.make a2 true in
  (MockA.check_fn
   := function
      | Th.Propagate -> Th.Propagations [ l1 ]
      | Th.Final -> Th.Sat);
  (MockB.check_fn
   := function
      | Th.Propagate -> Th.Propagations [ l2 ]
      | Th.Final -> Th.Sat);
  match Cmock.check t Th.Propagate with
  | Th.Propagations [ p1; p2 ] ->
    check "propagate: merged in A-then-B order" (Lit.equal p1 l1 && Lit.equal p2 l2)
  | _ -> check "propagate: merged in A-then-B order" false
;;

let test_propagate_conflict_shortcircuits_b () =
  reset_mocks ();
  let f = fixture () in
  let t = Cmock.create f.ctx f.env in
  let e = { Explanation.premises = []; rule = Explanation.Rule_tag.Lia_farkas } in
  (MockA.check_fn := fun _ -> Th.Conflict e);
  (MockB.check_fn := fun _ -> Th.Propagations []);
  clear_log ();
  (match Cmock.check t Th.Propagate with
   | Th.Conflict _ -> check "propagate: A-conflict returned verbatim" true
   | _ -> check "propagate: A-conflict returned verbatim" false);
  check "propagate: B not consulted after A conflict" (not (saw_check "B"))
;;

let test_explain_routing () =
  reset_mocks ();
  let f = fixture () in
  let t = Cmock.create f.ctx f.env in
  let a = fresh_atom f in
  let term = Context.eq f.ctx (const f "u") (const f "v") in
  Ctrl_router.set_owner term Ctrl_router.A;
  Cmock.register_atom t a term;
  let l = Lit.make a true in
  let expl =
    { Explanation.premises = [ l ]; rule = Explanation.Rule_tag.Euf_congruence }
  in
  (MockA.check_fn
   := function
      | Th.Propagate -> Th.Propagations [ l ]
      | Th.Final -> Th.Sat);
  (MockA.explain_fn := fun _ -> expl);
  ignore (Cmock.check t Th.Propagate);
  let got = Cmock.explain t l in
  check "explain: routed to the A owner" (saw_explain "A" l);
  check
    "explain: premises/rule passed through unchanged"
    (List.length got.Explanation.premises = 1
     &&
     match got.Explanation.rule with
     | Euf_congruence -> true
     | _ -> false)
;;

(* Make [x] and [y] genuine INTERFACE members so the disagreement search compares them.
   Under internalization the interface is STRUCTURAL, not model-derived: a bare variable
   is shared only when used as an operand by both an EUF-owned and a LIA-owned node (§3.1
   both-used clause). So register an EUF atom using x,y under an uninterpreted [g] (→ they
   pick up the EUF use bit) and a LIA atom using them inside a sum (→ the LIA use bit).
   Both bits set ⇒ x,y enter the interface. (g(x),g(y) also enter as equality-side
   boundary nodes, but the mock models below do not value them, so the both-valued skip
   drops them and only x,y are compared.) *)
let setup_shared f t x y =
  let g = Env.declare_fun f.env "g_iface" (Rank.create [ Sort.int ] Sort.int) in
  let euf_atom =
    Context.eq f.ctx (Context.app f.ctx g [ x ]) (Context.app f.ctx g [ y ])
  in
  let lia_atom = Context.le f.ctx (Context.add f.ctx x y) (Context.int_const f.ctx 0) in
  Ctrl_router.set_owner euf_atom Ctrl_router.A;
  Ctrl_router.set_owner lia_atom Ctrl_router.B;
  Cmock.register_atom t (fresh_atom f) euf_atom;
  Cmock.register_atom t (fresh_atom f) lia_atom
;;

let test_final_agree_sat () =
  reset_mocks ();
  let f = fixture () in
  let x = const f "x"
  and y = const f "y" in
  let t = Cmock.create f.ctx f.env in
  setup_shared f t x y;
  (MockA.model_fn := fun () -> [ x, Model.Int 1; y, Model.Int 1 ]);
  (MockB.model_fn := fun () -> [ x, Model.Int 5; y, Model.Int 5 ]);
  match Cmock.check t Th.Final with
  | Th.Sat -> check "final: agreeing arrangements (differing values) ⇒ Sat" true
  | _ -> check "final: agreeing arrangements (differing values) ⇒ Sat" false
;;

let test_final_disagree_split () =
  reset_mocks ();
  let f = fixture () in
  let x = const f "x"
  and y = const f "y" in
  let t = Cmock.create f.ctx f.env in
  setup_shared f t x y;
  (MockA.model_fn := fun () -> [ x, Model.Int 1; y, Model.Int 1 ]);
  (MockB.model_fn := fun () -> [ x, Model.Int 1; y, Model.Int 2 ]);
  match Cmock.check t Th.Final with
  | Th.Split terms ->
    let distinct =
      match terms with
      | [ a; b; c ] ->
        (not (Term.equal a b)) && (not (Term.equal a c)) && not (Term.equal b c)
      | _ -> false
    in
    check "final: disagreement ⇒ Split" true;
    check "final: split is 3 DISTINCT atoms (ℤ-trichotomy, not A∨¬A)" distinct;
    check
      "final: split's first atom is the equality x=y"
      (match terms with
       | a :: _ -> Term.equal a (Context.eq f.ctx x y)
       | [] -> false)
  | _ ->
    check "final: disagreement ⇒ Split" false;
    check "final: split is 3 DISTINCT atoms (ℤ-trichotomy, not A∨¬A)" false;
    check "final: split's first atom is the equality x=y" false
;;

let test_model_merge_sort_directed () =
  reset_mocks ();
  let f = fixture () in
  let x = const f "x"
  and y = const f "y" in
  let t = Cmock.create f.ctx f.env in
  setup_shared f t x y;
  (MockA.model_fn := fun () -> [ x, Model.Uninterp 7; y, Model.Uninterp 7 ]);
  (MockB.model_fn := fun () -> [ x, Model.Int 42; y, Model.Int 42 ]);
  (match Cmock.check t Th.Final with
   | Th.Sat -> ()
   | _ -> ());
  let m = Cmock.model t in
  check
    "merge: Int-sorted term takes the arithmetic child's Int value"
    (match Model.value m x with
     | Some (Model.Int 42) -> true
     | _ -> false)
;;

let test_poison_on_pinned_disagreement () =
  reset_mocks ();
  let f = fixture () in
  let x = const f "x"
  and y = const f "y" in
  let t = Cmock.create f.ctx f.env in
  let eq_atom = Context.eq f.ctx x y in
  Ctrl_router.set_owner eq_atom Ctrl_router.Both;
  let a = fresh_atom f in
  Cmock.register_atom t a eq_atom;
  Cmock.assert_lit t (Lit.make a true);
  (MockA.model_fn := fun () -> [ x, Model.Int 1; y, Model.Int 1 ]);
  (MockB.model_fn := fun () -> [ x, Model.Int 1; y, Model.Int 2 ]);
  let raised =
    try
      ignore (Cmock.check t Th.Final);
      false
    with
    | Cmb.Combination_unsound _ -> true
  in
  check "poison: disagreement on a PINNED equality raises Combination_unsound" raised
;;

let test_pin_unwinds_on_pop () =
  reset_mocks ();
  let f = fixture () in
  let x = const f "x"
  and y = const f "y" in
  let ff = ufun f "f" in
  let fx = Context.app f.ctx ff [ x ]
  and fy = Context.app f.ctx ff [ y ] in
  let t = Cmock.create f.ctx f.env in
  (* the shared equality is over App terms, so its sides fx,fy are structural interface
     members (equality-side boundary nodes) — the disagreement search can compare them *)
  let eq_atom = Context.eq f.ctx fx fy in
  Ctrl_router.set_owner eq_atom Ctrl_router.Both;
  let a = fresh_atom f in
  Cmock.register_atom t a eq_atom;
  (MockA.model_fn := fun () -> [ fx, Model.Int 1; fy, Model.Int 1 ]);
  (MockB.model_fn := fun () -> [ fx, Model.Int 1; fy, Model.Int 2 ]);
  Cmock.push t;
  Cmock.assert_lit t (Lit.make a true);
  Cmock.pop t 1;
  match Cmock.check t Th.Final with
  | Th.Split _ ->
    check "backtrack: popped pin re-enables the split (no false poison)" true
  | _ -> check "backtrack: popped pin re-enables the split (no false poison)" false
;;

(* codex C1 — provenance keyed on the SIGNED literal: A propagates [+e], B propagates [-e]
   over the SAME atom; explain of each must reach the child that produced THAT signed
   literal (keying on the atom alone would let B's [-e] clobber A's [+e]). *)
let test_provenance_signed_lit () =
  reset_mocks ();
  let f = fixture () in
  let x = const f "x"
  and y = const f "y" in
  let e = Context.eq f.ctx x y in
  Ctrl_router.set_owner e Ctrl_router.Both;
  let t = Cmock.create f.ctx f.env in
  let a = fresh_atom f in
  Cmock.register_atom t a e;
  let pos = Lit.make a true
  and neg = Lit.make a false in
  (MockA.check_fn
   := function
      | Th.Propagate -> Th.Propagations [ pos ]
      | Th.Final -> Th.Sat);
  (MockB.check_fn
   := function
      | Th.Propagate -> Th.Propagations [ neg ]
      | Th.Final -> Th.Sat);
  (MockA.explain_fn := fun _ -> { Explanation.premises = [ pos ]; rule = Euf_congruence });
  (MockB.explain_fn := fun _ -> { Explanation.premises = [ neg ]; rule = Lia_bound });
  ignore (Cmock.check t Th.Propagate);
  clear_log ();
  let ea = Cmock.explain t pos in
  let eb = Cmock.explain t neg in
  check
    "C1: explain(+e) routes to A (provenance keyed on signed lit)"
    (saw_explain "A" pos
     &&
     match ea.Explanation.rule with
     | Euf_congruence -> true
     | _ -> false);
  check
    "C1: explain(-e) routes to B (not clobbered by same atom)"
    (saw_explain "B" neg
     &&
     match eb.Explanation.rule with
     | Lia_bound -> true
     | _ -> false)
;;

(* codex C2 — pinned equalities carry their sign and are verified against EACH routed
   child's model. A positive equality routed to both: a child whose Sat-model violates it
   → degrade. A negative equality routed to EUF only: the arithmetic child is NOT required
   to satisfy it, so its differing model is a disagreement to SPLIT, not a false poison. *)
let test_pin_satisfaction () =
  reset_mocks ();
  let f = fixture () in
  let x = const f "x"
  and y = const f "y" in
  let e = Context.eq f.ctx x y in
  Ctrl_router.set_owner e Ctrl_router.Both;
  let t = Cmock.create f.ctx f.env in
  let a = fresh_atom f in
  Cmock.register_atom t a e;
  Cmock.assert_lit t (Lit.make a true);
  (* +e to both *)
  (MockA.model_fn := fun () -> [ x, Model.Int 1; y, Model.Int 1 ]);
  (* A: x=y ✓ *)
  (MockB.model_fn := fun () -> [ x, Model.Int 1; y, Model.Int 2 ]);
  (* B: x≠y ✗ *)
  let raised =
    try
      ignore (Cmock.check t Th.Final);
      false
    with
    | Cmb.Combination_unsound _ -> true
  in
  check "C2: a child model violating a +pinned equality → Combination_unsound" raised;
  reset_mocks ();
  let f = fixture () in
  let x = const f "x"
  and y = const f "y" in
  let ff = ufun f "f" in
  let fx = Context.app f.ctx ff [ x ]
  and fy = Context.app f.ctx ff [ y ] in
  (* App-term sides so fx,fy are structural interface members *)
  let e = Context.eq f.ctx fx fy in
  Ctrl_router.set_owner e Ctrl_router.Both;
  let t = Cmock.create f.ctx f.env in
  let a = fresh_atom f in
  Cmock.register_atom t a e;
  Cmock.assert_lit t (Lit.make a false);
  (* -e → A only *)
  (MockA.model_fn := fun () -> [ fx, Model.Int 1; fy, Model.Int 2 ]);
  (* A: fx≠fy ✓ satisfies -e *)
  (MockB.model_fn := fun () -> [ fx, Model.Int 5; fy, Model.Int 5 ]);
  (* B: fx=fy, but B wasn't told -e *)
  match Cmock.check t Th.Final with
  | Th.Split _ ->
    check
      "C2: -eq to A only; B's differing model is a disagreement → Split (no false poison)"
      true
  | _ ->
    check
      "C2: -eq to A only; B's differing model is a disagreement → Split (no false poison)"
      false
;;

(* codex C3 — the merged model is sort-correct over ALL subterms. An Int-sorted term takes
   the arithmetic child's Int value (not the congruence child's opaque class); a
   uninterpreted-sorted subterm is INCLUDED (as [Uninterp]); an Int term valued only as an
   opaque class is unresolvable → model degrades rather than emit a sort-wrong witness. *)
let test_model_domain_and_sort () =
  reset_mocks ();
  let f = fixture () in
  let u_sort = Sort.uninterpreted (Env.declare_sort f.env "U") in
  let p = Context.const f.ctx (Env.declare_fun f.env "p" (Rank.create [] u_sort)) in
  let g = Env.declare_fun f.env "g" (Rank.create [ u_sort ] Sort.int) in
  let gp = Context.app f.ctx g [ p ] in
  let c = const f "c" in
  let e = Context.eq f.ctx gp c in
  Ctrl_router.set_owner e Ctrl_router.Both;
  let t = Cmock.create f.ctx f.env in
  let a = fresh_atom f in
  Cmock.register_atom t a e;
  (MockA.model_fn
   := fun () -> [ gp, Model.Uninterp 3; c, Model.Uninterp 3; p, Model.Uninterp 9 ]);
  (MockB.model_fn := fun () -> [ gp, Model.Int 7; c, Model.Int 7 ]);
  (match Cmock.check t Th.Final with
   | Th.Sat -> ()
   | _ -> ());
  let m = Cmock.model t in
  check
    "C3: Int-sorted g(p) takes the Int-variant (LIA), not EUF's opaque class"
    (match Model.value m gp with
     | Some (Model.Int 7) -> true
     | _ -> false);
  check
    "C3: uninterpreted-sorted p is included in the witness as Uninterp"
    (match Model.value m p with
     | Some (Model.Uninterp 9) -> true
     | _ -> false);
  (* round-2 landmine + §10 realization (task #110): a pure-EUF Int term (valued only as
     an opaque class by the congruence child, unseen by the arithmetic child) is the
     DEFINING QF_UFLIA shape, not an error. model() must NOT raise. It used to OMIT the
     term; the §10 ℤ-realization now SURFACES its EUF class here as [Uninterp cid] — the
     extraction-layer "realize me" signal read only by the Int arm of {!Cdclt.model}'s
     [value_of], which turns it into a concrete integer. The term still never gets a
     non-Int VALUE in the shipped table (Cdclt realizes it), and model() still never
     raises on the opaque-class shape. *)
  reset_mocks ();
  let f = fixture () in
  let x = const f "x" in
  let e = Context.eq f.ctx x (const f "d") in
  Ctrl_router.set_owner e Ctrl_router.A;
  let t = Cmock.create f.ctx f.env in
  let a = fresh_atom f in
  Cmock.register_atom t a e;
  (MockA.model_fn := fun () -> [ x, Model.Uninterp 5 ]);
  (MockB.model_fn := fun () -> []);
  (match Cmock.check t Th.Final with
   | Th.Sat -> ()
   | _ -> ());
  let m =
    try Some (Cmock.model t) with
    | Cmb.Combination_unsound _ -> None
  in
  check
    "C3: pure-EUF Int term (opaque class only) → model surfaces its EUF class (§10 \
     realize signal), never raises"
    (match m with
     | Some m -> Model.value m x = Some (Model.Uninterp 5)
     | None -> false)
;;

(* codex C4 — a child returning empty propagations at Final is NOT a Sat certificate; the
   combinator must not launder it into Sat (consume models only after both Final→Sat). *)
let test_final_requires_sat () =
  reset_mocks ();
  let f = fixture () in
  let t = Cmock.create f.ctx f.env in
  (MockA.check_fn := fun _ -> Th.Sat);
  (MockB.check_fn := fun _ -> Th.Propagations []);
  let raised =
    try
      ignore (Cmock.check t Th.Final);
      false
    with
    | Cmb.Combination_unsound _ -> true
  in
  check "C4: empty propagations at Final is not a Sat certificate → degrade" raised
;;

(* codex W1 (CRITICAL wrong-SAT with the real adapters) — find_disagreement's domain must
   be the terms BOTH models VALUE, not a syntactic seen-set intersection. Here f(x),f(y)
   appear only in a LIA-owned atom, so a seen-set would attribute them to LIA alone; but
   EUF congruence-closes f(x)=f(y) from x=y and so its MODEL values them (equal), while
   LIA's model has f(x)<f(y) (unequal). The models disagree on (f(x),f(y)); a seen∩ domain
   misses it → certifies a theory-inconsistent Sat. The model-value domain catches it →
   Split. (A mock encodes the property the toy children cannot: EUF valuing a congruence
   equality on terms that are syntactically only in a LIA atom.) *)
let test_disagreement_domain_is_model_valued () =
  reset_mocks ();
  let f = fixture () in
  let x = const f "x"
  and y = const f "y" in
  let ff = ufun f "f" in
  let fx = Context.app f.ctx ff [ x ]
  and fy = Context.app f.ctx ff [ y ] in
  let eq_xy = Context.eq f.ctx x y in
  let lt = Context.lt f.ctx fx fy in
  Ctrl_router.set_owner eq_xy Ctrl_router.Both;
  Ctrl_router.set_owner lt Ctrl_router.B;
  (* f(x),f(y) occur ONLY in the LIA-owned atom [lt] *)
  let t = Cmock.create f.ctx f.env in
  Cmock.register_atom t (fresh_atom f) eq_xy;
  Cmock.register_atom t (fresh_atom f) lt;
  (* EUF's model: x=y and (congruence) f(x)=f(y). LIA's model: x=y but f(x)<f(y). *)
  (MockA.model_fn
   := fun () ->
        [ x, Model.Uninterp 1
        ; y, Model.Uninterp 1
        ; fx, Model.Uninterp 5
        ; fy, Model.Uninterp 5
        ]);
  (MockB.model_fn
   := fun () -> [ x, Model.Int 0; y, Model.Int 0; fx, Model.Int 1; fy, Model.Int 2 ]);
  match Cmock.check t Th.Final with
  | Th.Split _ ->
    check
      "W1: disagreement on model-valued f(x),f(y) (not in seen∩) ⇒ Split, not Sat"
      true
  | _ ->
    check
      "W1: disagreement on model-valued f(x),f(y) (not in seen∩) ⇒ Split, not Sat"
      false
;;

(* codex W1-lookup repro (compound term) — a disagreement on a COMPOUND like [x+1] is only
   caught because find_disagreement EVALUATES via model_eval (folding), not raw
   Model.value. The equality [f(w) = x+1] makes BOTH sides structural interface members
   (an App-side and a sum-side boundary node). EUF holds them distinct; LIA keys x (=4)
   and f(w) (=5) and folds x+1 to 5 = f(w). A raw lookup would skip the un-keyed compound
   (x+1) and certify a spurious Sat; the fold catches the disagreement. *)
let test_compound_disagreement_lookup () =
  reset_mocks ();
  let f = fixture () in
  let x = const f "x"
  and w = const f "w" in
  let ff = ufun f "f" in
  let fw = Context.app f.ctx ff [ w ] in
  let xp1 = Context.add f.ctx x (Context.int_const f.ctx 1) in
  let e = Context.eq f.ctx fw xp1 in
  Ctrl_router.set_owner e Ctrl_router.Both;
  let t = Cmock.create f.ctx f.env in
  Cmock.register_atom t (fresh_atom f) e;
  (* EUF: f(w) and (x+1) distinct classes. LIA: keys x (=4) and f(w) (=5); x+1 folds to 5. *)
  (MockA.model_fn := fun () -> [ fw, Model.Uninterp 1; xp1, Model.Uninterp 2 ]);
  (MockB.model_fn := fun () -> [ fw, Model.Int 5; x, Model.Int 4 ]);
  match Cmock.check t Th.Final with
  | Th.Split _ ->
    check "W1-lookup: disagreement on a compound (x+1) via model_eval ⇒ Split" true
  | _ -> check "W1-lookup: disagreement on a compound (x+1) via model_eval ⇒ Split" false
;;

(* codex W2 (HIGH wrong-SAT) — model_eval's fold must be overflow-GUARDED: a raw
   [max_int * 2] wraps to [-2], which would let check_pins read a violated pin as
   satisfied. Here a pin over the compound [max_int * x] with x↦2 must RAISE (→ unknown),
   not wrap. *)
let test_overflow_guarded_fold () =
  reset_mocks ();
  let f = fixture () in
  let x = const f "x" in
  let big = Context.mul_const f.ctx max_int x in
  let e = Context.eq f.ctx big (Context.int_const f.ctx 0) in
  Ctrl_router.set_owner e Ctrl_router.Both;
  let t = Cmock.create f.ctx f.env in
  let a = fresh_atom f in
  Cmock.register_atom t a e;
  Cmock.assert_lit t (Lit.make a true);
  (* pins [max_int * x = 0]; x↦2 makes the fold overflow *)
  (MockA.model_fn := fun () -> [ x, Model.Int 2 ]);
  (MockB.model_fn := fun () -> [ x, Model.Int 2 ]);
  let raised =
    try
      ignore (Cmock.check t Th.Final);
      false
    with
    | Cmb.Combination_unsound _ -> true
  in
  check
    "W2: overflow in the model-eval fold raises (→ unknown), never wraps to a wrong Sat"
    raised
;;

(* reviewer T4 — an [assert_lit]/[explain] on an atom [register_atom] never saw is an
   engine contract break; the combinator refuses to guess a routing → degrade. *)
let test_unregistered_atom_poison () =
  reset_mocks ();
  let f = fixture () in
  let t = Cmock.create f.ctx f.env in
  let stray = fresh_atom f in
  (* never registered *)
  let raised =
    try
      Cmock.assert_lit t (Lit.make stray true);
      false
    with
    | Cmb.Combination_unsound _ -> true
  in
  check "T4: assert_lit on an unregistered atom → Combination_unsound" raised
;;

(* reviewer T1 — [explain] of a literal never recorded as propagated falls back to the
   literal's assert-time owner (no crash, no wrong-arm), keeping the Both→arm
   well-defined. *)
let test_explain_fallback () =
  reset_mocks ();
  let f = fixture () in
  let x = const f "x"
  and y = const f "y" in
  let e = Context.eq f.ctx x y in
  Ctrl_router.set_owner e Ctrl_router.Both;
  let t = Cmock.create f.ctx f.env in
  let a = fresh_atom f in
  Cmock.register_atom t a e;
  (* nothing propagated; explain a positive Both lit → falls back to A (assert_to +Both) *)
  clear_log ();
  ignore (Cmock.explain t (Lit.make a true));
  check
    "T1: explain of a non-propagated +Both lit falls back to A (no crash)"
    (saw_explain "A" (Lit.make a true))
;;

(* codex round-2 regression — register_atom's subterm walk must be O(distinct subterms),
   not O(paths). Terms are a hash-consed DAG; an [g (t, t)] tower of depth 40 has 41
   distinct nodes but 2^40 root-to-leaf paths. Without the membership guard in
   [add_subterms] this registration would hang; with it, it returns in ms. *)
let test_compact_dag_registration () =
  reset_mocks ();
  let f = fixture () in
  let g = Env.declare_fun f.env "g" (Rank.create [ Sort.int; Sort.int ] Sort.int) in
  let rec tower n acc =
    if n = 0 then acc else tower (n - 1) (Context.app f.ctx g [ acc; acc ])
  in
  let top = tower 40 (const f "base") in
  let atom = Context.eq f.ctx top (const f "c") in
  Ctrl_router.set_owner atom Ctrl_router.A;
  let t = Cmock.create f.ctx f.env in
  (* were the walk path-exponential, this call would not return *)
  Cmock.register_atom t (fresh_atom f) atom;
  check
    "compact-DAG: depth-40 g(t,t) tower registers (guarded walk, terms stay linear)"
    (Context.term_count f.ctx < 100)
;;

(* codex S1 (unit) — the router's polarity contract on the concrete Uflia_router. *)
let test_router_polarity_contract () =
  let module R = Oxsmt_combine.Uflia_router in
  let f = fixture () in
  let x = const f "x"
  and y = const f "y" in
  let e = Context.eq f.ctx x y in
  check
    "S1: owner(Int eq) = Both (register into both)"
    (match R.owner e with
     | R.Both -> true
     | _ -> false);
  check
    "S1: assert_to(+Int eq) = Both"
    (match R.assert_to e ~positive:true with
     | R.Both -> true
     | _ -> false);
  check
    "S1: assert_to(-Int eq) = A (EUF only — LIA can't take a diseq)"
    (match R.assert_to e ~positive:false with
     | R.A -> true
     | _ -> false)
;;

(* ================================================================================== *)
(* Part 2 — INTEGRATION: toy EUF + toy LIA + a mini DPLL(T) driver. *)
(* ================================================================================== *)

(* An Int assignment of atomic terms (App / literal) → int; used by toy LIA and the
   SAT-model self-check. Descends into Arith (LIA's view) but treats an App atomically
   (LIA is congruence-blind). *)
let rec eval_int (asg : int Term.Map.t) (t : Term.t) : int =
  match t.Term.node with
  | Term.Int_const n -> n
  | Term.App (_, _) -> Term.Map.find t asg
  | Term.Arith lin ->
    Iarr.fold
      (fun acc (child, c) -> acc + (c * eval_int asg child))
      lin.Term.const
      lin.Term.coeffs
  | _ -> raise (Invalid_argument "eval_int: not an Int term")
;;

let eval_atom (asg : int Term.Map.t) (t : Term.t) : bool =
  match t.Term.node with
  | Term.Le a -> eval_int asg a <= 0
  | Term.Eq (a, b) -> eval_int asg a = eval_int asg b
  | Term.Bool_const b -> b
  | _ -> raise (Invalid_argument "eval_atom: not a LIA atom")
;;

(* atomic Int terms LIA assigns: App nodes (not descending into args) and the operands of
   an Arith node; literals are seeded to their value. *)
let rec lia_atoms (acc : Term.Set.t) (t : Term.t) : Term.Set.t =
  match t.Term.node with
  | Term.Int_const _ -> Term.Set.add t acc
  | Term.App (_, _) -> Term.Set.add t acc
  | Term.Arith lin ->
    Iarr.fold (fun acc (child, _) -> lia_atoms acc child) acc lin.Term.coeffs
  | Term.Le a -> lia_atoms acc a
  | Term.Eq (a, b) -> lia_atoms (lia_atoms acc a) b
  | Term.Not a -> lia_atoms acc a
  | Term.And xs | Term.Or xs -> Iarr.fold lia_atoms acc xs
  | Term.Ite (a, b, c) -> lia_atoms (lia_atoms (lia_atoms acc a) b) c
  | Term.Bool_const _ -> acc
;;

(* ---- TOY LIA: brute-force bounded-integer decision procedure ---------------------- *)

module Toy_lia = struct
  let range_lo = -3
  let range_hi = 3

  type t =
    { atom_term : Term.t Atom.Table.t (* learned at register_atom (monotone) *)
    ; mutable frames : Lit.t list list (* asserted lits per level *)
    ; mutable atoms : Term.Set.t
    ; mutable last_model : int Term.Map.t
    }

  let create _ctx _env =
    { atom_term = Atom.Table.create 32
    ; frames = [ [] ]
    ; atoms = Term.Set.empty
    ; last_model = Term.Map.empty
    }
  ;;

  (* faithful to the real LIA (lia.mli): a disequality is out of fragment. Raising here is
     what makes the S1 regression real — if the router ever routed a negated Int equality
     to LIA, this fires (and, via CONTRACT-POISON, would degrade to unknown). *)
  exception Unsupported_diseq

  let register_atom t a term =
    Atom.Table.replace t.atom_term a term;
    t.atoms <- lia_atoms t.atoms term
  ;;

  let assert_lit t l =
    (match (Atom.Table.find t.atom_term (Lit.atom l)).Term.node with
     | Term.Eq _ when not (Lit.sign l) -> raise Unsupported_diseq
     | _ -> ());
    match t.frames with
    | fr :: rest -> t.frames <- (l :: fr) :: rest
    | [] -> t.frames <- [ [ l ] ]
  ;;

  let asserted_lits t = List.concat t.frames

  (* (term, sign) pairs for the currently-asserted lits *)
  let constraints t =
    List.map
      (fun l -> Atom.Table.find t.atom_term (Lit.atom l), Lit.sign l)
      (asserted_lits t)
  ;;

  let feasible t =
    let vars =
      Term.Set.elements t.atoms
      |> List.filter (fun (a : Term.t) ->
        match a.Term.node with
        | Term.App _ -> true
        | _ -> false)
    in
    let base =
      Term.Set.elements t.atoms
      |> List.filter_map (fun (a : Term.t) ->
        match a.Term.node with
        | Term.Int_const n -> Some (a, n)
        | _ -> None)
      |> List.fold_left (fun m (a, n) -> Term.Map.add a n m) Term.Map.empty
    in
    let cs = constraints t in
    let ok asg =
      List.for_all (fun (term, sign) -> Bool.equal (eval_atom asg term) sign) cs
    in
    let rec search asg = function
      | [] -> if ok asg then Some asg else None
      | v :: rest ->
        let rec try_val n =
          if n > range_hi
          then None
          else (
            match search (Term.Map.add v n asg) rest with
            | Some m -> Some m
            | None -> try_val (n + 1))
        in
        try_val range_lo
    in
    search base vars
  ;;

  let check t effort : Th.check_result =
    match feasible t with
    | Some m ->
      t.last_model <- m;
      (match effort with
       | Th.Final -> Th.Sat
       | Th.Propagate -> Th.Propagations [])
    | None ->
      Th.Conflict
        { Explanation.premises = asserted_lits t; rule = Explanation.Rule_tag.Lia_farkas }
  ;;

  let explain _t _l = { Explanation.premises = []; rule = Explanation.Rule_tag.Lia_bound }
  let push t = t.frames <- [] :: t.frames

  let pop t n =
    let rec drop k fr =
      if k <= 0
      then fr
      else (
        match fr with
        | _ :: r -> drop (k - 1) r
        | [] -> [])
    in
    t.frames
    <- (match drop n t.frames with
        | [] -> [ [] ]
        | fr -> fr)
  ;;

  let model t =
    Model.of_alist
      (Term.Map.fold (fun k v acc -> (k, Model.Int v) :: acc) t.last_model [])
  ;;

  (* FABRIC seam (arithmetic child): the brute-force toy tracks no simplex bounds, so it
     reports no fixed value — the fix-trigger never injects through it. *)
  let check_fabric t eff = fabric_of_check (check t eff)
  let explain_fabric t l = fabric_of_expl (explain t l)
  let fixed_bounds _t _term = None
  let fabric_verify _t _term _value _lo _hi = false
end

(* ---- TOY EUF: naive congruence closure over App, with disequalities --------------- *)

module Toy_euf = struct
  type t =
    { atom_term : Term.t Atom.Table.t
    ; mutable frames : Lit.t list list
    ; mutable terms : Term.Set.t
    ; mutable last_class : int Term.Map.t
    }

  let create _ctx _env =
    { atom_term = Atom.Table.create 32
    ; frames = [ [] ]
    ; terms = Term.Set.empty
    ; last_class = Term.Map.empty
    }
  ;;

  let rec add_subterms acc (t : Term.t) =
    let acc = Term.Set.add t acc in
    match t.Term.node with
    | Term.App (_, args) -> Iarr.fold add_subterms acc args
    | Term.Eq (a, b) -> add_subterms (add_subterms acc a) b
    | Term.Le a -> add_subterms acc a
    | Term.Arith lin ->
      Iarr.fold (fun acc (c, _) -> add_subterms acc c) acc lin.Term.coeffs
    | Term.Not a -> add_subterms acc a
    | Term.And xs | Term.Or xs -> Iarr.fold add_subterms acc xs
    | Term.Ite (a, b, c) -> add_subterms (add_subterms (add_subterms acc a) b) c
    | Term.Bool_const _ | Term.Int_const _ -> acc
  ;;

  let register_atom t a term =
    Atom.Table.replace t.atom_term a term;
    t.terms <- add_subterms t.terms term
  ;;

  (* the CONGRUENCE_CHILD hook: internalise the term's subterm closure with no atom
     binding, exactly as the real Euf_adapter does — a naive congruence closure gains
     nodes for a boundary term surfacing only inside a LIA atom. *)
  let internalize_term t term = t.terms <- add_subterms t.terms term

  let assert_lit t l =
    match t.frames with
    | fr :: rest -> t.frames <- (l :: fr) :: rest
    | [] -> t.frames <- [ [ l ] ]
  ;;

  let asserted_lits t = List.concat t.frames
  let term_of t l = Atom.Table.find t.atom_term (Lit.atom l)

  (* union-find keyed by term tag, recomputed each check *)
  let closure t =
    let uf : (int, int) Hashtbl.t = Hashtbl.create 64 in
    Term.Set.iter (fun tm -> Hashtbl.replace uf tm.Term.tag tm.Term.tag) t.terms;
    let rec find x =
      match Hashtbl.find_opt uf x with
      | Some p when p <> x -> find p
      | _ -> x
    in
    let union a b =
      let ra = find a
      and rb = find b in
      if ra <> rb then Hashtbl.replace uf (max ra rb) (min ra rb)
    in
    List.iter
      (fun l ->
         if Lit.sign l
         then (
           match (term_of t l).Term.node with
           | Term.Eq (a, b) -> union a.Term.tag b.Term.tag
           | _ -> ()))
      (asserted_lits t);
    let apps =
      Term.Set.elements t.terms
      |> List.filter_map (fun (tm : Term.t) ->
        match tm.Term.node with
        | Term.App (s, args) when Iarr.length args > 0 -> Some (tm, s, Iarr.to_list args)
        | _ -> None)
    in
    let changed = ref true in
    while !changed do
      changed := false;
      List.iter
        (fun (t1, s1, a1) ->
           List.iter
             (fun (t2, s2, a2) ->
                if
                  Symbol.equal s1 s2
                  && List.length a1 = List.length a2
                  && List.for_all2 (fun x y -> find x.Term.tag = find y.Term.tag) a1 a2
                  && find t1.Term.tag <> find t2.Term.tag
                then (
                  union t1.Term.tag t2.Term.tag;
                  changed := true))
             apps)
        apps
    done;
    find
  ;;

  let consistent t =
    let find = closure t in
    List.for_all
      (fun l ->
         if Lit.sign l
         then true
         else (
           match (term_of t l).Term.node with
           | Term.Eq (a, b) -> find a.Term.tag <> find b.Term.tag
           | _ -> true))
      (asserted_lits t)
  ;;

  let check t effort : Th.check_result =
    if consistent t
    then (
      let find = closure t in
      t.last_class
      <- Term.Set.fold
           (fun tm m -> Term.Map.add tm (find tm.Term.tag) m)
           t.terms
           Term.Map.empty;
      match effort with
      | Th.Final -> Th.Sat
      | Th.Propagate -> Th.Propagations [])
    else
      Th.Conflict
        { Explanation.premises = asserted_lits t
        ; rule = Explanation.Rule_tag.Euf_congruence
        }
  ;;

  let explain _t _l =
    { Explanation.premises = []; rule = Explanation.Rule_tag.Euf_congruence }
  ;;

  let push t = t.frames <- [] :: t.frames

  let pop t n =
    let rec drop k fr =
      if k <= 0
      then fr
      else (
        match fr with
        | _ :: r -> drop (k - 1) r
        | [] -> [])
    in
    t.frames
    <- (match drop n t.frames with
        | [] -> [ [] ]
        | fr -> fr)
  ;;

  let model t =
    Model.of_alist
      (Term.Map.fold (fun k v acc -> (k, Model.Uninterp v) :: acc) t.last_class [])
  ;;

  (* FABRIC seam (congruence child). [fabric_are_equal] reads the closure; the toy fixes
     no arithmetic value so [fixed_bounds] is [None] and [assert_fabric_eq] is unreached
     in the toy layer (a real fixed-value injection is exercised by the real-adapter
     layer). *)
  let check_fabric t eff = fabric_of_check (check t eff)
  let explain_fabric t l = fabric_of_expl (explain t l)
  let fixed_bounds _t _term = None
  let fabric_verify _t _term _value _lo _hi = false

  let fabric_are_equal t a b =
    let find = closure t in
    find a.Term.tag = find b.Term.tag
  ;;

  let assert_fabric_eq _t ~edge_id:_ _ _ = ()
end

module Cuflia = Cmb.Combine (Oxsmt_combine.Uflia_router) (Toy_euf) (Toy_lia)

(* ---- mini DPLL(T) driver over the combined theory --------------------------------- *)

type verdict =
  | Vsat of Model.t
  | Vunsat
  | Vunknown
(* Combine.Incomplete — a sound completeness degrade (Bool compound under UF) *)

exception Driver_overflow

(* Count [Split]s the driver observes, for the pure-QF_LIA "empty interface ⇒ zero
   arrangement splits" perf fixture. Reset per query by the caller. *)
let splits_seen = ref 0

(* A split-branching STRATEGY maps a [Split]'s disjuncts to the DFS branches to try; each
   branch is a set of literals asserted together. The default is lazy — one positive
   disjunct per branch (what the previous driver did). {!full_assignment_cells} instead
   makes each branch a COMPLETE decision of the shared pair's relation (the equality atom
   is assigned in EVERY branch), matching the CDCL(T) seam's full-assignment-at-[Final]
   contract — so a test can exercise the REAL termination mechanism, not an eq-first
   order. *)
let lazy_cells terms = List.map (fun t -> [ t, true ]) terms

let full_assignment_cells terms =
  match terms with
  | [ eq; lt; gt ] ->
    (* a real clausified [eq ∨ lt ∨ gt] split, fully assigned: each cell decides ALL THREE
       atoms (exactly one true), as the SAT core would at a full model. Every cell is
       load-bearing — the SAT integration goal below is satisfiable ONLY via the [lt]
       arrangement (eq ⇒ EUF congruence conflict, gt ⇒ LIA-infeasible), so deleting any
       cell flips a verdict. *)
    [ [ eq, true; lt, false; gt, false ]
    ; [ eq, false; lt, true; gt, false ]
    ; [ eq, false; lt, false; gt, true ]
    ]
  | _ -> lazy_cells terms
;;

(* Solve a conjunction of unit literals [(atom_term, sign)]. The driver internalizes each
   atom 1:1 through [register_atom] (as the clausifier would), asserts it, then drives
   [check Final] to a verdict — branching (DFS) on each {!Combine.Split} per [cells],
   which is exactly the split lifecycle the CDCL(T) seam will run.

   Functorized over the combined theory [C] so the SAME loop drives either the toy stack
   ([Driver_toy], below) or the real Euf_adapter + Lia_adapter stack ([Driver_real], the
   W1-through-real-stack gate) — the header's "swap the two functor arguments" promise. *)
module Make_driver (C : Th.THEORY) = struct
  (* A [Combine.Incomplete] raised at register/assert time (a Bool compound under a UF
     argument, ADR §3.6 case (ii)) is a sound completeness degrade → the whole query is
     [Vunknown], exactly as the session layer would map CONTRACT-POISON's sibling. Caught
     at the top so it also unwinds a mid-search branch. *)
  let solve ?(cells = lazy_cells) f (formula : (Term.t * bool) list) : verdict =
    try
      let t = C.create f.ctx f.env in
      let atom_tbl : Atom.t Term.Table.t = Term.Table.create 64 in
      let atom_of term =
        match Term.Table.find_opt atom_tbl term with
        | Some a -> a
        | None ->
          let a = fresh_atom f in
          Term.Table.replace atom_tbl term a;
          C.register_atom t a term;
          a
      in
      let assert_term term sign = C.assert_lit t (Lit.make (atom_of term) sign) in
      List.iter (fun (term, sign) -> assert_term term sign) formula;
      let rec search depth : verdict =
        if depth > 64 then raise Driver_overflow;
        match C.check t Th.Final with
        | Th.Conflict _ -> Vunsat
        | Th.Sat -> Vsat (C.model t)
        | Th.Propagations lits ->
          List.iter (fun l -> C.assert_lit t l) lits;
          search (depth + 1)
        | Th.Split terms ->
          incr splits_seen;
          let rec try_cells = function
            | [] -> Vunsat
            | cell :: rest ->
              C.push t;
              List.iter (fun (tm, sg) -> assert_term tm sg) cell;
              (match search (depth + 1) with
               | (Vsat _ | Vunknown) as v -> v
               | Vunsat ->
                 C.pop t 1;
                 try_cells rest)
          in
          try_cells (cells terms)
      in
      search 0
    with
    | Oxsmt_combine.Combine.Incomplete _ -> Vunknown
  ;;
end

module Driver_toy = Make_driver (Cuflia)

(* Preserve the historic unqualified call-site name for the toy-stack integration tests. *)
let solve ?cells f formula = Driver_toy.solve ?cells f formula

(* self-check a SAT model (a stand-in for the §8 evaluator): every formula literal must
   evaluate true under the merged witness. [ev_int] folds Arith over leaf Int values (so a
   compound like [x+1] is evaluated even if the witness only keys [x]); equality compares
   the raw model values of the two sides, so it also handles uninterpreted-sort equalities
   (opaque-class identity), whose sides the witness keys as [Uninterp]. *)
let model_satisfies (m : Model.t) (formula : (Term.t * bool) list) : bool =
  let rec ev_int tm : int option =
    match Model.value m tm with
    | Some (Model.Int n) -> Some n
    | Some _ -> None
    | None ->
      (match tm.Term.node with
       | Term.Int_const n -> Some n
       | Term.Arith lin ->
         Iarr.fold
           (fun acc (child, c) ->
              match acc, ev_int child with
              | Some a, Some v -> Some (a + (c * v))
              | _ -> None)
           (Some lin.Term.const)
           lin.Term.coeffs
       | _ -> None)
  in
  (* a value for an equality side: an Int (folded) tagged, or the raw model value. *)
  let eq_val tm : Model.value option =
    match ev_int tm with
    | Some n -> Some (Model.Int n)
    | None -> Model.value m tm
  in
  let val_eq u v =
    match u, v with
    | Model.Int a, Model.Int b -> a = b
    | Model.Bool a, Model.Bool b -> Bool.equal a b
    | Model.Uninterp a, Model.Uninterp b -> a = b
    | _ -> false
  in
  List.for_all
    (fun (term, sign) ->
       let v =
         match term.Term.node with
         | Term.Le a ->
           (match ev_int a with
            | Some n -> Some (n <= 0)
            | None -> None)
         | Term.Eq (a, b) ->
           (match eq_val a, eq_val b with
            | Some x, Some y -> Some (val_eq x y)
            | _ -> None)
         | _ -> None
       in
       match v with
       | Some b -> Bool.equal b sign
       | None -> false)
    formula
;;

let test_integration_sat () =
  let f = fixture () in
  let a = const f "a"
  and b = const f "b" in
  let ff = ufun f "f" in
  let fa = Context.app f.ctx ff [ a ]
  and fb = Context.app f.ctx ff [ b ] in
  (* f(a) != f(b) ∧ a <= b — satisfiable (a<b, f distinct) *)
  let formula = [ Context.eq f.ctx fa fb, false; Context.le f.ctx a b, true ] in
  match solve f formula with
  | Vsat m ->
    check "integration SAT: verdict Sat" true;
    check "integration SAT: model self-check passes" (model_satisfies m formula)
  | Vunsat | Vunknown ->
    check "integration SAT: verdict Sat" false;
    check "integration SAT: model self-check passes" false
;;

let test_integration_unsat_direct () =
  let f = fixture () in
  let a = const f "a"
  and b = const f "b" in
  let ff = ufun f "f" in
  let fa = Context.app f.ctx ff [ a ]
  and fb = Context.app f.ctx ff [ b ] in
  (* a = b ∧ f(a) != f(b) — UNSAT: the shared equality reaches EUF (Both-routing) and
     congruence forces f(a)=f(b). *)
  let formula = [ Context.eq f.ctx a b, true; Context.eq f.ctx fa fb, false ] in
  match solve f formula with
  | Vunsat -> check "integration UNSAT (direct, Both-routing → EUF congruence)" true
  | Vsat _ | Vunknown ->
    check "integration UNSAT (direct, Both-routing → EUF congruence)" false
;;

let test_integration_unsat_split () =
  let f = fixture () in
  let a = const f "a"
  and b = const f "b" in
  let ff = ufun f "f" in
  let fa = Context.app f.ctx ff [ a ]
  and fb = Context.app f.ctx ff [ b ] in
  (* f(a) != f(b) ∧ a <= b ∧ b <= a — UNSAT, but ONLY via model-based combination: LIA
     forces a=b (never asserted), EUF is independently Sat; the arrangement disagrees, so
     the combinator must split on a=b, after which EUF derives the congruence conflict. *)
  let formula =
    [ Context.eq f.ctx fa fb, false
    ; Context.le f.ctx a b, true
    ; Context.le f.ctx b a, true
    ]
  in
  match solve f formula with
  | Vunsat -> check "integration UNSAT (requires model-based ℤ-equality split)" true
  | Vsat _ | Vunknown ->
    check "integration UNSAT (requires model-based ℤ-equality split)" false
;;

(* The SAME goals under FULL-ASSIGNMENT-at-Final driving (the real seam's contract): each
   split branch decides the shared equality atom explicitly (eq=true, or eq=false + an
   ordering), so the termination mechanism under test is "the equality atom is always
   assigned; either polarity resolves the disagreement next Final" — not an eq-first DFS
   accident. Verdicts must match the lazy driver. *)
let test_integration_full_assignment () =
  let f = fixture () in
  let a = const f "a"
  and b = const f "b" in
  let ff = ufun f "f" in
  let fa = Context.app f.ctx ff [ a ]
  and fb = Context.app f.ctx ff [ b ] in
  (* UNSAT-via-split, driven full-assignment *)
  let unsat_formula =
    [ Context.eq f.ctx fa fb, false
    ; Context.le f.ctx a b, true
    ; Context.le f.ctx b a, true
    ]
  in
  (match solve ~cells:full_assignment_cells f unsat_formula with
   | Vunsat ->
     check "full-assignment: UNSAT-via-split terminates UNSAT (real mechanism)" true
   | Vsat _ | Vunknown ->
     check "full-assignment: UNSAT-via-split terminates UNSAT (real mechanism)" false);
  (* SAT case, driven full-assignment: model still self-checks *)
  let f = fixture () in
  let a = const f "a"
  and b = const f "b" in
  let ff = ufun f "f" in
  let fa = Context.app f.ctx ff [ a ]
  and fb = Context.app f.ctx ff [ b ] in
  let sat_formula = [ Context.eq f.ctx fa fb, false; Context.le f.ctx a b, true ] in
  match solve ~cells:full_assignment_cells f sat_formula with
  | Vsat m ->
    check
      "full-assignment: SAT terminates + model self-checks"
      (model_satisfies m sat_formula)
  | Vunsat | Vunknown -> check "full-assignment: SAT terminates + model self-checks" false
;;

(* codex round-2 C2 landmine — the ubiquitous [y = x + 1]. The pinned equality's RHS is a
   compound [Arith] the arithmetic child never keys (its model keys only leaves x, y). The
   old raw-lookup check_pins saw None and degraded this to unknown; the evaluating check
   folds x+1 and certifies Sat. Discriminating: without the fold fix this returns Vunsat
   (poison→driver treats it as no model) or worse. *)
let test_integration_compound_pin () =
  let f = fixture () in
  let x = const f "x"
  and y = const f "y" in
  (* y = x + 1 (SAT) — Eq(y, x+1); LIA keys x,y but not the compound x+1 *)
  let formula =
    [ Context.eq f.ctx y (Context.add f.ctx x (Context.int_const f.ctx 1)), true ]
  in
  (match solve f formula with
   | Vsat m ->
     check "C2 landmine: y = x+1 (compound pin side) ⇒ Sat" true;
     check "C2 landmine: y = x+1 model self-checks" (model_satisfies m formula)
   | Vunsat | Vunknown ->
     check "C2 landmine: y = x+1 (compound pin side) ⇒ Sat" false;
     check "C2 landmine: y = x+1 model self-checks" false);
  (* codex's concrete reproducer: (= (+ (f a) x) 0) — a compound equality with a function
     application ON one side. The pinned side (f a)+x is un-keyed by LIA (it keys f(a), a,
     x as leaves), so check_pins must fold it; the App leaf inside the sum exercises the
     App branch of the evaluator. *)
  let g = fixture () in
  let a = const g "a"
  and x = const g "x" in
  let ff = ufun g "f" in
  let sum = Context.add g.ctx (Context.app g.ctx ff [ a ]) x in
  let formula2 = [ Context.eq g.ctx sum (Context.int_const g.ctx 0), true ] in
  match solve g formula2 with
  | Vsat m ->
    check "C2 reproducer: (f a) + x = 0 (compound with an App leaf) ⇒ Sat" true;
    check "C2 reproducer: (f a) + x = 0 model self-checks" (model_satisfies m formula2)
  | Vunsat | Vunknown ->
    check "C2 reproducer: (f a) + x = 0 (compound with an App leaf) ⇒ Sat" false;
    check "C2 reproducer: (f a) + x = 0 model self-checks" false
;;

(* codex round-2 C3 landmine + reviewer's discriminating fixture — a term that appears
   ONLY as a function argument, with no arithmetic atom on it. [f(x)] here is Int-sorted
   but is seen only by EUF (inside the uninterpreted-sort equality k(f(x)) = k(z), routed
   to EUF); the arithmetic child never sees it, so it has only an opaque class. The old
   model() raised on it → unknown on this normal shape. model() now SURFACES f(x)'s EUF
   class (§10 realize signal, task #110) rather than omitting it, and the witness (a,b the
   arithmetic child, the k-terms the congruence child, f(x) its realized integer) still
   passes the eval self-check. Also carries a compound pin (y = x+1) so both landmines
   fire in one fixture. *)
let test_integration_pure_euf_int_arg () =
  let f = fixture () in
  let x = const f "x"
  and y = const f "y"
  and z = const f "z" in
  let ff = ufun f "f" in
  let u_sort = Sort.uninterpreted (Env.declare_sort f.env "U") in
  let k = Env.declare_fun f.env "k" (Rank.create [ Sort.int ] u_sort) in
  let fx = Context.app f.ctx ff [ x ] in
  let kfx = Context.app f.ctx k [ fx ]
  and kz = Context.app f.ctx k [ z ] in
  (* k(f(x)) = k(z) (U-sorted eq, EUF-only — f(x) is a pure-EUF Int term) ∧ y = x + 1
     (LIA, compound pin) *)
  let formula =
    [ Context.eq f.ctx kfx kz, true
    ; Context.eq f.ctx y (Context.add f.ctx x (Context.int_const f.ctx 1)), true
    ]
  in
  match solve f formula with
  | Vsat m ->
    check "C3 landmine: pure-EUF Int fn-arg ⇒ Sat (no false unknown)" true;
    check "C3 landmine: witness passes eval self-check" (model_satisfies m formula)
  | Vunsat | Vunknown ->
    check "C3 landmine: pure-EUF Int fn-arg ⇒ Sat (no false unknown)" false;
    check "C3 landmine: witness passes eval self-check" false
;;

(* Uflia_router shared-sort invariant (the load-bearing "why the ℤ-trichotomy suffices"
   argument): the disagreement search only ever hands equality_split an Int pair (its
   domain is Int-sorted terms both models value), so equality_split yields the trichotomy
   on an Int pair and REFUSES a non-Int pair (unreachable in QF_UFLIA) rather than
   building an ill-sorted order atom. *)
let test_router_shared_sort_invariant () =
  let module R = Oxsmt_combine.Uflia_router in
  let f = fixture () in
  let u_sort = Sort.uninterpreted (Env.declare_sort f.env "U") in
  let uc name =
    Context.const f.ctx (Env.declare_fun f.env name (Rank.create [] u_sort))
  in
  let p = uc "p"
  and q = uc "q" in
  let x = const f "x"
  and y = const f "y" in
  (match R.equality_split f.ctx x y with
   | [ e; _; _ ] ->
     check
       "router: Int pair ⇒ 3-atom split, head = x=y"
       (Term.equal e (Context.eq f.ctx x y))
   | _ -> check "router: Int pair ⇒ 3-atom split, head = x=y" false);
  let raised =
    try
      ignore (R.equality_split f.ctx p q);
      false
    with
    | Cmb.Combination_unsound _ -> true
  in
  check "router: non-Int pair ⇒ guarded raise (unreachable in QF_UFLIA)" raised
;;

(* ================================================================================== *)
(* Part 3 — THE W1 GATE: the REAL Euf_adapter + real Lia_adapter, NO mocks. *)
(* ================================================================================== *)

(* The master's empirical acceptance criterion, run end-to-end through the real stack via
   the SAME functorized driver. Both goals are the wrong-SAT the W1 fix targets: the
   shared applications (f(x),f(y) — and, in the tower, g(f(x)),g(f(y))) occur ONLY inside
   a LIA order atom, so EUF sees them purely because the adapter registers a non-owned
   atom's full subterm closure (K_foreign, landed on trunk as e00e6e4). Congruence then
   closes f(x)=f(y) from x=y, while LIA's model keeps the pair distinct (a strict order ⇒
   ≠) — the arrangement disagreement the model-based domain (Int terms BOTH models value,
   NOT a syntactic seen-set intersection) must catch and split. Every trichotomy branch is
   then refuted: eq ⇒ LIA infeasible against the strict order; ≠ ⇒ EUF congruence
   conflict.

   Driven full-assignment (the CDCL(T) seam's contract: each split branch decides the
   whole eq/lt/gt trichotomy), because the lazy one-disjunct driver would loop here — the
   [lt] disjunct is self-consistent for LIA, so only asserting the [eq]-polarity closes
   it. *)
module Cuflia_real =
  Cmb.Combine (Oxsmt_combine.Uflia_router) (Oxsmt_euf.Euf_adapter) (Oxsmt_lia.Lia_adapter)

module Driver_real = Make_driver (Cuflia_real)

let test_w1_real_flat () =
  let f = fixture () in
  let x = const f "x"
  and y = const f "y" in
  let ff = ufun f "f" in
  let fx = Context.app f.ctx ff [ x ]
  and fy = Context.app f.ctx ff [ y ] in
  (* x = y ∧ f(x) < f(y) — UNSAT: x=y ⇒ (EUF) f(x)=f(y), but f(x)<f(y) ⇒ f(x)≠f(y). *)
  let formula = [ Context.eq f.ctx x y, true; Context.lt f.ctx fx fy, true ] in
  (match Driver_real.solve ~cells:full_assignment_cells f formula with
   | Vunsat -> check "W1 real stack: x=y ∧ f(x)<f(y) ⇒ UNSAT" true
   | Vsat _ | Vunknown -> check "W1 real stack: x=y ∧ f(x)<f(y) ⇒ UNSAT" false);
  (* Discriminator — DROP x=y: f(x)<f(y) alone is SAT, so the UNSAT above is the
     congruence collision, not a stack that certifies UNSAT unconditionally. *)
  let g = fixture () in
  let x = const g "x"
  and y = const g "y" in
  let ff = ufun g "f" in
  let fx = Context.app g.ctx ff [ x ]
  and fy = Context.app g.ctx ff [ y ] in
  let sat_formula = [ Context.lt g.ctx fx fy, true ] in
  match Driver_real.solve ~cells:full_assignment_cells g sat_formula with
  | Vsat _ -> check "W1 real stack: f(x)<f(y) alone ⇒ SAT (no spurious UNSAT)" true
  | Vunsat | Vunknown ->
    check "W1 real stack: f(x)<f(y) alone ⇒ SAT (no spurious UNSAT)" false
;;

let test_w1_real_tower () =
  let f = fixture () in
  let x = const f "x"
  and y = const f "y" in
  let ff = ufun f "f"
  and gg = ufun f "g" in
  let gfx = Context.app f.ctx gg [ Context.app f.ctx ff [ x ] ]
  and gfy = Context.app f.ctx gg [ Context.app f.ctx ff [ y ] ] in
  (* x = y ∧ g(f(x)) < g(f(y)) — UNSAT via the FULL subterm closure: x=y ⇒ f(x)=f(y) ⇒
     g(f(x))=g(f(y)), contradicting the strict order. Exercises nested congruence over
     terms that appear only inside a LIA atom (not just the top-level applications). *)
  let formula = [ Context.eq f.ctx x y, true; Context.lt f.ctx gfx gfy, true ] in
  match Driver_real.solve ~cells:full_assignment_cells f formula with
  | Vunsat -> check "W1 real stack: x=y ∧ g(f(x))<g(f(y)) ⇒ UNSAT (nested closure)" true
  | Vsat _ | Vunknown ->
    check "W1 real stack: x=y ∧ g(f(x))<g(f(y)) ⇒ UNSAT (nested closure)" false
;;

(* Category coverage for the e-graph membership rule (codex R4). The W1
   congruence-through-a-LIA-atom collision must be caught for uninterpreted applications
   of DIFFERENT shapes than the unary Int→Int [f] above, so a CATEGORY-SELECTIVE
   under-inclusion in [internalize_uf_subterms] — one that internalizes only unary apps,
   or only apps with Int arguments — cannot slip past the suite (the registry mutant
   already covers the drop-unary case; these pin the arity and argument-sort dimensions).
   Two shapes, each occurring ONLY inside a LIA order atom (so visibility rides the
   membership internalize): an ARITY-2 application, and an application whose ARGUMENTS are
   uninterpreted-SORTED. *)
let test_w1_real_arity2 () =
  let f = fixture () in
  let x = const f "x"
  and y = const f "y"
  and u = const f "u"
  and v = const f "v" in
  let gg = Env.declare_fun f.env "g2" (Rank.create [ Sort.int; Sort.int ] Sort.int) in
  let gxy = Context.app f.ctx gg [ x; y ]
  and guv = Context.app f.ctx gg [ u; v ] in
  (* x=u ∧ y=v ∧ g(x,y) < g(u,v) — UNSAT: the two equalities ⇒ (EUF, pairwise-arg
     congruence) g(x,y)=g(u,v), against the strict order. Exercises an arity-2 boundary
     application. *)
  let formula =
    [ Context.eq f.ctx x u, true
    ; Context.eq f.ctx y v, true
    ; Context.lt f.ctx gxy guv, true
    ]
  in
  (match Driver_real.solve ~cells:full_assignment_cells f formula with
   | Vunsat -> check "W1 real (arity-2): x=u ∧ y=v ∧ g(x,y)<g(u,v) ⇒ UNSAT" true
   | Vsat _ | Vunknown ->
     check "W1 real (arity-2): x=u ∧ y=v ∧ g(x,y)<g(u,v) ⇒ UNSAT" false);
  (* discriminator: drop the equalities ⇒ SAT (the UNSAT above is the congruence, not a
     stack that certifies UNSAT unconditionally). *)
  let g = fixture () in
  let x = const g "x"
  and y = const g "y"
  and u = const g "u"
  and v = const g "v" in
  let gg = Env.declare_fun g.env "g2" (Rank.create [ Sort.int; Sort.int ] Sort.int) in
  let gxy = Context.app g.ctx gg [ x; y ]
  and guv = Context.app g.ctx gg [ u; v ] in
  match
    Driver_real.solve ~cells:full_assignment_cells g [ Context.lt g.ctx gxy guv, true ]
  with
  | Vsat _ -> check "W1 real (arity-2): g(x,y)<g(u,v) alone ⇒ SAT" true
  | Vunsat | Vunknown -> check "W1 real (arity-2): g(x,y)<g(u,v) alone ⇒ SAT" false
;;

let test_w1_real_usort_args () =
  let f = fixture () in
  let u_sort = Sort.uninterpreted (Env.declare_sort f.env "U") in
  let uconst nm =
    Context.const f.ctx (Env.declare_fun f.env nm (Rank.create [] u_sort))
  in
  let a = uconst "a"
  and b = uconst "b" in
  let ff = Env.declare_fun f.env "fu" (Rank.create [ u_sort ] Sort.int) in
  let fa = Context.app f.ctx ff [ a ]
  and fb = Context.app f.ctx ff [ b ] in
  (* a=b (uninterpreted-sort equality, routed EUF) ∧ f(a) < f(b) (LIA order, Int codomain)
     — UNSAT: a=b ⇒ (EUF) f(a)=f(b), against the strict order. Exercises a boundary
     application whose ARGUMENTS are uninterpreted-sorted (not Int). *)
  let formula = [ Context.eq f.ctx a b, true; Context.lt f.ctx fa fb, true ] in
  (match Driver_real.solve ~cells:full_assignment_cells f formula with
   | Vunsat -> check "W1 real (U-sort args): a=b ∧ f(a)<f(b) ⇒ UNSAT" true
   | Vsat _ | Vunknown -> check "W1 real (U-sort args): a=b ∧ f(a)<f(b) ⇒ UNSAT" false);
  let g = fixture () in
  let u_sort = Sort.uninterpreted (Env.declare_sort g.env "U") in
  let uconst nm =
    Context.const g.ctx (Env.declare_fun g.env nm (Rank.create [] u_sort))
  in
  let a = uconst "a"
  and b = uconst "b" in
  let ff = Env.declare_fun g.env "fu" (Rank.create [ u_sort ] Sort.int) in
  let fa = Context.app g.ctx ff [ a ]
  and fb = Context.app g.ctx ff [ b ] in
  match
    Driver_real.solve ~cells:full_assignment_cells g [ Context.lt g.ctx fa fb, true ]
  with
  | Vsat _ -> check "W1 real (U-sort args): f(a)<f(b) alone ⇒ SAT" true
  | Vunsat | Vunknown -> check "W1 real (U-sort args): f(a)<f(b) alone ⇒ SAT" false
;;

(* Part 3 — Bool-boundary through the REAL stack (internalization ADR §3.6). *)

let bfun f name = Env.declare_fun f.env name (Rank.create [ Sort.bool ] Sort.int)
let pfun f name = Env.declare_fun f.env name (Rank.create [ Sort.int ] Sort.bool)

let bvar f name =
  Context.const f.ctx (Env.declare_fun f.env name (Rank.create [] Sort.bool))
;;

(* Case (i) leaf + (i') constant, and the codex-H2 buried-leaf boundary. h : Bool → Int.
   Three companion shapes pin the exact H2 ruling (surfaced/bound leaf stays decidable;
   buried/unbound leaf degrades):
   - ¬b ∧ h(b)≠h(false) → UNSAT: ¬b SURFACES b (a SAT atom), routing b=false_const into
     EUF (Predicate/K_bool); h(false) is native EUF (false = false_const, §3.1), so
     congruence fires h(b)=h(false) against the asserted disequality.
   - b ∧ h(b)≠h(false) → SAT: b surfaced true, so b≠false_const (true≠false axiom); the
     h-arguments differ, no congruence, and — b being BOUND — no degrade.
   - h(b)≠h(false) ALONE → UNKNOWN: b is BURIED (only under h, never a SAT atom) and
     unbound in EUF, so the combinator cannot soundly certify (it would keep b a third
     Boolean class) and degrades via Combine.Incomplete (codex H2; team-lead ruling). *)
let test_bool_leaf_real () =
  let leaf_h () =
    let f = fixture () in
    let b = bvar f "b" in
    let h = bfun f "h" in
    let hb = Context.app f.ctx h [ b ]
    and hfalse = Context.app f.ctx h [ Context.bool_const f.ctx false ] in
    f, b, Context.eq f.ctx hb hfalse
  in
  let f, b, hb_ne_hfalse = leaf_h () in
  (match
     Driver_real.solve ~cells:full_assignment_cells f [ b, false; hb_ne_hfalse, false ]
   with
   | Vunsat ->
     check "Bool leaf real stack: ¬b ∧ h(b)≠h(false) ⇒ UNSAT (b bound false)" true
   | Vsat _ | Vunknown ->
     check "Bool leaf real stack: ¬b ∧ h(b)≠h(false) ⇒ UNSAT (b bound false)" false);
  let f, b, hb_ne_hfalse = leaf_h () in
  (match
     Driver_real.solve ~cells:full_assignment_cells f [ b, true; hb_ne_hfalse, false ]
   with
   | Vsat _ -> check "Bool leaf real stack: b ∧ h(b)≠h(false) ⇒ SAT (b bound true)" true
   | Vunsat | Vunknown ->
     check "Bool leaf real stack: b ∧ h(b)≠h(false) ⇒ SAT (b bound true)" false);
  let f, _b, hb_ne_hfalse = leaf_h () in
  match Driver_real.solve ~cells:full_assignment_cells f [ hb_ne_hfalse, false ] with
  | Vunknown ->
    check
      "Bool leaf real stack: h(b)≠h(false) alone ⇒ UNKNOWN (buried unbound b, H2)"
      true
  | Vsat _ | Vunsat ->
    check
      "Bool leaf real stack: h(b)≠h(false) alone ⇒ UNKNOWN (buried unbound b, H2)"
      false
;;

(* Case (ii): ¬b ∧ h(b∧c) ≠ h(false) — genuinely UNSAT, but the leaf bridge cannot couple
   the opaque compound to b,c, so the walk DEGRADES to UNKNOWN (sound, via
   Combine.Incomplete). *)
let test_bool_compound_real () =
  let f = fixture () in
  let b = bvar f "b"
  and c = bvar f "c" in
  let h = bfun f "h" in
  let hbc = Context.app f.ctx h [ Context.and_ f.ctx [ b; c ] ]
  and hfalse = Context.app f.ctx h [ Context.bool_const f.ctx false ] in
  let formula = [ b, false; Context.eq f.ctx hbc hfalse, false ] in
  (match Driver_real.solve ~cells:full_assignment_cells f formula with
   | Vunknown ->
     check "Bool compound real stack: ¬b ∧ h(b∧c)≠h(false) ⇒ UNKNOWN (degrade)" true
   | Vsat _ | Vunsat ->
     check "Bool compound real stack: ¬b ∧ h(b∧c)≠h(false) ⇒ UNKNOWN (degrade)" false);
  (* the old v6 fixture also degrades (it too is a compound under a UF argument) *)
  let g = fixture () in
  let b = bvar g "b"
  and c = bvar g "c" in
  let h = bfun g "h" in
  let hbc = Context.app g.ctx h [ Context.and_ g.ctx [ b; c ] ]
  and htrue = Context.app g.ctx h [ Context.bool_const g.ctx true ] in
  (* assert the conjunction as the clausifier would — b and c as separate unit literals,
     never a raw [And] node (a connective, not a theory atom) *)
  let formula = [ b, true; c, true; Context.eq g.ctx hbc htrue, false ] in
  match Driver_real.solve ~cells:full_assignment_cells g formula with
  | Vunknown ->
    check "Bool compound real stack: (b∧c) ∧ h(b∧c)≠h(true) ⇒ UNKNOWN (degrade)" true
  | Vsat _ | Vunsat ->
    check "Bool compound real stack: (b∧c) ∧ h(b∧c)≠h(true) ⇒ UNKNOWN (degrade)" false
;;

(* codex FIX-ROUND fixtures — the two HIGH wrong-SAT triggers, now corrected. *)

(* H1: a bare Int variable occurring ONLY as an (dis)equality side got no EUF-use bit, so
   the interface stayed empty and the disagreement was missed. Trigger: (distinct x y) ∧
   x≤y ∧ y≤x — LIA entails x=y, EUF holds x≠y (the diseq routes to EUF only, S1). With x,y
   now EUF-used (equality operands) AND lia_used (arith), they enter the interface, the
   disagreement splits, and every branch is refuted → UNSAT. *)
let test_h1_distinct_bare_vars_real () =
  let f = fixture () in
  let x = const f "x"
  and y = const f "y" in
  let formula =
    [ Context.eq f.ctx x y, false (* (distinct x y) = x ≠ y *)
    ; Context.le f.ctx x y, true
    ; Context.le f.ctx y x, true
    ]
  in
  (match Driver_real.solve ~cells:full_assignment_cells f formula with
   | Vunsat -> check "H1 real stack: (distinct x y) ∧ x≤y ∧ y≤x ⇒ UNSAT" true
   | Vsat _ | Vunknown -> check "H1 real stack: (distinct x y) ∧ x≤y ∧ y≤x ⇒ UNSAT" false);
  (* discriminator — DROP y≤x: (distinct x y) ∧ x≤y is SAT (x<y) *)
  let g = fixture () in
  let x = const g "x"
  and y = const g "y" in
  match
    Driver_real.solve
      ~cells:full_assignment_cells
      g
      [ Context.eq g.ctx x y, false; Context.le g.ctx x y, true ]
  with
  | Vsat _ -> check "H1 real stack: (distinct x y) ∧ x≤y alone ⇒ SAT (x<y)" true
  | Vunsat | Vunknown ->
    check "H1 real stack: (distinct x y) ∧ x≤y alone ⇒ SAT (x<y)" false
;;

(* H2: a bare Bool leaf buried under a UF argument, never surfaced as a SAT atom, could
   stay a third opaque EUF Boolean class. Trigger: h(b)≠h(true) ∧ h(b)≠h(false),
   h:Bool→Int — genuinely UNSAT (b is true or false, forcing one congruence), but the
   combinator can only see b as opaque. Sound outcome under the ruling: degrade to UNKNOWN
   (buried unbound → Incomplete), never wrong-SAT. *)
let test_h2_buried_bool_leaf_real () =
  let f = fixture () in
  let b = bvar f "b" in
  let h = bfun f "h" in
  let hb = Context.app f.ctx h [ b ] in
  let htrue = Context.app f.ctx h [ Context.bool_const f.ctx true ]
  and hfalse = Context.app f.ctx h [ Context.bool_const f.ctx false ] in
  let formula = [ Context.eq f.ctx hb htrue, false; Context.eq f.ctx hb hfalse, false ] in
  match Driver_real.solve ~cells:full_assignment_cells f formula with
  | Vunknown ->
    check
      "H2 real stack: h(b)≠h(true) ∧ h(b)≠h(false) ⇒ UNKNOWN (buried leaf, no wrong-SAT)"
      true
  | Vsat _ | Vunsat ->
    check
      "H2 real stack: h(b)≠h(true) ∧ h(b)≠h(false) ⇒ UNKNOWN (buried leaf, no wrong-SAT)"
      false
;;

(* H2 sibling: the same hole for a buried Bool-RETURNING uninterpreted application g(x),
   g:Int→Bool, under h. h(g(x))≠h(true) ∧ h(g(x))≠h(false) — g(x) is opaque (never a
   surfaced predicate atom) → degrade to UNKNOWN (sound). *)
let test_h2_buried_bool_uf_real () =
  let f = fixture () in
  let x = const f "x" in
  let g = pfun f "g" in
  let h = bfun f "h" in
  let gx = Context.app f.ctx g [ x ] in
  let hgx = Context.app f.ctx h [ gx ] in
  let htrue = Context.app f.ctx h [ Context.bool_const f.ctx true ]
  and hfalse = Context.app f.ctx h [ Context.bool_const f.ctx false ] in
  let formula =
    [ Context.eq f.ctx hgx htrue, false; Context.eq f.ctx hgx hfalse, false ]
  in
  match Driver_real.solve ~cells:full_assignment_cells f formula with
  | Vunknown ->
    check "H2-sib real stack: h(g x)≠h(true) ∧ h(g x)≠h(false) ⇒ UNKNOWN (buried UF)" true
  | Vsat _ | Vunsat ->
    check
      "H2-sib real stack: h(g x)≠h(true) ∧ h(g x)≠h(false) ⇒ UNKNOWN (buried UF)"
      false
;;

(* KNOWN-GAP fixture (board #42, codex delta MEDIUM) — the grow-only [bool_uf_args] set is
   not retracted on [pop], so a Bool-UF-arg recorded before a pop lingers and, unbound
   after the pop, forces a SPURIOUS degrade. Sequence: push; assert h(b)≠h(false); pop;
   check — after the pop the problem is EMPTY (the disequality is retracted), so it is
   GENUINELY SAT, but the stale [b] in [bool_uf_args] is unbound at Sat certification →
   UNKNOWN. This is a completeness gap (never a wrong SAT/UNSAT), pinned here as KNOWN, in
   the same grow-only-vs-pop class as M1. FLIP the expectation to Vsat when the
   grow-only/activity reconciliation lands (board #42). Driven directly against the
   combined theory so the push/pop lifecycle is real. *)
let test_pop_stale_bool_uf_arg_known_gap () =
  let f = fixture () in
  let b = bvar f "b" in
  let h = bfun f "h" in
  let hb = Context.app f.ctx h [ b ]
  and hfalse = Context.app f.ctx h [ Context.bool_const f.ctx false ] in
  let t = Cuflia_real.create f.ctx f.env in
  let a = fresh_atom f in
  Cuflia_real.push t;
  Cuflia_real.register_atom t a (Context.eq f.ctx hb hfalse);
  Cuflia_real.assert_lit t (Lit.make a false);
  Cuflia_real.pop t 1;
  let verdict =
    try
      match Cuflia_real.check t Th.Final with
      | Th.Sat -> `Sat
      | Th.Conflict _ -> `Unsat
      | Th.Split _ | Th.Propagations _ -> `Other
    with
    | Oxsmt_combine.Combine.Incomplete _ -> `Unknown
  in
  check
    "KNOWN GAP #42: push;h(b)≠h(false);pop;check ⇒ UNKNOWN today (grow-only \
     bool_uf_args; flip to SAT on reconciliation)"
    (match verdict with
     | `Unknown -> true
     | `Sat | `Unsat | `Other -> false)
;;

(* Part 4 — the ADR §6 acceptance corpus through the REAL stack (Euf_adapter +
   Lia_adapter, no mocks). Every fixture is the internalization design's load-bearing
   evidence. *)

let kfun f name usort = Env.declare_fun f.env name (Rank.create [ Sort.int ] usort)

(* codex round-7 R1: x=0 ∧ f(x+1)<f(1) — UNSAT. The boundary node [x+1] (LIA under the EUF
   [f]) is exactly what round-7's euf_domain gate over-excluded; the interface set
   includes it by construction, so the (x+1 = 1) disagreement is found and split, EUF then
   closes f(x+1)=f(1) against the strict order. *)
let test_r1_real () =
  let f = fixture () in
  let x = const f "x" in
  let ff = ufun f "f" in
  let xp1 = Context.add f.ctx x (Context.int_const f.ctx 1) in
  let fxp1 = Context.app f.ctx ff [ xp1 ]
  and f1 = Context.app f.ctx ff [ Context.int_const f.ctx 1 ] in
  let formula =
    [ Context.eq f.ctx x (Context.int_const f.ctx 0), true
    ; Context.lt f.ctx fxp1 f1, true
    ]
  in
  (match Driver_real.solve ~cells:full_assignment_cells f formula with
   | Vunsat -> check "R1 real stack: x=0 ∧ f(x+1)<f(1) ⇒ UNSAT" true
   | Vsat _ | Vunknown -> check "R1 real stack: x=0 ∧ f(x+1)<f(1) ⇒ UNSAT" false);
  (* discriminator — DROP x=0: f(x+1)<f(1) alone is SAT *)
  let g = fixture () in
  let x = const g "x" in
  let ff = ufun g "f" in
  let xp1 = Context.add g.ctx x (Context.int_const g.ctx 1) in
  let fxp1 = Context.app g.ctx ff [ xp1 ]
  and f1 = Context.app g.ctx ff [ Context.int_const g.ctx 1 ] in
  match
    Driver_real.solve ~cells:full_assignment_cells g [ Context.lt g.ctx fxp1 f1, true ]
  with
  | Vsat _ -> check "R1 real stack: f(x+1)<f(1) alone ⇒ SAT (no spurious UNSAT)" true
  | Vunsat | Vunknown ->
    check "R1 real stack: f(x+1)<f(1) alone ⇒ SAT (no spurious UNSAT)" false
;;

(* second dual-leg repro: x=y ∧ f(x+1)<f(y+1) — UNSAT (boundary nodes x+1,y+1 under f,
   plus f(x+1),f(y+1) under the order atom). *)
let test_fx1_fy1_real () =
  let f = fixture () in
  let x = const f "x"
  and y = const f "y" in
  let ff = ufun f "f" in
  let xp1 = Context.add f.ctx x (Context.int_const f.ctx 1)
  and yp1 = Context.add f.ctx y (Context.int_const f.ctx 1) in
  let fxp1 = Context.app f.ctx ff [ xp1 ]
  and fyp1 = Context.app f.ctx ff [ yp1 ] in
  let formula = [ Context.eq f.ctx x y, true; Context.lt f.ctx fxp1 fyp1, true ] in
  match Driver_real.solve ~cells:full_assignment_cells f formula with
  | Vunsat -> check "real stack: x=y ∧ f(x+1)<f(y+1) ⇒ UNSAT" true
  | Vsat _ | Vunknown -> check "real stack: x=y ∧ f(x+1)<f(y+1) ⇒ UNSAT" false
;;

(* numeral corner: x=1 ∧ f(x)≠f(1) — UNSAT. The numeral [1] under [f] is a boundary node
   by construction (LIA under EUF), so the shared arrangement includes it. *)
let test_numeral_corner_real () =
  let f = fixture () in
  let x = const f "x" in
  let ff = ufun f "f" in
  let fx = Context.app f.ctx ff [ x ]
  and f1 = Context.app f.ctx ff [ Context.int_const f.ctx 1 ] in
  let formula =
    [ Context.eq f.ctx x (Context.int_const f.ctx 1), true
    ; Context.eq f.ctx fx f1, false
    ]
  in
  (match Driver_real.solve ~cells:full_assignment_cells f formula with
   | Vunsat -> check "numeral corner real stack: x=1 ∧ f(x)≠f(1) ⇒ UNSAT" true
   | Vsat _ | Vunknown -> check "numeral corner real stack: x=1 ∧ f(x)≠f(1) ⇒ UNSAT" false);
  (* discriminator — DROP x=1: f(x)≠f(1) alone is SAT *)
  let g = fixture () in
  let x = const g "x" in
  let ff = ufun g "f" in
  let fx = Context.app g.ctx ff [ x ]
  and f1 = Context.app g.ctx ff [ Context.int_const g.ctx 1 ] in
  match
    Driver_real.solve ~cells:full_assignment_cells g [ Context.eq g.ctx fx f1, false ]
  with
  | Vsat _ -> check "numeral corner real stack: f(x)≠f(1) alone ⇒ SAT" true
  | Vunsat | Vunknown -> check "numeral corner real stack: f(x)≠f(1) alone ⇒ SAT" false
;;

(* the fork the ruling nearly scoped out: an EUF-OWNED containing atom (a U-sorted diseq)
   with a buried LIA node — x=0 ∧ k(x+1)≠k(1), k : Int → U — UNSAT. The atom is EUF-owned
   (never seen by LIA), yet the walk descends into k's argument and records x+1,1 as
   boundary nodes, so the (x+1 = 1) split reaches EUF and closes k(x+1)=k(1). *)
let test_buried_lia_euf_owned_real () =
  let f = fixture () in
  let usort = Sort.uninterpreted (Env.declare_sort f.env "U") in
  let x = const f "x" in
  let k = kfun f "k" usort in
  let xp1 = Context.add f.ctx x (Context.int_const f.ctx 1) in
  let kxp1 = Context.app f.ctx k [ xp1 ]
  and k1 = Context.app f.ctx k [ Context.int_const f.ctx 1 ] in
  let formula =
    [ Context.eq f.ctx x (Context.int_const f.ctx 0), true
    ; Context.eq f.ctx kxp1 k1, false
    ]
  in
  match Driver_real.solve ~cells:full_assignment_cells f formula with
  | Vunsat -> check "buried-LIA real stack: x=0 ∧ k(x+1)≠k(1) ⇒ UNSAT" true
  | Vsat _ | Vunknown -> check "buried-LIA real stack: x=0 ∧ k(x+1)≠k(1) ⇒ UNSAT" false
;;

(* predicate variant: x=0 ∧ p(x+1) ∧ ¬p(1), p : Int → Bool — UNSAT (same buried-LIA node
   x+1, now under a Bool-codomain application; congruence on p closes p(x+1)=p(1)). *)
let test_predicate_variant_real () =
  let f = fixture () in
  let x = const f "x" in
  let p = pfun f "p" in
  let xp1 = Context.add f.ctx x (Context.int_const f.ctx 1) in
  let pxp1 = Context.app f.ctx p [ xp1 ]
  and p1 = Context.app f.ctx p [ Context.int_const f.ctx 1 ] in
  let formula =
    [ Context.eq f.ctx x (Context.int_const f.ctx 0), true; pxp1, true; p1, false ]
  in
  (match Driver_real.solve ~cells:full_assignment_cells f formula with
   | Vunsat -> check "predicate variant real stack: x=0 ∧ p(x+1) ∧ ¬p(1) ⇒ UNSAT" true
   | Vsat _ | Vunknown ->
     check "predicate variant real stack: x=0 ∧ p(x+1) ∧ ¬p(1) ⇒ UNSAT" false);
  (* discriminator — DROP x=0: p(x+1) ∧ ¬p(1) alone is SAT *)
  let g = fixture () in
  let x = const g "x" in
  let p = pfun g "p" in
  let xp1 = Context.add g.ctx x (Context.int_const g.ctx 1) in
  let pxp1 = Context.app g.ctx p [ xp1 ]
  and p1 = Context.app g.ctx p [ Context.int_const g.ctx 1 ] in
  match Driver_real.solve ~cells:full_assignment_cells g [ pxp1, true; p1, false ] with
  | Vsat _ -> check "predicate variant real stack: p(x+1) ∧ ¬p(1) alone ⇒ SAT" true
  | Vunsat | Vunknown ->
    check "predicate variant real stack: p(x+1) ∧ ¬p(1) alone ⇒ SAT" false
;;

(* mixed-equality totality (§6): the walk must descend BOTH sides of an equality. Here the
   only shared arithmetic crossing is buried in the RIGHT side of an EUF-owned equality
   f(x) = a+1 (a+1 is LIA under the neutral Eq node); with a=0 ∧ f(0)≠f(1), forcing a+1 =
   1 = x closes f(x)=f(1). A walk that descended only the left side would miss a+1, never
   split, and wrong-SAT. *)
let test_mixed_equality_totality_real () =
  let f = fixture () in
  let a = const f "a" in
  let ff = ufun f "f" in
  let ap1 = Context.add f.ctx a (Context.int_const f.ctx 1) in
  let f0 = Context.app f.ctx ff [ Context.int_const f.ctx 0 ]
  and f1 = Context.app f.ctx ff [ Context.int_const f.ctx 1 ] in
  (* a=0 ∧ f(0)=f(a+1) ∧ f(a+1)≠f(1): a+1 folds to 1 (a=0), so f(a+1)=f(1) by congruence,
     contradicting f(a+1)≠f(1). The crossing a+1 lives only on the right side of the
     middle equality — the walk must record it. *)
  let formula =
    [ Context.eq f.ctx a (Context.int_const f.ctx 0), true
    ; Context.eq f.ctx f0 (Context.app f.ctx ff [ ap1 ]), true
    ; Context.eq f.ctx (Context.app f.ctx ff [ ap1 ]) f1, false
    ]
  in
  match Driver_real.solve ~cells:full_assignment_cells f formula with
  | Vunsat ->
    check "mixed-eq totality real stack: right-side crossing a+1 recorded ⇒ UNSAT" true
  | Vsat _ | Vunknown ->
    check "mixed-eq totality real stack: right-side crossing a+1 recorded ⇒ UNSAT" false
;;

(* deep tower: depth-d g(f(g(f(…)))) under a strict order with x=y — UNSAT within the
   per-ground-check split budget (the driver's depth guard is 64). *)
let test_deep_tower_real () =
  let f = fixture () in
  let x = const f "x"
  and y = const f "y" in
  let ff = ufun f "f"
  and gg = ufun f "g" in
  let rec tower n t =
    if n = 0
    then t
    else tower (n - 1) (Context.app f.ctx (if n mod 2 = 0 then ff else gg) [ t ])
  in
  let tx = tower 6 x
  and ty = tower 6 y in
  let formula = [ Context.eq f.ctx x y, true; Context.lt f.ctx tx ty, true ] in
  match Driver_real.solve ~cells:full_assignment_cells f formula with
  | Vunsat ->
    check "deep tower real stack: depth-6 tower, x=y ⇒ UNSAT (within budget)" true
  | Vsat _ | Vunknown ->
    check "deep tower real stack: depth-6 tower, x=y ⇒ UNSAT (within budget)" false
;;

(* pure-QF_LIA perf fixture: no uninterpreted symbol ⇒ empty interface ⇒ ZERO arrangement
   splits (the G1 fix by construction — owner(Le)=Both's O(N²) blow-up is dead, not
   relocated). Uses the LAZY driver so a stray split would show up as splits_seen>0. *)
let test_pure_qf_lia_zero_splits () =
  let f = fixture () in
  let vars = List.init 6 (fun i -> const f (Printf.sprintf "v%d" i)) in
  (* a satisfiable chain of order + equality atoms over pure LIA variables *)
  let formula =
    List.map2
      (fun a b -> Context.le f.ctx a b, true)
      (List.filteri (fun i _ -> i < 5) vars)
      (List.filteri (fun i _ -> i > 0) vars)
  in
  splits_seen := 0;
  let v = Driver_real.solve f formula in
  check
    "pure-QF_LIA real stack: SAT chain decided"
    (match v with
     | Vsat _ -> true
     | Vunsat | Vunknown -> false);
  check
    "pure-QF_LIA real stack: EMPTY interface ⇒ ZERO arrangement splits"
    (!splits_seen = 0)
;;

(* push/pop-reassert (invariant (i)/(ii)): a mixed goal asserted, decided, then the SAME
   goal re-driven on a fresh stack gives the identical verdict — grow-only, idempotent
   re-registration of the same hash-consed nodes. *)
let test_push_pop_reassert_real () =
  let build () =
    let f = fixture () in
    let x = const f "x" in
    let ff = ufun f "f" in
    let xp1 = Context.add f.ctx x (Context.int_const f.ctx 1) in
    let fxp1 = Context.app f.ctx ff [ xp1 ]
    and f1 = Context.app f.ctx ff [ Context.int_const f.ctx 1 ] in
    ( f
    , [ Context.eq f.ctx x (Context.int_const f.ctx 0), true
      ; Context.lt f.ctx fxp1 f1, true
      ] )
  in
  let f1, formula1 = build () in
  let f2, formula2 = build () in
  let v1 = Driver_real.solve ~cells:full_assignment_cells f1 formula1 in
  let v2 = Driver_real.solve ~cells:full_assignment_cells f2 formula2 in
  let same =
    match v1, v2 with
    | Vunsat, Vunsat -> true
    | _ -> false
  in
  check "push/pop-reassert real stack: identical verdict across a fresh re-drive" same
;;

(* use-history transition (P6, invariant (ii) reformulation): incremental. First check
   with x LIA-only is SAT; after asserting f(x)≠f(0), x becomes a both-used interface
   member and the check must FLIP to UNSAT — catching a tag-memoized "x is not an
   interface member" classification that misses the transition. Driven directly against
   the combined theory (not the one-shot [solve]) so the two checks share one incremental
   state. *)
let test_use_history_transition_real () =
  let f = fixture () in
  let x = const f "x" in
  let ff = ufun f "f" in
  let t = Cuflia_real.create f.ctx f.env in
  let atom_tbl : Atom.t Term.Table.t = Term.Table.create 16 in
  let atom_of term =
    match Term.Table.find_opt atom_tbl term with
    | Some a -> a
    | None ->
      let a = fresh_atom f in
      Term.Table.replace atom_tbl term a;
      Cuflia_real.register_atom t a term;
      a
  in
  let assert_term term sign = Cuflia_real.assert_lit t (Lit.make (atom_of term) sign) in
  (* solve to a fixed point via the full-assignment split cells, sharing state [t] *)
  let rec drive depth : verdict =
    if depth > 64
    then raise Driver_overflow
    else (
      match Cuflia_real.check t Th.Final with
      | Th.Conflict _ -> Vunsat
      | Th.Sat -> Vsat (Cuflia_real.model t)
      | Th.Propagations lits ->
        List.iter (fun l -> Cuflia_real.assert_lit t l) lits;
        drive (depth + 1)
      | Th.Split terms ->
        (match full_assignment_cells terms with
         | cell :: _ ->
           (* enough for this fixture: the flip is derivable on the eq branch, and the
              first check has no split at all *)
           List.iter (fun (tm, sg) -> assert_term tm sg) cell;
           drive (depth + 1)
         | [] -> Vunsat))
  in
  (* x ≤ 0 ∧ -x ≤ 0 (x is LIA-only here) *)
  assert_term (Context.le f.ctx x (Context.int_const f.ctx 0)) true;
  assert_term (Context.le f.ctx (Context.neg f.ctx x) (Context.int_const f.ctx 0)) true;
  let first =
    match Cuflia_real.check t Th.Final with
    | Th.Sat -> true
    | _ -> false
  in
  check "use-history transition: first check (x LIA-only) ⇒ SAT" first;
  (* now assert f(x) ≠ f(0): x becomes both-used; x=0 (from the bounds) ⇒ f(x)=f(0) *)
  let fx = Context.app f.ctx ff [ x ]
  and f0 = Context.app f.ctx ff [ Context.int_const f.ctx 0 ] in
  assert_term (Context.eq f.ctx fx f0) false;
  let second =
    match drive 0 with
    | Vunsat -> true
    | _ -> false
  in
  check
    "use-history transition: after f(x)≠f(0), x is a both-used member ⇒ flip to UNSAT"
    second
;;

(* GUARD-DROP PIN (build-digest deviation, team-lead-approved): the build plan's step-4
   boundary rule carried an extra `parent_owner <> Neutral` clause that EXCLUDED the owned
   sides of an equality atom (an [Eq] node is Neutral). That contradicts ADR §3.1 (a
   crossing is recorded per parent→child EDGE) and the §6 mixed-equality totality test,
   and it is unsound at the MODEL level: f(a)≠f(b) ∧ a≤b is SAT, but with the guard
   f(a),f(b) never enter the interface, the EUF/LIA disagreement on them is never found,
   and the merged model takes LIA's free (equal) values for f(a),f(b) — violating the
   asserted f(a)≠f(b). This pins the fix: the merged model MUST satisfy the formula; a
   reviewer who re-adds `parent_owner <> Neutral` makes this go red.

   Driven through the TOY stack (not the real Euf/Lia): the toy LIA totalizes a value for
   every registered App variable, so an owned Eq-side dropped from the interface surfaces
   as a concrete f(a)=f(b) in the merged model — the observable inconsistency. The real
   Lia_adapter leaves an unconstrained App term unvalued, so its model OMITS f(a),f(b)
   (codex C3 pure-EUF-Int-term case) and cannot exhibit the fault, which is why the
   original empirical demonstration was the toy layer. Distinct shape from the f(x)=x+y
   mixed-equality test: here the owned sides are under a NEGATED Int equality whose diseq
   routes to EUF only (S1), so LIA is never told them apart. *)
let test_owned_eq_sides_guard_toy () =
  let f = fixture () in
  let a = const f "a"
  and b = const f "b" in
  let ff = ufun f "f" in
  let fa = Context.app f.ctx ff [ a ]
  and fb = Context.app f.ctx ff [ b ] in
  let formula = [ Context.eq f.ctx fa fb, false; Context.le f.ctx a b, true ] in
  match solve ~cells:full_assignment_cells f formula with
  | Vsat m ->
    check "guard-drop pin: f(a)≠f(b) ∧ a≤b ⇒ SAT" true;
    check
      "guard-drop pin: merged model SATISFIES f(a)≠f(b) (owned Eq-sides in the interface)"
      (model_satisfies m formula)
  | Vunsat | Vunknown ->
    check "guard-drop pin: f(a)≠f(b) ∧ a≤b ⇒ SAT" false;
    check
      "guard-drop pin: merged model SATISFIES f(a)≠f(b) (owned Eq-sides in the interface)"
      false
;;

(* ================================================================================== *)
(* ADR-0014 Stage 1b — the theory fabric (justified LIA fixed-value equality injection). *)

module Fab = Cuflia_real

(* Drive a conjunction of unit literals through a REAL combined instance to a Final
   verdict, capturing every emitted fabric event and the injected-edge count. The fixed-
   value fabric fixtures resolve at [Final] without a Split (the injection conflicts or is
   consistent before any arrangement split), so a [Split] here is itself a failure. *)
let fab_run ?(trace = true) f (formula : (Term.t * bool) list) =
  let t = Fab.create f.ctx f.env in
  let events = ref [] in
  if trace
  then
    Fab.set_fabric_trace
      t
      (Some { Fabric.on_fabric_eq = (fun e -> events := e :: !events) });
  let atoms : Atom.t Term.Table.t = Term.Table.create 32 in
  let atom_of term =
    match Term.Table.find_opt atoms term with
    | Some a -> a
    | None ->
      let a = fresh_atom f in
      Term.Table.replace atoms term a;
      Fab.register_atom t a term;
      a
  in
  let assert_tm term sign = Fab.assert_lit t (Lit.make (atom_of term) sign) in
  List.iter (fun (term, sign) -> assert_tm term sign) formula;
  let rec loop depth =
    if depth > 64
    then `Unknown
    else (
      match Fab.check t Th.Final with
      | Th.Conflict _ -> `Unsat
      | Th.Sat -> `Sat
      | Th.Propagations lits ->
        List.iter (fun l -> Fab.assert_lit t l) lits;
        loop (depth + 1)
      | Th.Split _ -> `Split)
  in
  let v =
    try loop 0 with
    | Cmb.Combination_unsound _ -> `Unknown
  in
  v, List.rev !events, (Fab.fabric_stats t).Fab.edges_injected
;;

(* Two shared Int terms fixed by LIA BOUNDS (not EUF-visible equalities) to the same value
   makes [f(x)≠f(y)] UNSAT — the fabric injects [x=y] into the hub, congruence closes
   [f(x)~f(y)], and the diseq conflicts. WITHOUT the injection EUF never learns [x~y] at
   [Final] without a split; the fabric removes that round-trip. Also the F7 acceptance: a
   fabric UNSAT records ONE well-formed [on_fabric_eq] event carrying the 4 oriented bound
   literals as Γ. KILLS weak-Γ (verifier refuses → 0 edges → not UNSAT) and confirms the
   [Conflict] premises are real [Lit.t]s (handle-leak-via-check → the frozen seam would
   never typecheck a bare handle; the expansion chokepoint is what makes it real). *)
let test_fabric_fixed_value_unsat () =
  let f = fixture () in
  let x = const f "x"
  and y = const f "y" in
  let ff = ufun f "f" in
  let five = Context.int_const f.ctx 5 in
  let formula =
    [ Context.le f.ctx x five, true
    ; Context.ge f.ctx x five, true
    ; Context.le f.ctx y five, true
    ; Context.ge f.ctx y five, true
    ; Context.eq f.ctx (Context.app f.ctx ff [ x ]) (Context.app f.ctx ff [ y ]), false
    ]
  in
  let v, events, edges = fab_run f formula in
  check "fabric: bound-fixed x,y ∧ f(x)≠f(y) ⇒ UNSAT" (v = `Unsat);
  check "fabric: exactly one edge injected" (edges = 1);
  check "fabric: exactly one on_fabric_eq event" (List.length events = 1);
  match events with
  | [ e ] ->
    check
      "fabric: event equates x and y"
      ((Term.equal e.Fabric.s x && Term.equal e.Fabric.t y)
       || (Term.equal e.Fabric.s y && Term.equal e.Fabric.t x));
    check
      "fabric: Γ carries the 4 oriented bound literals"
      (List.length e.Fabric.gamma = 4)
  | _ -> check "fabric: event well-formed" false
;;

(* Soundness: two shared terms fixed to DIFFERENT values are never equated — no injection,
   [f(x)≠f(y)] stays SAT. A mutant that equated on presence-in-map rather than equal value
   would inject [x=y] and wrong-UNSAT this. *)
let test_fabric_distinct_values_sat () =
  let f = fixture () in
  let x = const f "x"
  and y = const f "y" in
  let ff = ufun f "f" in
  let formula =
    [ Context.le f.ctx x (Context.int_const f.ctx 5), true
    ; Context.ge f.ctx x (Context.int_const f.ctx 5), true
    ; Context.le f.ctx y (Context.int_const f.ctx 6), true
    ; Context.ge f.ctx y (Context.int_const f.ctx 6), true
    ; Context.eq f.ctx (Context.app f.ctx ff [ x ]) (Context.app f.ctx ff [ y ]), false
    ]
  in
  let v, _events, edges = fab_run f formula in
  check "fabric: x=5,y=6 ∧ f(x)≠f(y) ⇒ SAT (no false equate)" (v = `Sat);
  check "fabric: no edge injected for distinct values" (edges = 0)
;;

(* Soundness: the offset case. [x] and [x+1] share one simplex variable (the const rides
   the bound); with [x] fixed to 5 the fabric must see [x+1] fixed to 6, NOT 5, and NOT
   equate [x] with [x+1]. [f(x)≠f(x+1)] is SAT. A [fixed_bounds] that dropped the const
   would inject [x=x+1] and wrong-UNSAT. *)
let test_fabric_const_offset_sat () =
  let f = fixture () in
  let x = const f "x" in
  let ff = ufun f "f" in
  let xp1 = Context.add f.ctx x (Context.int_const f.ctx 1) in
  let formula =
    [ Context.le f.ctx x (Context.int_const f.ctx 5), true
    ; Context.ge f.ctx x (Context.int_const f.ctx 5), true
    ; Context.eq f.ctx (Context.app f.ctx ff [ x ]) (Context.app f.ctx ff [ xp1 ]), false
    ]
  in
  let v, _events, _edges = fab_run f formula in
  check "fabric: x=5 ∧ f(x)≠f(x+1) ⇒ SAT (const offset respected)" (v = `Sat)
;;

(* F3: the injected edge is origin-frame trailed — a [pop] over the frame that fixed [y]
   drops the merge AND its registry entry, so the verdict flips back and a re-assert
   restores it. A strand-the-edge mutant (edge/merge survives the pop) wrong-UNSATs the
   post-pop check; a stale-owner mutant (untrailed [propagated_by]) misroutes explain. *)
let test_fabric_pop_reassert () =
  let f = fixture () in
  let x = const f "x"
  and y = const f "y" in
  let ff = ufun f "f" in
  let five = Context.int_const f.ctx 5 in
  let t = Fab.create f.ctx f.env in
  let atoms : Atom.t Term.Table.t = Term.Table.create 32 in
  let atom_of term =
    match Term.Table.find_opt atoms term with
    | Some a -> a
    | None ->
      let a = fresh_atom f in
      Term.Table.replace atoms term a;
      Fab.register_atom t a term;
      a
  in
  let assert_tm term sign = Fab.assert_lit t (Lit.make (atom_of term) sign) in
  let fx = Context.app f.ctx ff [ x ]
  and fy = Context.app f.ctx ff [ y ] in
  (* base frame: x fixed to 5, f(x)≠f(y), y free ⇒ SAT (no injection). *)
  assert_tm (Context.le f.ctx x five) true;
  assert_tm (Context.ge f.ctx x five) true;
  assert_tm (Context.eq f.ctx fx fy) false;
  let final () =
    match Fab.check t Th.Final with
    | Th.Conflict _ -> `Unsat
    | Th.Sat -> `Sat
    | Th.Propagations _ -> `Prop
    | Th.Split _ -> `Split
  in
  (* base: no edge is injectable (only x is fixed), so the frame is NOT unsat — the
     arrangement is simply undecided (a Split). The F3 property under test is
     edge-presence ⟺ UNSAT, not the base verdict. *)
  check "fabric pop: base (y free) ⇒ not UNSAT" (final () <> `Unsat);
  Fab.push t;
  assert_tm (Context.le f.ctx y five) true;
  assert_tm (Context.ge f.ctx y five) true;
  check "fabric pop: y fixed to 5 ⇒ UNSAT (edge injected)" (final () = `Unsat);
  Fab.pop t 1;
  check "fabric pop: after pop, edge dropped ⇒ NOT UNSAT again" (final () <> `Unsat);
  Fab.push t;
  assert_tm (Context.le f.ctx y five) true;
  assert_tm (Context.ge f.ctx y five) true;
  check "fabric pop: re-assert ⇒ UNSAT again (re-injection)" (final () = `Unsat)
;;

(* Codex-review #3 discriminator: a pre-fabric propagation owner must NOT survive a pop
   and misroute [explain] once an edge is live. Sequence: registry empty → push → LIA
   propagates the shared equality [p=q] (owner B) → pop → push → a fixed-value
   disagreement injects [p~q] (registry now live) → EUF re-propagates [p=q] (owner A). On
   the buggy code the phase-1 [p=q→B] entry is untrailed, survives the pop, and the
   first-wins guard refuses to overwrite it, so [explain (p=q)] routes to B — whose
   pop-scoped cache dropped the reason — and degrades to [Combination_unsound]/[Failure]
   (→ unknown), a regression vs trunk. The fix trails the phase-1 entry so the pop drops
   it and phase-2 records the fresh owner A. *)
let test_fabric_pop_owner_strand () =
  let f = fixture () in
  let p = const f "p"
  and q = const f "q" in
  let five = Context.int_const f.ctx 5 in
  let e_atom = Context.eq f.ctx p q in
  let t = Fab.create f.ctx f.env in
  let atoms : Atom.t Term.Table.t = Term.Table.create 32 in
  let atom_of term =
    match Term.Table.find_opt atoms term with
    | Some a -> a
    | None ->
      let a = fresh_atom f in
      Term.Table.replace atoms term a;
      Fab.register_atom t a term;
      a
  in
  let e_lit = Lit.make (atom_of e_atom) true in
  let assert_tm term sign = Fab.assert_lit t (Lit.make (atom_of term) sign) in
  let propagates_e r =
    match r with
    | Th.Propagations ls -> List.exists (Lit.equal e_lit) ls
    | _ -> false
  in
  (* base: p fixed to 5. *)
  assert_tm (Context.le f.ctx p five) true;
  assert_tm (Context.ge f.ctx p five) true;
  (* PHASE 1 (registry empty): push, fix q; LIA now entails p=q and propagates it (owner
     B). *)
  Fab.push t;
  assert_tm (Context.le f.ctx q five) true;
  assert_tm (Context.ge f.ctx q five) true;
  let phase1 = Fab.check t Th.Propagate in
  (* NOTE (reachability): in QF_UFLIA LIA propagates only [Le] atoms and EUF only
     equalities/predicates, so NO shared literal is propagated by BOTH children — LIA does
     not propagate the equality [p=q] here. Codex-review #3's dual-owner-across-a-pop
     sequence is therefore a LATENT code-path defect (the untrailed pre-fabric owner + the
     first-wins guard), not a naturally-reachable regression, and cannot be driven to a
     wrong verdict through the public seam in v1. The fix (trailing pre-fabric owners) is
     applied as defense-in-depth and to satisfy the Rev-6.1 trailing pin; this test drives
     the reachable pop→re-inject→explain path and asserts explain still routes correctly. *)
  ignore (propagates_e phase1);
  (* pop the frame. *)
  Fab.pop t 1;
  (* PHASE 2 (edge live): re-fix q; at Final the (p,q) disagreement injects p~q, and EUF
     re-propagates p=q (owner A). *)
  Fab.push t;
  assert_tm (Context.le f.ctx q five) true;
  assert_tm (Context.ge f.ctx q five) true;
  let phase2 = Fab.check t Th.Final in
  check
    "strand precond: edge injected + EUF re-propagates p=q (owner A)"
    (propagates_e phase2);
  (* the discriminator: [explain p=q] must route to the FRESH owner and not degrade. *)
  let explained =
    match Fab.explain t e_lit with
    | _ -> true
    | exception (Cmb.Combination_unsound _ | Failure _) -> false
  in
  check
    "strand: explain(p=q) routes to fresh owner after pop (no degrade to unknown)"
    explained
;;

(* REAL weak-Γ discriminator for the F1-SEM verifier (selection-review item a). Exercises
   the actual [Lia_adapter.fabric_verify] — the independent semantic re-derivation — and
   proves it does semantic work, not the old token-presence check. The load-bearing case
   is a SEMANTICALLY INSUFFICIENT Γ: a term [q] with only a lower bound asserted (Γ =
   [{q>=5}]) does NOT pin [q] to 5, and the verifier REJECTS a "fixed to 5" witness for
   it. The old tautological check (the 4 witness tokens ∈ Γ) would ACCEPT such a witness —
   so this assertion fails if the verifier is reverted to token-presence. Also:
   correct-witness accept, wrong-value reject, swapped-oriented-token reject. *)
let test_f1sem_verifier_discriminates () =
  let module La = Oxsmt_lia.Lia_adapter in
  let f = fixture () in
  let p = const f "p"
  and q = const f "q" in
  let five = Context.int_const f.ctx 5 in
  let t = La.create f.ctx f.env in
  let atoms : Atom.t Term.Table.t = Term.Table.create 16 in
  let atom_of term =
    match Term.Table.find_opt atoms term with
    | Some a -> a
    | None ->
      let a = fresh_atom f in
      Term.Table.replace atoms term a;
      La.register_atom t a term;
      a
  in
  let lit term sign = Lit.make (atom_of term) sign in
  (* p pinned to 5 by BOTH bounds (a genuine fix). *)
  La.assert_lit t (lit (Context.le f.ctx p five) true);
  La.assert_lit t (lit (Context.ge f.ctx p five) true);
  (* q lower-bounded ONLY (semantically insufficient: q ∈ [5, +∞), not pinned). *)
  let q_ge = Context.ge f.ctx q five in
  let q_ge_lit = Lit.make (atom_of q_ge) true in
  La.assert_lit t q_ge_lit;
  match La.fixed_bounds t p with
  | None -> check "f1sem: p is fixed (precondition)" false
  | Some fb ->
    let v = fb.Fabric.value in
    check
      "f1sem: verifier ACCEPTS a genuine fix with its real oriented tokens"
      (La.fabric_verify t p v fb.Fabric.lower fb.Fabric.upper);
    check
      "f1sem: verifier REJECTS a wrong claimed value"
      (not (La.fabric_verify t p "6" fb.Fabric.lower fb.Fabric.upper));
    check
      "f1sem: verifier REJECTS swapped oriented tokens"
      (not (La.fabric_verify t p v fb.Fabric.upper fb.Fabric.lower));
    check
      "f1sem: verifier REJECTS a semantically insufficient (one-sided) Γ [weak-Γ mutant]"
      (not (La.fabric_verify t q v (Fabric.Real q_ge_lit) (Fabric.Real q_ge_lit)))
;;

let () =
  Printf.printf "== combine mechanics ==\n";
  test_routing ();
  test_push_pop_lockstep ();
  test_propagate_merge ();
  test_propagate_conflict_shortcircuits_b ();
  test_explain_routing ();
  test_final_agree_sat ();
  test_final_disagree_split ();
  test_model_merge_sort_directed ();
  test_poison_on_pinned_disagreement ();
  test_pin_unwinds_on_pop ();
  test_provenance_signed_lit ();
  test_pin_satisfaction ();
  test_model_domain_and_sort ();
  test_final_requires_sat ();
  test_disagreement_domain_is_model_valued ();
  test_compound_disagreement_lookup ();
  test_overflow_guarded_fold ();
  test_unregistered_atom_poison ();
  test_explain_fallback ();
  test_compact_dag_registration ();
  test_router_polarity_contract ();
  Printf.printf "\n== combine integration (toy EUF + toy LIA + mini DPLL(T)) ==\n";
  test_integration_sat ();
  test_integration_unsat_direct ();
  test_integration_unsat_split ();
  test_integration_full_assignment ();
  test_integration_compound_pin ();
  test_integration_pure_euf_int_arg ();
  test_router_shared_sort_invariant ();
  Printf.printf
    "\n== combine W1 gate (REAL Euf_adapter + real Lia_adapter, no mocks) ==\n";
  test_w1_real_flat ();
  test_w1_real_tower ();
  test_w1_real_arity2 ();
  test_w1_real_usort_args ();
  test_bool_leaf_real ();
  test_bool_compound_real ();
  test_h1_distinct_bare_vars_real ();
  test_h2_buried_bool_leaf_real ();
  test_h2_buried_bool_uf_real ();
  test_pop_stale_bool_uf_arg_known_gap ();
  Printf.printf "\n== combine ADR §6 corpus (REAL Euf_adapter + real Lia_adapter) ==\n";
  test_r1_real ();
  test_fx1_fy1_real ();
  test_numeral_corner_real ();
  test_buried_lia_euf_owned_real ();
  test_predicate_variant_real ();
  test_mixed_equality_totality_real ();
  test_owned_eq_sides_guard_toy ();
  test_deep_tower_real ();
  test_pure_qf_lia_zero_splits ();
  test_push_pop_reassert_real ();
  test_use_history_transition_real ();
  Printf.printf "== ADR-0014 Stage 1b fabric ==\n";
  test_fabric_fixed_value_unsat ();
  test_fabric_distinct_values_sat ();
  test_fabric_const_offset_sat ();
  test_fabric_pop_reassert ();
  test_fabric_pop_owner_strand ();
  test_f1sem_verifier_discriminates ();
  Printf.printf "\n%d passed, %d failed\n" !passes !failures;
  if !failures > 0 then exit 1
;;

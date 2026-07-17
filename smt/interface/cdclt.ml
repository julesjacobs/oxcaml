(* CDCL(T) seam glue: the adapter that presents the internalization-based combined theory
   [Combine (Uflia_router) (Euf_adapter) (Lia_adapter)] (ADR-0010: model-based
   Nelson-Oppen where each boundary node is its own proxy, no purification pass) to the
   propositional SAT core through its {!Oxsmt_solver.Sat.theory} callback record (ADR-0005
   §3). This is the piece that makes {!Session} a real CDCL(T) solver rather than a
   propositional skeleton.

   {b What it maps.} The SAT core speaks [Sat.var]/[Sat.lit] over opaque boolean
   variables; the frozen THEORY speaks [Atom.t]/[Lit.t]. Each theory atom is minted 1:1
   with a SAT var (CONTRACT-ATOM). This module owns that bijection ([v2a]/[a2v]) plus the
   atom->term map the theory needs, and translates every seam event:
   - [on_assign lit ~level]: forward the (registered) atom's signed literal to the theory
     ([assert_lit]); ignore non-atom vars (Tseitin aux, frame selectors, boolean vars).
   - [check ~final]: run the combined theory at [Propagate]/[Final] effort and map its
     {!Oxsmt_core.Theory.check_result} onto {!Oxsmt_solver.Sat.theory_result}. A [Split]
     becomes a [T_lemma] whose one clause is the disjunction of the split disjuncts'
     signed literals (each disjunct is an atom or a negated atom; see {!split_lit},
     CONTRACT-SPLIT); it is counted against the split budget.
   - [explain lit]: the lazy, precedence-valid premise set for a theory-propagated
     literal.
   - [on_backtrack ~level]: pop theory frames down to [level] (one frame per SAT decision
     level, kept in lockstep by {!sync_level}).

   {b Frame discipline.} The theory is driven with one backtrack frame per SAT decision
   level. Registrations of the clausifier's atoms are done at the base frame ({!intern},
   called before [solve]) so they survive every backjump. Atoms minted from a [Split] are
   registered mid-solve, so the frame that introduced their e-nodes may be popped
   (euf.mli); they are flagged and {b re-registered} (idempotently, C7) before each
   assert, which rederives the truncated structure.

   {b Determinism (I6).} Every list crosses the seam in the theory's own deterministic
   order; atom/var ids are minted in a fixed sequence; no wall-clock, no randomness. *)

open Oxsmt_core
module Sat = Oxsmt_solver.Sat
module Egraph_view = Oxsmt_ematch.Egraph_view

module Combined =
  Oxsmt_combine.Combine.Combine (Oxsmt_combine.Uflia_router) (Oxsmt_euf.Euf_adapter)
    (Oxsmt_lia.Lia_adapter)

module Combined_real =
  Oxsmt_combine.Combine.Combine (Oxsmt_combine.Uflra_router) (Oxsmt_euf.Euf_adapter)
    (Oxsmt_lia.Lra_adapter)

module Dt = Oxsmt_dt.Dt
module Arr = Oxsmt_arr.Arr

(* The theory the seam drives. A problem that declares an algebraic datatype installs the
   standalone DT theory, one that uses arrays the standalone arrays theory (both e-graph
   clients — EUF congruence plus their own axioms); every other problem keeps the
   Nelson-Oppen EUF+LIA {!Combined} stack, byte-identical to before. The choice is made
   lazily at the first [intern] (after the session's declarations, so the datatype / array
   registries are populated by then) and is total, syntactic, assert-time — never a
   per-term relevance guess. *)
type theory_impl =
  | TCombined of Combined.t
  | TCombinedReal of Combined_real.t
  | TDt of Dt.t
  | TArr of Arr.t

type arithmetic_family =
  | None_seen
  | Integer
  | Real
  | Mixed

let th_register impl a term =
  match impl with
  | TCombined th -> Combined.register_atom th a term
  | TCombinedReal th -> Combined_real.register_atom th a term
  | TDt th -> Dt.register_atom th a term
  | TArr th -> Arr.register_atom th a term
;;

let th_assert impl lit =
  match impl with
  | TCombined th -> Combined.assert_lit th lit
  | TCombinedReal th -> Combined_real.assert_lit th lit
  | TDt th -> Dt.assert_lit th lit
  | TArr th -> Arr.assert_lit th lit
;;

let th_check impl effort =
  match impl with
  | TCombined th -> Combined.check th effort
  | TCombinedReal th -> Combined_real.check th effort
  | TDt th -> Dt.check th effort
  | TArr th -> Arr.check th effort
;;

let th_explain impl lit =
  match impl with
  | TCombined th -> Combined.explain th lit
  | TCombinedReal th -> Combined_real.explain th lit
  | TDt th -> Dt.explain th lit
  | TArr th -> Arr.explain th lit
;;

let th_push impl =
  match impl with
  | TCombined th -> Combined.push th
  | TCombinedReal th -> Combined_real.push th
  | TDt th -> Dt.push th
  | TArr th -> Arr.push th
;;

let th_pop impl n =
  match impl with
  | TCombined th -> Combined.pop th n
  | TCombinedReal th -> Combined_real.pop th n
  | TDt th -> Dt.pop th n
  | TArr th -> Arr.pop th n
;;

let th_model impl =
  match impl with
  | TCombined th -> Combined.model th
  | TCombinedReal th -> Combined_real.model th
  | TDt th -> Dt.model th
  | TArr th -> Arr.model th
;;

(* A model value for a symbol / table cell, in the eval-agnostic vocabulary the CLI
   renders to the §8 self-check sidecar grammar. [VUninterp i] is a 0-based ELEMENT INDEX
   into its uninterpreted sort's finite universe (NOT the raw e-graph class id —
   extraction remaps class ids to dense per-sort indices, ADR-UF-models §1/R10). *)
type value =
  | VBool of bool
  | VInt of
      Bigint.t (* arbitrary precision (core-bignum W2): a term value can exceed int63 *)
  | VReal of Oxsmt_lia.Rational.t
  | VUninterp of int

(* A total interpretation of one uninterpreted function/predicate symbol (ADR-UF-models
   §0/§1): [cases] maps argument-index tuples to the result value, [default] covers every
   unlisted tuple. First-match / structural-equality lookup, as both N-version readers
   expect (tests/eval eval.ml, tests/gate encoder.ml). *)
type fun_table =
  { default : value
  ; cases : (value list * value) list
  }

(* A model binding: a nullary symbol's value, or a function/predicate's table. *)
type binding =
  | Const of string * value
  | Fun of string * fun_table

(* The finite universe cardinality of one uninterpreted sort (SMT-LIB sorts are inhabited
   ⇒ [card >= 1], R2). *)
type sort_card =
  { sort_name : string
  ; card : int
  }

(* Raised when the per-check-sat split budget is exhausted: the [T_lemma] loop has no
   intrinsic bound (LIA B&B / N-O splitting can diverge — CONTRACT-SPLIT-TERM), so the
   driver caps it deterministically and routes exhaustion to [unknown] (never a verdict
   from an unfinished search). Caught at the {!Session} boundary. *)
exception Split_budget_exceeded

(* Monomorphic [Sat.var]-keyed table ([Sat.var = int]): avoids the polymorphic
   [caml_hash]/[compare_val] the default [Hashtbl] runs on every intern-path lookup. None
   of the tables below ([v2a]/[v2term]/[is_split]) is ever iterated, so bucket layout is
   not observable. *)
module Vartbl = Hashtbl.Make (struct
    type t = int

    let equal = Int.equal
    let hash (x : int) = x
  end)

(* ADR-0014 Stage 4.2 DARK flag: earliest-removed incremental theory undo under
   chronological backtracking. OFF (default, unset) => the driver installs
   [on_chrono_rewind = None] and the SAT core takes its byte-identical full-rebuild chrono
   arm; the [ckpt_log] machinery below is never touched (and [on_assign] keeps its exact
   pre-S4.2 [sync_level] behaviour). Requires BOTH this flag AND [OXSMT_CHRONO]:
   incremental undo is meaningless without chrono, and only the chrono scattered-removal
   arm ever invokes the hook (the monotone arm always uses [on_backtrack ~level]). Same
   on-value vocabulary as the SAT core's [OXSMT_CHRONO]. *)
let env_on name =
  match Sys.getenv_opt name with
  | Some ("1" | "true" | "yes" | "on") -> true
  | Some _ | None -> false
;;

let incr_undo = lazy (env_on "OXSMT_CHRONO_INCR_UNDO" && env_on "OXSMT_CHRONO")

type leaf_certificate_trace =
  { on_theory_atom : var:Sat.var -> atom:Term.t -> unit
  ; on_euf_leaf : clause:Sat.lit list -> unit
  ; on_dt_distinctness :
      registry:Datatype_defs.t
      -> clause:Sat.lit list
      -> left:Term.t
      -> right:Term.t
      -> unit
  ; on_lia_conflict :
      premise_lits:Sat.lit list -> multipliers:Oxsmt_lia.Rational.t list -> unit
  }

type lia_certificate_trace =
  { on_theory_atom : var:Sat.var -> atom:Term.t -> unit
  ; on_lia_conflict :
      premise_lits:Sat.lit list -> multipliers:Oxsmt_lia.Rational.t list -> unit
  }

type combined_checkpoint =
  | CInt of Combined.checkpoint
  | CReal of Combined_real.checkpoint

type t =
  { mutable theory : theory_impl option
      (* chosen lazily at the first [intern] from [registry] (empty => Combined). [None]
         until then, and forever for a pure-propositional problem with no theory atom. *)
  ; ctx : Context.t
  ; env : Env.t
  ; cap : Env.reserved_cap
      (* ADR-0012 R1 reserved-minting capability for [env] (threaded from Session, the
         sole holder). Handed to the standalone arrays theory, which mints unforgeable
         reserved extensionality witnesses; unused by the other theories. *)
  ; registry : Oxsmt_core.Datatype_defs.t ref
      (* datatype declarations (shared ref with Session); empty for a non-DT problem *)
  ; array_registry : Oxsmt_core.Array_defs.t ref
      (* array select/store symbols (shared ref with Session); empty for a non-array
         problem. Checked before [registry] in [ensure_theory]. *)
  ; arithmetic_family : arithmetic_family ref
  ; sat : Sat.t
  ; alloc : Atom.allocator
  ; v2a : Atom.t Vartbl.t (* SAT var -> theory atom (theory atoms only) *)
  ; v2term : Term.t Vartbl.t (* SAT var -> its atom term *)
  ; a2v : Sat.var Atom.Table.t (* theory atom -> SAT var *)
  ; t2v : Sat.var Term.Table.t (* atom term -> SAT var (hash-cons sharing) *)
  ; is_split : unit Vartbl.t (* atoms minted from a Split (need re-register) *)
  ; subterms : unit Term.Table.t (* every subterm of a registered atom, for the model *)
  ; mutable level : int (* theory frames pushed above the base (= SAT decision level) *)
  ; split_budget : int
  ; mutable splits : int (* splits emitted in the current check-sat *)
  ; budget : Budget.t (* shared effort budget (board #60): SAT ticks it, we tick Final *)
  ; mutable last_model : Model.t option (* snapshot taken at the accepting Final->Sat *)
  ; mutable capture_egraph : bool
      (* Set per check when a live universal lemma can consume a Final snapshot. Keeping
         it false on QF checks avoids an otherwise pointless full e-graph copy. *)
  ; mutable last_egraph_view : Egraph_view.t option
      (* Immutable congruence classes and ground-term universe from the accepting Final.
         SAT backtracks its trail before Session runs E-matching, so the live engine no
         longer contains the equalities that made that candidate model consistent. *)
  ; mutable last_dt_model : (Term.t * Dt.ctor_tree) list option
      (* DT constructor-tree checker model, snapshotted at the accepting Final->Sat when
         the installed theory is the standalone DT theory (else [None]); read by
         {!Session}'s DT-branch commit through {!dt_model} and checked by
         [Dt_model_check]. *)
  ; mutable last_array_model : (Term.t * Arr.value) list option
      (* arrays checker model, snapshotted at the accepting Final->Sat when the installed
         theory is the standalone arrays theory (else [None]); read by {!Session}'s arrays
         commit through {!array_model} and checked by [Array_model_check]. *)
  ; mutable relevancy : Relevancy.t option
      (* dynamic relevancy driver (task #24), [None] unless {!Session} installed one from
         the [OXSMT_RELEVANCY] gate. When [Some], the two trail seam events below stream
         to it so it can maintain relevancy marks in lockstep with the SAT trail; the
         branch filter itself is installed directly on the SAT core by {!Session}. A
         [None] arm is behaviourally inert — the theory glue is byte-identical with
         relevancy off. *)
  ; mutable leaf_certificate_trace : leaf_certificate_trace option
      (* Off-seam theory-leaf evidence for the certificate recorder. [None] in every
         normal solve; the observational callbacks cannot affect search. *)
  ; ckpt_log : combined_checkpoint option Dynarray.t
  (* ADR-0014 S4.2 (dark, [incr_undo] only): one Combined sub-frame checkpoint per
     [on_assign], keyed by ABSOLUTE SAT-trail index — [ckpt_log.(i)] is the theory
     watermark just BEFORE trail literal [i] was asserted, so [rewind i] restores it and
     undoes literals [i..]. The seam fires exactly one [on_assign] per trail placement, so
     [Dynarray.length ckpt_log] tracks the SAT trail length exactly and the core's
     absolute rewind index [w] indexes this vector directly.

     INVARIANT (index alignment): the log is kept the SAME length as the SAT trail at
     every quiescent point — grown one entry per [on_assign], truncated to [w] on a chrono
     rewind (the survivors [w..] re-appended by the core's replay), and (crucially)
     NEITHER cleared NOR shrunk across a query boundary, because the SAT core RETAINS its
     level-0 trail prefix across [check_sat]s. {!reset_for_new_query} therefore
     invalidates each retained entry to [None] (a fail-closed spacer — level-0 literals
     are never removed, so it is never a legitimate rewind target) but PRESERVES the entry
     count, so a query-2 [on_assign] lands at its true absolute trail index (H2). A [None]
     entry is any [on_assign] made with no live [Combined] theory (a pure-Boolean prefix
     or a retained cross-query spacer); {!on_chrono_rewind} fails closed if the core ever
     names one as a target. Empty and unused when the flag is off. *)
  }

let sign_lit = Sat.sign_of_lit

(* Signed core [Lit.t] -> SAT literal. The atom is always registered (every atom the
   theory can name came through [intern]), so the lookup cannot miss for a correct theory. *)
let satlit_of_lit t (lit : Lit.t) =
  let v = Atom.Table.find t.a2v (Lit.atom lit) in
  if Lit.sign lit then Sat.pos v else Sat.neg v
;;

let is_euf_atom_term term =
  Theory_view.is_atom term
  &&
  match Theory_view.atom term with
  | Theory_view.Equality _ | Theory_view.Predicate _ | Theory_view.Bool_lit _ -> true
  | Theory_view.Le_zero _ -> false
;;

(* The exact structural subterm closure of a set of certificate statement atoms. This is
   intentionally local rather than [t.subterms]: the latter contains every registered atom
   in the query and would let an unrelated constructor term authorize a witness for this
   leaf. *)
let statement_subterms roots =
  let seen = Term.Table.create 32 in
  let rec add (term : Term.t) =
    if not (Term.Table.mem seen term)
    then (
      Term.Table.replace seen term ();
      match term.Term.node with
      | Term.Bool_const _ | Term.Int_const _ | Term.Real_const _ -> ()
      | Term.App (_, args) -> Iarr.iter add args
      | Term.Arith { coeffs; _ } -> Iarr.iter (fun (child, _) -> add child) coeffs
      | Term.Real_arith { coeffs; _ } -> Iarr.iter (fun (child, _) -> add child) coeffs
      | Term.Le child | Term.Not child -> add child
      | Term.Eq (a, b) ->
        add a;
        add b
      | Term.And children | Term.Or children -> Iarr.iter add children
      | Term.Ite (c, a, b) ->
        add c;
        add a;
        add b)
  in
  List.iter add roots;
  seen
;;

(* Preserve an [Euf_congruence] leaf only when it is genuinely a pure congruence proof.
   The rule tag is shared by the standalone datatype/array engines, so require the real
   combined EUF+LIA stack. The theory fabric also retains the tag after expanding an EUF
   edge to arithmetic premises, so a live edge makes the leaf conditional. Finally every
   literal must resolve through the authoritative SAT-var map to an EUF proposition. *)
let record_euf_leaf t ~rule ~clause =
  match t.leaf_certificate_trace, rule, t.theory with
  | Some trace, Explanation.Rule_tag.Euf_congruence, Some (TCombined combined)
    when (not (Combined.has_live_fabric_edges combined))
         && List.for_all
              (fun lit ->
                match Vartbl.find_opt t.v2term (Sat.var_of_lit lit) with
                | Some term -> is_euf_atom_term term
                | None -> false)
              clause -> trace.on_euf_leaf ~clause
  | Some _, _, _ | None, _, _ -> ()
;;

(* Preserve a datatype constructor-distinctness claim only on the standalone DT stack. The
   generic [Euf_congruence] rule tag is also used by array and combined-theory conflicts,
   so it is not a discriminator. Instead the DT engine must recover the exact constructor
   pair whose congruence explanation equals this emitted premise list.

   The remaining gates bind the claim to the certificate statement: every premise must be
   a positive equality SAT literal, and both claimed constructor terms must occur in the
   structural closure of those exact equality atoms. A negative premise, foreign atom,
   missing SAT-var binding, or unrelated term leaves the leaf conditional. *)
let record_dt_distinctness t ~premises ~premise_lits ~clause =
  match t.leaf_certificate_trace, t.theory with
  | Some trace, Some (TDt dt) ->
    let rec equality_atoms acc = function
      | [] -> Some (List.rev acc)
      | lit :: rest ->
        if not (Sat.sign_of_lit lit)
        then None
        else (
          match Vartbl.find_opt t.v2term (Sat.var_of_lit lit) with
          | Some atom when Theory_view.is_atom atom ->
            (match Theory_view.atom atom with
             | Theory_view.Equality _ -> equality_atoms (atom :: acc) rest
             | Theory_view.Predicate _ | Theory_view.Bool_lit _ | Theory_view.Le_zero _ ->
               None)
          | Some _ | None -> None)
    in
    (match equality_atoms [] premise_lits with
     | None -> ()
     | Some atoms ->
       (match Dt.constructor_clash_for_premises dt premises with
        | None -> ()
        | Some (left, right) ->
          let closure = statement_subterms atoms in
          if Term.Table.mem closure left && Term.Table.mem closure right
          then trace.on_dt_distinctness ~registry:!(t.registry) ~clause ~left ~right))
  | Some _, Some (TCombined _ | TCombinedReal _ | TArr _) | Some _, None | None, _ -> ()
;;

let set_leaf_certificate_trace t tr =
  (match tr with
   | Some _ ->
     if Option.is_some t.leaf_certificate_trace
     then invalid_arg "Cdclt.set_leaf_certificate_trace: already installed";
     if Term.Table.length t.t2v <> 0
     then
       invalid_arg
         "Cdclt.set_leaf_certificate_trace: must be installed before theory atoms are \
          internalized"
   | None -> ());
  t.leaf_certificate_trace <- tr
;;

let set_lia_certificate_trace t tr =
  set_leaf_certificate_trace
    t
    (Option.map
       (fun (trace : lia_certificate_trace) ->
         { on_theory_atom = trace.on_theory_atom
         ; on_euf_leaf = (fun ~clause:_ -> ())
         ; on_dt_distinctness = (fun ~registry:_ ~clause:_ ~left:_ ~right:_ -> ())
         ; on_lia_conflict = trace.on_lia_conflict
         })
       tr)
;;

(* Collect [term] and every subterm (all sorts), for reconstructing the model. Membership
   is a monotonic imperative table keyed on [Term] tag (O(1), monomorphic) rather than a
   balanced [Term.Set] whose per-op closure-dispatched compare dominated the intern path;
   [subterms_sorted] recovers the old tag-ascending [Set.elements] order at model time. *)
let rec collect t (term : Term.t) =
  if not (Term.Table.mem t.subterms term)
  then (
    Term.Table.replace t.subterms term ();
    match term.Term.node with
    | Term.Bool_const _ | Term.Int_const _ | Term.Real_const _ -> ()
    | Term.App (_, args) -> Iarr.iter (collect t) args
    | Term.Arith lin -> Iarr.iter (fun (c, _) -> collect t c) lin.Term.coeffs
    | Term.Real_arith lin -> Iarr.iter (fun (c, _) -> collect t c) lin.Term.coeffs
    | Term.Le a | Term.Not a -> collect t a
    | Term.Eq (a, b) ->
      collect t a;
      collect t b
    | Term.And xs | Term.Or xs -> Iarr.iter (collect t) xs
    | Term.Ite (a, b, c) ->
      collect t a;
      collect t b;
      collect t c)
;;

(* The collected subterms in tag-ascending order — identical to the old
   [Term.Set.elements t.subterms], so every downstream model-extraction order is preserved
   (I6). [Term.compare] is [Int.compare] on the tag. *)
let subterms_sorted t =
  List.sort Term.compare (Term.Table.fold (fun k () acc -> k :: acc) t.subterms [])
;;

(* Get-or-create the SAT var for a theory-atom [term], registering it with the combined
   theory on first sight (CONTRACT-REG). [split] flags an atom minted mid-solve from a
   [Split], whose e-node registration may later be truncated by a backjump. *)
let theory_instantiated t = t.theory <> None

(* Reset-per-query theory invalidation (task #54). When {!Session} REPLACES the datatype /
   array registry after a prior query has already instantiated + cached a theory, that
   cached theory (and the whole SAT-var<->theory-atom bijection it registered) is stale
   against the new registry — the #51 landmine: the session-lifetime
   [ctor_terms]/[seen_cat] the old [Dt.t] accumulated would meet a differently-populated
   registry and drive a false constructor-clash (the codex wrong-[unsat]). Rather than
   fail-close to [unknown] (the #51 interim guard), DROP the entire theory instance and
   the bijection, so the NEXT [intern] rebuilds the theory fresh from the new registry
   ([ensure_theory]) and re-interns every term against it — no stale classification can
   survive, because the old [Dt.t] (with its [ctor_terms]) is gone and every re-used
   [Term.t] mints a brand-new atom.

   PRECONDITION (enforced by {!Session}): called only between queries, with the SAT core
   at decision level 0 and NO live assertions bound to the dropped bijection. {!Session}
   raises fail-LOUD when a registry replacement is attempted with live assertions above
   base (the contract-A ruling), so this never strands an in-flight atom. The SAT core's
   vars/clauses from the prior (already-popped) query stay allocated but inert — their
   frame selector is free to be false, so they are trivially satisfiable and, being absent
   from the cleared [v2a], are ignored by [on_assign]; re-interned terms get fresh vars
   that never collide. Keeps [alloc] / [sat] / [budget] / the shared registry refs; only
   the per-session theory choice and the interning tables are reset. *)
let reset_for_new_query t =
  Vartbl.clear t.v2a;
  Vartbl.clear t.v2term;
  Atom.Table.clear t.a2v;
  Term.Table.clear t.t2v;
  Vartbl.clear t.is_split;
  Term.Table.clear t.subterms;
  t.theory <- None;
  t.level <- 0;
  t.splits <- 0;
  t.capture_egraph <- false;
  t.last_model <- None;
  t.last_egraph_view <- None;
  t.last_dt_model <- None;
  t.last_array_model <- None;
  (* S4.2 (dark, H2): the logged checkpoints belong to the dropped theory instance, but
     the SAT core RETAINS its level-0 trail prefix across the query boundary, so the log
     must stay index-aligned with that retained prefix. Invalidate every retained entry to
     a fail-closed [None] spacer (never a legitimate rewind target — level-0 literals are
     never removed by [cancel_until]) while PRESERVING the entry count, so the next
     query's [on_assign] appends at its true absolute trail index. Clearing the log here
     would shift every subsequent absolute index by the retained-prefix length and
     mis-target rewinds. *)
  for i = 0 to Dynarray.length t.ckpt_log - 1 do
    Dynarray.set t.ckpt_log i None
  done
;;

let ensure_theory t =
  match t.theory with
  | Some impl -> impl
  | None ->
    let impl =
      match !(t.arithmetic_family) with
      | Mixed -> failwith "cdclt.ensure_theory: mixed Int/Real arithmetic is unsupported"
      | Real when not (Oxsmt_core.Lra_config.enabled ()) ->
        failwith "cdclt.ensure_theory: Real arithmetic is disabled"
      | Real
        when (not (Oxsmt_core.Array_defs.is_empty !(t.array_registry)))
             || not (Oxsmt_core.Datatype_defs.is_empty !(t.registry)) ->
        failwith
          "cdclt.ensure_theory: arrays/datatypes combined with Real are unsupported"
      | Real -> TCombinedReal (Combined_real.create t.ctx t.env)
      | Integer | None_seen ->
        if not (Oxsmt_core.Array_defs.is_empty !(t.array_registry))
        then TArr (Arr.create t.ctx t.env t.cap !(t.array_registry))
        else if not (Oxsmt_core.Datatype_defs.is_empty !(t.registry))
        then TDt (Dt.create t.ctx t.env t.registry)
        else TCombined (Combined.create t.ctx t.env)
    in
    t.theory <- Some impl;
    impl
;;

let intern t ~split term =
  match Term.Table.find_opt t.t2v term with
  | Some v -> v
  | None ->
    let impl = ensure_theory t in
    let v = Sat.new_var t.sat in
    let a = Atom.fresh t.alloc in
    Term.Table.replace t.t2v term v;
    Vartbl.replace t.v2a v a;
    Vartbl.replace t.v2term v term;
    Atom.Table.replace t.a2v a v;
    if split then Vartbl.replace t.is_split v ();
    collect t term;
    th_register impl a term;
    (match t.leaf_certificate_trace with
     | Some tr -> tr.on_theory_atom ~var:v ~atom:term
     | None -> ());
    v
;;

(* Public wrapper used by {!Session} during clausification (base-frame registration). *)
let intern_atom t term = intern t ~split:false term

(* Bind an ALREADY-ALLOCATED SAT var [v] (a nullary Bool variable's PROPOSITIONAL var,
   minted by {!Session} in [prop_to_var]/[bool_consts]) as an EUF [K_bool] theory atom for
   [term], so the congruence engine merges [term] with [true_const]/[false_const] when the
   SAT core assigns [v]. This is the completeness half of the Bool-cardinality rule for a
   BARE Bool variable used as an uninterpreted-function argument (combine.ml's H2 guard):
   unlike an applied predicate [p(x…)] — which {!Session.register_bool_terms} routes
   through {!intern_atom} and whose truth EUF can also propagate by congruence — a bare
   buried Bool variable surfaces in NO clause, so without an atom binding EUF never learns
   its truth and leaves it a third opaque Boolean class
   ([h(b) ≠ h(true) ∧ h(b) ≠ h(false)] then wrong-degrades to [unknown]). Reusing the SAME
   [v] as the propositional variable (rather than minting a fresh one via {!intern}) keeps
   a single SAT variable per term: the model still reads its value from [bool_consts], and
   EUF and the propositional skeleton can never disagree on [term]. The var is on the SAT
   decision heap ([Sat.new_var] inserts it), so it is decided even when it occurs in no
   clause, and [on_assign] then asserts it to EUF. Idempotent: a no-op if [term] is
   already a theory atom or [v] already owns one. *)
let bind_bool_var_atom t term v =
  if (not (Term.Table.mem t.t2v term)) && not (Vartbl.mem t.v2a v)
  then (
    let impl = ensure_theory t in
    let a = Atom.fresh t.alloc in
    Term.Table.replace t.t2v term v;
    Vartbl.replace t.v2a v a;
    Vartbl.replace t.v2term v term;
    Atom.Table.replace t.a2v a v;
    collect t term;
    th_register impl a term;
    match t.leaf_certificate_trace with
    | Some tr -> tr.on_theory_atom ~var:v ~atom:term
    | None -> ())
;;

(* Keep one theory frame per SAT decision level (I push lazily as levels open; a dummy
   assumption level can jump the level by more than one, hence the loop). *)
let sync_level t =
  match t.theory with
  | None -> () (* no theory installed (pure-propositional): no frames to push *)
  | Some impl ->
    let d = Sat.decision_level t.sat in
    while t.level < d do
      t.level <- t.level + 1;
      th_push impl
    done
;;

(* Install the dynamic relevancy driver (task #24). [None] restores the byte-identical
   default. The branch filter itself is installed on the SAT core by {!Session}; this only
   routes the trail seam events below to the driver. *)
let set_relevancy t r = t.relevancy <- r

(* [~level] is [l]'s TRUE decision level, pushed by the SAT core (fabric S4 seam). On a
   monotone trail it equals [Sat.decision_level t.sat] (so this is byte-identical to the
   pre-S4 glue, which read that); under chronological backtracking it is [l]'s own
   backjump level, which can be BELOW the current decision level. We forward it to
   relevancy verbatim (a latent-correctness fix: the old code passed the CURRENT level,
   wrong for a below-current CB assignment). The theory-atom assertion below still files
   into the TOP frame: the child theories (Combined/Dt/Arr) are strictly LIFO scope
   stacks, so an assertion cannot be filed into a lower (non-top) true-level frame without
   breaking LIFO order — true-level filing / scope-aware undo needs the S4.0 sub-frame
   watermark primitive wired into the child scope, the fabric S4 follow-up. On the
   monotone (default, non-chrono) path [~level] equals the top frame, so top-frame filing
   IS true-level filing and behavior is unchanged. *)
let on_assign t l ~level =
  (match t.relevancy with
   | None -> ()
   | Some rel ->
     Relevancy.on_assign rel ~var:(Sat.var_of_lit l) ~value:(sign_lit l) ~level);
  if Lazy.force incr_undo
  then
    (* S4.2 single-base-frame discipline (dark): never push a per-level frame; log the
       Combined sub-frame watermark BEFORE this literal is asserted, so a chrono
       earliest-removed rewind can restore any trail prefix. [ckpt_log] grows one entry
       per [on_assign], in lockstep with the trail (length == trail length). Under chrono
       every backtrack routes through the SAT core's scattered-removal arm, which calls
       [on_chrono_rewind] (not [on_backtrack] with a real pop), so the theory only ever
       operates at its base frame — no frame push is needed. *)
    Dynarray.add_last
      t.ckpt_log
      (match t.theory with
       | Some (TCombined th) -> Some (CInt (Combined.checkpoint th))
       | Some (TCombinedReal th) -> Some (CReal (Combined_real.checkpoint th))
       | Some (TDt _ | TArr _) | None -> None)
  else sync_level t;
  let v = Sat.var_of_lit l in
  match Vartbl.find_opt t.v2a v with
  | None -> () (* an aux / selector / boolean-variable literal: not a theory atom *)
  | Some a ->
    let impl = ensure_theory t in
    (* a Split-minted atom's e-nodes may have been truncated by a pop; re-register
       (idempotent) so the child engines hold it before we assert. *)
    if Vartbl.mem t.is_split v then th_register impl a (Vartbl.find t.v2term v);
    th_assert impl (Lit.make a (sign_lit l))
;;

let on_backtrack t ~level =
  (match t.relevancy with
   | None -> ()
   | Some rel -> Relevancy.on_backtrack rel ~level);
  let n = t.level - level in
  if n > 0
  then (
    (match t.theory with
     | Some impl -> th_pop impl n
     | None -> ());
    t.level <- level)
;;

(* ADR-0014 S4.2 chrono earliest-removed incremental undo (dark, [incr_undo] only). The
   SAT core calls [rewind w] where [w] is the earliest-removed pre-compaction trail index
   (= the number of trail-prefix literals retained): rewind the Combined theory to the
   sub-frame watermark logged just before literal [w] was asserted, dropping exactly the
   theory state for stream positions [w..]. The core then replays those survivors via
   [on_assign] (each re-logs its checkpoint into the truncated [ckpt_log]). OBS-EQ to the
   pop-to-base + replay-all rebuild arm.

   [w] indexes [ckpt_log] ABSOLUTELY ([ckpt_log] length tracks the trail length; see the
   field's index-alignment invariant). Three cases, by design (H1):
   - [w >= length]: nothing at trail index [>= w] was removed — the log points at or past
     its end, so the current theory state ALREADY reflects exactly the retained prefix.
     NO-OP. This is the zero-removal chrono [cancel_until] ([w = trail_n], reachable when
     an already-true assumption opens a dummy decision level with no trail literal): keep
     everything, replay nothing. It must NEVER wipe live theory state.
   - [w < length] with [ckpt_log.(w) = Some c]: the normal case — rewind to [c], then drop
     the log suffix (the core replays [w..]).
   - [w < length] with [ckpt_log.(w) = None]: the stream==trail invariant is broken (in
     the Combined arm every [on_assign] logs a [Some]; a [None] here would be a retained
     cross-query spacer, never a legitimate rewind target). Fail CLOSED (raise -> the I8
     firewall -> unknown) rather than fall back to a state-wiping default.

   Fails loud on a standalone DT/array theory: incremental undo is only sound over the
   Combined stack's sub-frame trail (a silent no-undo would be a soundness break, so we
   raise rather than degrade). *)
let on_chrono_rewind t w =
  (match t.theory with
   | None -> () (* pure-Boolean: no theory to rewind (the log is still truncated below) *)
   | Some (TDt _ | TArr _) ->
     failwith
       "cdclt.on_chrono_rewind: OXSMT_CHRONO_INCR_UNDO requires the Combined theory"
   | Some (TCombined th) ->
     if w < Dynarray.length t.ckpt_log
     then (
       match Dynarray.get t.ckpt_log w with
       | Some (CInt target) -> Combined.rewind_to_checkpoint th target
       | Some (CReal _) ->
         failwith "cdclt.on_chrono_rewind: Real checkpoint with Integer theory"
       | None ->
         failwith
           "cdclt.on_chrono_rewind: unlogged prefix at retained trail index \
            (stream/trail desync)")
   | Some (TCombinedReal th) ->
     if w < Dynarray.length t.ckpt_log
     then (
       match Dynarray.get t.ckpt_log w with
       | Some (CReal target) -> Combined_real.rewind_to_checkpoint th target
       | Some (CInt _) ->
         failwith "cdclt.on_chrono_rewind: Integer checkpoint with Real theory"
       | None ->
         failwith
           "cdclt.on_chrono_rewind: unlogged prefix at retained trail index \
            (stream/trail desync)"));
  (* Keep [ckpt_log] trail-aligned: drop the suffix the core's replay re-appends. A no-op
     when [w >= length] (the zero-removal NO-OP case above — nothing to drop). *)
  if w < Dynarray.length t.ckpt_log then Dynarray.truncate t.ckpt_log w
;;

(* Clausify one disjunct of a theory [Split] into a signed SAT literal (CONTRACT-SPLIT). A
   disjunct is a theory atom or the negation of one; peel leading [Not]s tracking parity
   so [Not B] becomes [¬(intern B)]. Interning the [Not] node itself would mint a fresh
   POSITIVE atom [N] and emit [A ∨ N] instead of the required [A ∨ ¬B] — a wrong clause
   (latent today: current UFLIA split producers emit only positive atoms). *)
let rec split_lit t ~sign (tm : Term.t) =
  match tm.Term.node with
  | Term.Not a -> split_lit t ~sign:(not sign) a
  | _ ->
    let v = intern t ~split:true tm in
    if sign then Sat.pos v else Sat.neg v
;;

(* Desugar a THEORY [check_result] (other than the Final [Sat] arm, whose model snapshot
   is impl-specific and stays in {!check}) into the SAT-core [theory_result], sharing the
   signed-literal clausifier {!split_lit} for both [Split] and [Lemma]. Factored out of
   {!check} so the CONTRACT-LEMMA/CONTRACT-SPLIT desugar is exercised directly by the seam
   tests (see {!desugar_result_for_test}) rather than a re-implemented copy. Behavior is
   byte-identical to the inlined arms it replaced:
   - [Split] at [Final] clausifies (≥2-atom disjunction) and is DROPPED at [Propagate]
     (illegal there — CONTRACT-SPLIT), matching the old
     [Theory.Sat | Theory.Split _ -> T_consistent []] Propagate arm;
   - [Lemma] (CONTRACT-LEMMA) clausifies IDENTICALLY at BOTH efforts — never dropped at
     [Propagate] (the LCG-serving arm) — each [(tm, sign)] going through
     [split_lit t ~sign tm] (Not-peeling tracks parity), capped by the same per-check-sat
     split budget. *)
let desugar_result t ~final (r : Theory.check_result) : Sat.theory_result =
  match r with
  | Theory.Sat -> Sat.T_consistent [] (* Final: caller snapshots the model separately *)
  | Theory.Propagations lits -> Sat.T_consistent (List.map (satlit_of_lit t) lits)
  | Theory.Conflict e ->
    let premise_lits = List.map (satlit_of_lit t) e.Explanation.premises in
    (match t.leaf_certificate_trace with
     | None -> ()
     | Some _ ->
       let clause = List.map Sat.neg_lit premise_lits in
       record_euf_leaf t ~rule:e.Explanation.rule ~clause;
       record_dt_distinctness t ~premises:e.Explanation.premises ~premise_lits ~clause);
    (* The frozen SAT trace deliberately carries only the materialized clause. When a
       certificate recorder explicitly installed the off-seam channel, preserve the Farkas
       evidence HERE, before [T_conflict] drops the rule tag and term meanings.

       Binding check: the adapter's observational core is accepted only when mapping its
       [(atom, polarity)] list through this driver's authoritative term->SAT table gives
       exactly the Explanation premise list. A stale/misaligned core therefore emits no
       witness (the leaf remains conditional), never a witness for the wrong clause. *)
    (match t.leaf_certificate_trace, e.Explanation.rule, t.theory with
     | Some tr, Explanation.Rule_tag.Lia_farkas, Some (TCombined combined) ->
       (match
          Oxsmt_lia.Lia_adapter.last_conflict_core (Combined.arith_state combined)
        with
        | Some { farkas = Some multipliers; atoms } ->
          let rec map_atoms acc = function
            | [] -> Some (List.rev acc)
            | (atom, polarity) :: rest ->
              (match Term.Table.find_opt t.t2v atom with
               | None -> None
               | Some var ->
                 let lit = if polarity then Sat.pos var else Sat.neg var in
                 map_atoms (lit :: acc) rest)
          in
          (match map_atoms [] atoms with
           | Some mapped when mapped = premise_lits ->
             tr.on_lia_conflict ~premise_lits ~multipliers
           | Some _ | None -> ())
        | Some { farkas = None; _ } | None -> ())
     | Some _, _, _ | None, _, _ -> ());
    Sat.T_conflict premise_lits
  | Theory.Split terms ->
    if not final
    then Sat.T_consistent [] (* CONTRACT-SPLIT: a Split is illegal/dropped at Propagate *)
    else (
      t.splits <- t.splits + 1;
      if t.splits > t.split_budget then raise Split_budget_exceeded;
      Sat.T_lemma [ List.map (split_lit t ~sign:true) terms ])
  | Theory.Lemma signed ->
    t.splits <- t.splits + 1;
    if t.splits > t.split_budget then raise Split_budget_exceeded;
    Sat.T_lemma [ List.map (fun (tm, sign) -> split_lit t ~sign tm) signed ]
;;

(* Test-only re-export of {!desugar_result} so the CONTRACT-LEMMA seam tests (H1) can feed
   a crafted [Theory.check_result] at either effort through the REAL clausifier and
   inspect the emitted clause (multi-antecedent, per-disjunct sign, Not-peeling) and the
   both-efforts delivery (a [Lemma] is not dropped at Propagate, a [Split] is). *)
let desugar_result_for_test = desugar_result

let live_egraph_view t : Egraph_view.t =
  match t.theory with
  | Some (TCombined th) ->
    let cs = Combined.congruence_state th in
    { app_terms_by_symbol = (fun sym -> Oxsmt_euf.Euf_adapter.app_terms_by_symbol cs sym)
    ; find_class_opt = (fun term -> Oxsmt_euf.Euf_adapter.find_class_opt cs term)
    ; equal_if_registered = (fun a b -> Oxsmt_euf.Euf_adapter.equal_if_registered cs a b)
    ; class_members = (fun term -> Oxsmt_euf.Euf_adapter.class_members cs term)
    ; ground_terms_by_sort =
        (fun sort -> Oxsmt_euf.Euf_adapter.registered_terms_by_sort cs sort)
    }
  | Some (TCombinedReal th) ->
    let cs = Combined_real.congruence_state th in
    { app_terms_by_symbol = (fun sym -> Oxsmt_euf.Euf_adapter.app_terms_by_symbol cs sym)
    ; find_class_opt = (fun term -> Oxsmt_euf.Euf_adapter.find_class_opt cs term)
    ; equal_if_registered = (fun a b -> Oxsmt_euf.Euf_adapter.equal_if_registered cs a b)
    ; class_members = (fun term -> Oxsmt_euf.Euf_adapter.class_members cs term)
    ; ground_terms_by_sort =
        (fun sort -> Oxsmt_euf.Euf_adapter.registered_terms_by_sort cs sort)
    }
  | Some (TDt th) ->
    { app_terms_by_symbol = Dt.app_terms_by_symbol th
    ; find_class_opt = Dt.find_class_opt th
    ; equal_if_registered = Dt.equal_if_registered th
    ; class_members = Dt.class_members th
    ; ground_terms_by_sort = Dt.registered_terms_by_sort th
    }
  | Some (TArr th) ->
    { app_terms_by_symbol = Arr.app_terms_by_symbol th
    ; find_class_opt = Arr.find_class_opt th
    ; equal_if_registered = Arr.equal_if_registered th
    ; class_members = Arr.class_members th
    ; ground_terms_by_sort = Arr.registered_terms_by_sort th
    }
  | None -> Egraph_view.empty
;;

let live_registered_terms t =
  match t.theory with
  | Some (TCombined th) ->
    Oxsmt_euf.Euf_adapter.registered_terms (Combined.congruence_state th)
  | Some (TCombinedReal th) ->
    Oxsmt_euf.Euf_adapter.registered_terms (Combined_real.congruence_state th)
  | Some (TDt th) -> Dt.registered_terms th
  | Some (TArr th) -> Arr.registered_terms th
  | None -> []
;;

(* Capture the accepting candidate's equality classes before Sat.solve backtracks the
   theory trail. The engine supplies its exact registration-order universe; Cdclt's
   tag-sorted atom closure supplements it with pure-theory terms (notably LIA constants)
   that are valid quantifier substitutions but deliberately absent from congruence
   closure. Registered terms come first, preserving the existing candidate order;
   [Egraph_view.snapshot] closes and deduplicates the union. *)
let snapshot_egraph_view t =
  let live = live_egraph_view t in
  let ground_terms = live_registered_terms t @ subterms_sorted t in
  Egraph_view.snapshot live ~ground_terms
;;

let check t ~final =
  match t.theory with
  | None ->
    Sat.T_consistent [] (* no theory installed: propositional-only, nothing to add *)
  | Some impl ->
    if final
    then (
      (* effort (board #60): one seam Final-round. Ticked before the (possibly expensive)
         complete theory check so an exhausted budget cuts off here; a Final that returns
         a [Split] is the wired realization of a B&B node, so this subsumes "B&B nodes".
         May raise [Budget.Exceeded], which unwinds [Sat.solve] like
         [Split_budget_exceeded]. *)
      Budget.tick t.budget;
      match th_check impl Theory.Final with
      | Theory.Sat as r ->
        t.last_model <- Some (th_model impl);
        (* At the accepting Final the engine holds the satisfying assignment — the valid
           point to extract a checker model. For the standalone DT theory, snapshot its
           constructor-tree model (Dt_model_check re-derives the verdict from it); other
           theories have no tree model. *)
        t.last_dt_model
        <- (match impl with
            | TDt th -> Dt.check_model th
            | TCombined _ | TCombinedReal _ | TArr _ -> None);
        t.last_array_model
        <- (match impl with
            | TArr th -> Arr.array_model th
            | TCombined _ | TCombinedReal _ | TDt _ -> None);
        t.last_egraph_view
        <- (if t.capture_egraph then Some (snapshot_egraph_view t) else None);
        desugar_result t ~final:true r
      | r -> desugar_result t ~final:true r)
    else desugar_result t ~final:false (th_check impl Theory.Propagate)
;;

let explain t l =
  let a = Vartbl.find t.v2a (Sat.var_of_lit l) in
  let impl = ensure_theory t in
  let e = th_explain impl (Lit.make a (sign_lit l)) in
  let premise_lits = List.map (satlit_of_lit t) e.Explanation.premises in
  (match t.leaf_certificate_trace with
   | None -> ()
   | Some _ ->
     let clause = l :: List.map Sat.neg_lit premise_lits in
     record_euf_leaf t ~rule:e.Explanation.rule ~clause);
  premise_lits
;;

(* Test-only re-exports of the seam callbacks the SAT core drives internally, so the S4.2
   incremental-undo REDs (H1 zero-removal wipe, H2 cross-query index skew) can reproduce
   the exact driver behaviour against a REAL Combined theory without staging a full chrono
   solve — same discipline as {!desugar_result_for_test} (no re-implemented copy).
   [on_assign] and [on_chrono_rewind] are the very closures installed in {!create};
   [ckpt_log_length] reads the driver's trail-shadow so a test can assert the
   index-alignment invariant directly. *)
let on_assign_for_test = on_assign
let on_chrono_rewind_for_test = on_chrono_rewind
let check_for_test = check
let ckpt_log_length_for_test t = Dynarray.length t.ckpt_log

(* Install the seam callbacks into a pristine [sat] (no clauses, empty trail — the seam's
   set_theory contract). Must be called before any clause is added. The theory itself is
   created lazily at the first [intern] (see {!ensure_theory}) from the datatype
   [registry] (empty => the EUF+LIA stack), so a non-datatype session is byte-identical. *)
let create
  ctx
  env
  sat
  ~split_budget
  ~budget
  ~registry
  ~array_registry
  ~arithmetic_family
  ~cap
  =
  let t =
    { theory = None
    ; ctx
    ; env
    ; cap
    ; registry
    ; array_registry
    ; arithmetic_family
    ; sat
    ; alloc = Atom.create_allocator ()
    ; v2a = Vartbl.create 256
    ; v2term = Vartbl.create 256
    ; a2v = Atom.Table.create 256
    ; t2v = Term.Table.create 256
    ; is_split = Vartbl.create 16
    ; subterms = Term.Table.create 256
    ; level = 0
    ; split_budget
    ; splits = 0
    ; budget
    ; last_model = None
    ; capture_egraph = false
    ; last_egraph_view = None
    ; last_dt_model = None
    ; last_array_model = None
    ; relevancy = None
    ; leaf_certificate_trace = None
    ; ckpt_log = Dynarray.create ()
    }
  in
  Sat.set_theory
    sat
    (Some
       { Sat.on_assign = on_assign t
       ; on_backtrack = on_backtrack t
       ; check = check t
       ; explain = explain t
       ; (* S4.2 (dark): install the incremental-undo hook only under the flag; [None]
            (default) keeps the SAT core on its byte-identical full-rebuild chrono arm. *)
         on_chrono_rewind =
           (if Lazy.force incr_undo then Some (on_chrono_rewind t) else None)
       });
  (* Effort seam (board #60): the SAT core ticks the shared budget at each conflict /
     decision through this opaque closure, keeping [oxsmt_solver] budget-agnostic. *)
  Sat.set_budget_tick sat (Some (fun () -> Budget.tick budget));
  t
;;

(* Reset the per-check-sat split counter, effort budget, and stale model snapshot. *)
let begin_check t ~capture_egraph =
  t.splits <- 0;
  Budget.reset t.budget;
  t.capture_egraph <- capture_egraph;
  t.last_model <- None;
  t.last_egraph_view <- None;
  t.last_dt_model <- None;
  t.last_array_model <- None
;;

(* task #106: reset the LIA conflict-evidence stash so [last_conflict_core] reflects only
   the CURRENT check-sat (only the EUF+LIA stack carries it; DT/arrays have none). Kept
   SEPARATE from [begin_check] and called by {!Session.check_sat} at its very top so it
   also runs on the pure-BV fast path, which bypasses [begin_check] entirely — otherwise a
   pure-BV Unsat could surface a prior LIA check's stale core. *)
let clear_last_conflict t =
  match t.theory with
  | Some (TCombined th) ->
    Oxsmt_lia.Lia_adapter.clear_last_conflict (Combined.arith_state th)
  | Some (TCombinedReal th) ->
    Oxsmt_lia.Lra_adapter.clear_last_conflict (Combined_real.arith_state th)
  | Some (TDt _) | Some (TArr _) | None -> ()
;;

let splits_used t = t.splits
let effort_used t = Budget.used t.budget

(* task #106: passthrough of the LIA adapter's observational conflict evidence.
   Re-exported record so {!Session} can read the fields. Only the EUF+LIA stack carries
   it. *)
type conflict_core =
  { farkas : Oxsmt_lia.Rational.t list option
  ; atoms : (Term.t * bool) list
  }

let last_conflict_core t : conflict_core option =
  match t.theory with
  | Some (TCombined th) ->
    Option.map
      (fun (core : Oxsmt_lia.Lia_adapter.conflict_core) ->
        { farkas = core.farkas; atoms = core.atoms })
      (Oxsmt_lia.Lia_adapter.last_conflict_core (Combined.arith_state th))
  | Some (TCombinedReal th) ->
    Option.map
      (fun (core : Oxsmt_lia.Lra_adapter.conflict_core) ->
        { farkas = core.farkas; atoms = core.atoms })
      (Oxsmt_lia.Lra_adapter.last_conflict_core (Combined_real.arith_state th))
  | Some (TDt _) | Some (TArr _) | None -> None
;;

(* Convert a snapshot [Model.value] to the sidecar vocabulary. *)
let value_of (v : Model.value) =
  match v with
  | Model.Bool b -> VBool b
  | Model.Int n -> VInt n
  | Model.Real q -> VReal (Oxsmt_lia.Rational.of_big_frac ~num:q.Term.num ~den:q.Term.den)
  | Model.Uninterp i -> VUninterp i
;;

(* Reconstruct a nullary-symbol model from the snapshot taken at the accepting Final->Sat.
   Returns [None] when the last check-sat was not a theory [Sat], or when a checkable
   model would require a function table (any applied symbol appears among the subterms) or
   a needed nullary value is missing — the driver then reports [unknown] rather than an
   [sat] it cannot self-certify (see {!binding}). Deterministic: bindings sorted by symbol
   name (I6). *)
let model_bindings t =
  match t.last_model with
  | None -> None
  | Some m ->
    let exception Needs_table in
    (try
       let bindings =
         subterms_sorted t
         |> List.filter_map (fun (term : Term.t) ->
           match term.Term.node with
           | Term.App (sym, args) when Iarr.length args = 0 ->
             (match Model.value m term with
              | Some v -> Some (Const (Symbol.name sym, value_of v))
              | None -> raise Needs_table)
           | Term.App (_, _) -> raise Needs_table (* applied symbol: needs a table *)
           | _ -> None)
       in
       let name_of = function
         | Const (a, _) -> a
         | Fun (a, _) -> a
       in
       let by_name a b = String.compare (name_of a) (name_of b) in
       Some (List.sort by_name bindings)
     with
     | Needs_table -> None)
;;

(* Total order on model values (VBool < VInt < VReal < VUninterp), for canonical case
   ordering. *)
let value_compare (a : value) (b : value) =
  match a, b with
  | VBool x, VBool y -> Bool.compare x y
  | VBool _, _ -> -1
  | _, VBool _ -> 1
  | VInt x, VInt y -> Bigint.compare x y
  | VInt _, _ -> -1
  | _, VInt _ -> 1
  | VReal x, VReal y -> Oxsmt_lia.Rational.compare x y
  | VReal _, _ -> -1
  | _, VReal _ -> 1
  | VUninterp x, VUninterp y -> Int.compare x y
;;

(* Reconstruct the FINITE FUNCTION MODEL — uninterpreted-sort universes + const bindings +
   per-symbol function/predicate tables — from the snapshot taken at the accepting
   Final->Sat (ADR-UF-models §1). Reads only [Model.value] over the registered subterms. A
   [None] from [Model.value] skips a term the theory never valued; it is NOT a push/pop
   "liveness filter" (review F2, corrected): user push/pop is assumption-literal-based
   ([session.ml] guards frame clauses by a selector), NOT theory push/pop, so a popped
   frame's terms stay REGISTERED in the combinator and [Model.value] still returns their
   last value — a stale row is not filtered here. Post-pop soundness comes instead from
   the incremental query degrading to [unknown] (P6), not from this read. For the
   first-cut single-check-sat QF_UF corpus there is no pop, so every used term is live and
   the distinction is moot. Rider 2 holds regardless: NO e-graph mutation, NO euf.mli
   surface — the read is [Model.value] only. Element ids are the e-graph class ids
   remapped to dense 0-based per-sort indices. Returns [None] (=> [unknown], fail-closed)
   when a needed value is missing or a Bool-codomain (predicate) cell is unbound (buried
   H2 class; the combinator usually degrades that earlier via [Incomplete], guarded here
   too). Deterministic (R10): ascending class-id numbering, then canonical sort of sorts,
   bindings, and case tuples.

   {b QF_UFLIA §10 ℤ-realization (task #110).} An Int-sorted table cell (function argument
   or result) needs a concrete integer. Two sources: LIA valued it numerically
   ([Model.Int n] — keep n), or it is a pure-EUF Int class LIA never valued
   ([Model.Uninterp cid], surfaced by [combine.ml]'s model rather than omitted). Pass 1b
   realizes each such class to a concrete integer distinct from every LIA-used integer and
   distinct per class — respecting EUF (dis)equalities by construction (same class => same
   integer; distinct classes / a class vs a LIA value => different integers). The class
   appears in no LIA atom so LIA constrains it not at all, hence any such integer is a
   legal witness; a wrong choice can only make an assertion false under the R1 checker
   ([Model_check]) => [unknown], never a wrong [sat]. *)
let model t =
  match t.last_model with
  | None -> None
  | Some m ->
    let exception Degrade in
    (try
       let terms = subterms_sorted t in
       (* pass 1: per uninterpreted sort, gather distinct class ids -> dense 0-based index *)
       let sort_ids : (string, int list) Hashtbl.t = Hashtbl.create 16 in
       List.iter
         (fun (term : Term.t) ->
           match term.Term.sort with
           | Sort.Uninterpreted sym ->
             (match Model.value m term with
              | Some (Model.Uninterp cid) ->
                let name = Symbol.name sym in
                let prev =
                  match Hashtbl.find_opt sort_ids name with
                  | Some l -> l
                  | None -> []
                in
                Hashtbl.replace sort_ids name (cid :: prev)
              | _ -> ())
           | Sort.Bool
           | Sort.Int _
           | Sort.Real
           | Sort.Datatype _
           | Sort.Array _
           | Sort.BitVec _ -> ())
         terms;
       let index : (int, int) Hashtbl.t = Hashtbl.create 64 in
       let sort_cards = ref [] in
       Hashtbl.iter
         (fun name ids ->
           let ids = List.sort_uniq Int.compare ids in
           List.iteri (fun i cid -> Hashtbl.replace index cid i) ids;
           sort_cards := { sort_name = name; card = List.length ids } :: !sort_cards)
         sort_ids;
       (* pass 1b: the §10 ℤ-realization (task #110). An Int-sorted term LIA valued
          numerically arrives as [Model.Int n] (tier 1: keep n). An Int class LIA never
          valued arrives as [Model.Uninterp cid] (combine.ml's model surfaces a pure-EUF
          Int class here rather than omitting it): realize it to a concrete integer,
          distinct from every ALREADY-VALUED integer AND distinct per class. The exclusion
          pool ([int_used]) is EVERY [Model.Int n] appearing anywhere in the merged model
          — not merely integers LIA assigned to variables, but also constants AND numerals
          (an [Int_const] resolves to its own value via the combinator's [model_eval], so
          it is present here). This is load-bearing: a tier-2 class can carry an asserted
          disequality against a numeral or a valued constant (e.g. [x <> 5], the diseq
          routed to EUF only), and realizing away from the full valued set is exactly what
          keeps that disequality true. Respects EUF (dis)equalities by construction — a
          same-class term realizes to the SAME integer, distinct classes (and a class vs
          any valued integer) to DIFFERENT integers. The class appears in no LIA atom, so
          LIA constrains it not at all and any such integer is a legal witness; a wrong
          choice can only make an assertion false under the R1 checker (Model_check) ->
          [unknown], never a wrong [sat]. Deterministic (R10): least-unused-nonnegative
          over ASCENDING class ids, over the valued set. *)
       let int_used : (int, unit) Hashtbl.t = Hashtbl.create 64 in
       let int_classes = ref [] in
       List.iter
         (fun (term : Term.t) ->
           match term.Term.sort with
           | Sort.Int _ ->
             (match Model.value m term with
              | Some (Model.Int n) ->
                (* Record only values that fit int63: the fresh witnesses [fresh] mints
                   are small non-negative ints, so a >int63 used value (a uint256
                   constant) cannot collide with any witness and need not be excluded. *)
                (match Bigint.to_int_opt n with
                 | Some i -> Hashtbl.replace int_used i ()
                 | None -> ())
              | Some (Model.Uninterp cid) ->
                (* §10 v2 gap B (task #117): an [Arith] term (a linear composite used only
                   as a UF argument) is NOT realized to a fresh per-class integer — it is
                   EVALUATED structurally from its operands in [value_of], mirroring R1's
                   [ev], so its table key matches R1's structural evaluation. Only genuine
                   leaves (vars, UF apps) mint a fresh class integer. Skipping [Arith]
                   here keeps the fresh assignment DETERMINISTIC across v2 runs (R10:
                   least-unused over the sorted leaf-class ids); it intentionally does NOT
                   match v1's fresh-value stream — v1 minted for [Arith] cids too, so v2
                   hands out DIFFERENT integers to the surviving leaves. Determinism is a
                   within-version property, not cross-version equality. *)
                (match term.Term.node with
                 | Term.Arith _ -> ()
                 | _ -> int_classes := cid :: !int_classes)
              | _ -> ())
           | Sort.Bool
           | Sort.Real
           | Sort.Uninterpreted _
           | Sort.Datatype _
           | Sort.Array _
           | Sort.BitVec _ -> ())
         terms;
       (* Real realization mirrors the integer realization above, but stays exact and
          never projects a witness through a native integer. A pure-EUF Real class is
          absent from every LRA atom, so any fresh rational distinct from all LRA-used
          values and other classes is a legal witness. *)
       let real_used = ref [] in
       let real_classes = ref [] in
       List.iter
         (fun (term : Term.t) ->
           if Sort.equal term.sort Sort.real
           then (
             match Model.value m term with
             | Some (Model.Real q) ->
               let q = Oxsmt_lia.Rational.of_big_frac ~num:q.Term.num ~den:q.Term.den in
               if not (List.exists (Oxsmt_lia.Rational.equal q) !real_used)
               then real_used := q :: !real_used
             | Some (Model.Uninterp cid) ->
               (match term.node with
                | Term.Real_arith _ -> ()
                | _ -> real_classes := cid :: !real_classes)
             | Some (Model.Bool _ | Model.Int _) | None -> ())
           else ())
         terms;
       let real_realize : (int, Oxsmt_lia.Rational.t) Hashtbl.t = Hashtbl.create 64 in
       let next_real = ref Bigint.zero in
       let fresh_real () =
         let rec choose () =
           let q = Oxsmt_lia.Rational.of_bigint !next_real in
           next_real := Bigint.add !next_real Bigint.one;
           if List.exists (Oxsmt_lia.Rational.equal q) !real_used then choose () else q
         in
         let q = choose () in
         real_used := q :: !real_used;
         q
       in
       List.iter
         (fun cid ->
           if not (Hashtbl.mem real_realize cid)
           then Hashtbl.replace real_realize cid (fresh_real ()))
         (List.sort_uniq Int.compare !real_classes);
       let int_realize : (int, int) Hashtbl.t = Hashtbl.create 64 in
       let next = ref 0 in
       let fresh () =
         while Hashtbl.mem int_used !next do
           incr next
         done;
         let v = !next in
         Hashtbl.replace int_used v ();
         incr next;
         v
       in
       List.iter
         (fun cid ->
           if not (Hashtbl.mem int_realize cid)
           then Hashtbl.replace int_realize cid (fresh ()))
         (List.sort_uniq Int.compare !int_classes);
       let rec value_of (term : Term.t) =
         match Model.value m term, term.Term.node with
         | Some (Model.Bool b), _ -> VBool b
         | Some (Model.Int n), _ -> VInt n
         | Some (Model.Real q), _ ->
           VReal (Oxsmt_lia.Rational.of_big_frac ~num:q.Term.num ~den:q.Term.den)
         | _, Term.Arith lin ->
           (* §10 v2 gap B (task #117): a pure-EUF Int [Arith] term (LIA never numerically
              valued it — else the [Model.Int] arm above caught it, tier 1) is EVALUATED
              structurally over its operands, exactly as R1's [ev] does, so the table key
              this row is stored under equals the key R1 recomputes. Operands resolve
              recursively (leaves realize/inherit via the arms below); [Arith] children
              are non-[Arith] (term.ml invariant), so the recursion is shallow. The fold
              is exact arbitrary-precision arithmetic ({!Bigint}, core-bignum W2),
              matching R1's [Model_check] fold exactly — neither can overflow, so a big
              constant or coefficient is evaluated precisely rather than degrading the
              model. *)
           let step acc (child, coeff) =
             match value_of child with
             | VInt cv -> Bigint.add acc (Bigint.mul coeff cv)
             | _ -> raise Degrade
           in
           VInt (Iarr.fold step lin.Term.const lin.Term.coeffs)
         | _, Term.Real_arith lin ->
           let step acc (child, coeff) =
             match value_of child with
             | VReal cv ->
               let coeff =
                 Oxsmt_lia.Rational.of_big_frac ~num:coeff.Term.num ~den:coeff.Term.den
               in
               Oxsmt_lia.Rational.add acc (Oxsmt_lia.Rational.mul coeff cv)
             | _ -> raise Degrade
           in
           let const =
             Oxsmt_lia.Rational.of_big_frac
               ~num:lin.Term.const.Term.num
               ~den:lin.Term.const.Term.den
           in
           VReal (Iarr.fold step const lin.Term.coeffs)
         | Some (Model.Uninterp cid), _ ->
           (* An [Uninterp] value on an Int-sorted term is the §10 realize-me signal (pass
              1b); on an uninterpreted-sorted term it is the dense element index (pass 1). *)
           (match term.Term.sort with
            | Sort.Int _ ->
              (match Hashtbl.find_opt int_realize cid with
               | Some n -> VInt (Bigint.of_int n)
               | None -> raise Degrade)
            | Sort.Uninterpreted _ ->
              (match Hashtbl.find_opt index cid with
               | Some i -> VUninterp i
               | None -> raise Degrade)
            | Sort.Real ->
              (match Hashtbl.find_opt real_realize cid with
               | Some q -> VReal q
               | None -> raise Degrade)
            (* A datatype-sorted term reaching extraction has no certified value ([Model]
               offers no constructor-tree witness yet); combine already refuses to certify
               such a Sat, so this is a defensive backstop — degrade to no-model. *)
            | Sort.Bool | Sort.Datatype _ | Sort.Array _ | Sort.BitVec _ -> raise Degrade)
         | None, _ -> raise Degrade
       in
       let default_for (sort : Sort.t) =
         match sort with
         | Sort.Bool -> VBool false
         | Sort.Int _ -> VInt Bigint.zero
         | Sort.Real -> VReal Oxsmt_lia.Rational.zero
         | Sort.Uninterpreted _ -> VUninterp 0
         | Sort.Datatype _ | Sort.Array _ | Sort.BitVec _ -> raise Degrade
       in
       (* pass 2: non-Bool nullary consts + function/predicate table rows (per symbol) *)
       let consts = ref [] in
       let tables : (string, Sort.t * (value list * value) list ref) Hashtbl.t =
         Hashtbl.create 16
       in
       List.iter
         (fun (term : Term.t) ->
           match term.Term.node with
           | Term.App (sym, args) when Iarr.length args = 0 ->
             (match term.Term.sort with
              | Sort.Bool ->
                () (* propositional variable: session's bool_consts owns it *)
              | Sort.Int _
              | Sort.Real
              | Sort.Uninterpreted _
              | Sort.Datatype _
              | Sort.Array _
              | Sort.BitVec _ ->
                consts := Const (Symbol.name sym, value_of term) :: !consts)
           | Term.App (sym, args) ->
             let row = List.map value_of (Iarr.to_list args), value_of term in
             let name = Symbol.name sym in
             let rows =
               match Hashtbl.find_opt tables name with
               | Some (_, rows) -> rows
               | None ->
                 let rows = ref [] in
                 Hashtbl.replace tables name (term.Term.sort, rows);
                 rows
             in
             rows := row :: !rows
           | _ -> ())
         terms;
       let row_compare (a, ra) (b, rb) =
         match List.compare value_compare a b with
         | 0 -> value_compare ra rb
         | c -> c
       in
       let fun_bindings =
         Hashtbl.fold
           (fun name (codomain, rows) acc ->
             let cases = List.sort_uniq row_compare !rows in
             Fun (name, { default = default_for codomain; cases }) :: acc)
           tables
           []
       in
       let name_of = function
         | Const (n, _) -> n
         | Fun (n, _) -> n
       in
       let bindings =
         List.sort
           (fun a b -> String.compare (name_of a) (name_of b))
           (!consts @ fun_bindings)
       in
       let sort_cards =
         List.sort (fun a b -> String.compare a.sort_name b.sort_name) !sort_cards
       in
       Some (sort_cards, bindings)
     with
     | Degrade -> None)
;;

(* The DT constructor-tree checker model snapshotted at the accepting Final->Sat, or
   [None] when the last check-sat was not a DT-theory [Sat]. Read by {!Session}'s DT
   commit branch and validated by [Dt_model_check] before any [sat] is reported. *)
let dt_model t = t.last_dt_model

(* The arrays checker model snapshotted at the accepting Final->Sat, or [None] when the
   last check-sat was not an arrays-theory [Sat]. Read by {!Session}'s arrays commit
   branch and validated by [Array_model_check] before any [sat] is reported. *)
let array_model t = t.last_array_model

(* After an accepting Final, expose its immutable congruence snapshot: Sat.solve has
   already backtracked the live theory to level zero by the time Session asks the matcher
   for a view. Before any accepting candidate (or after begin_check cleared it), retain
   the live non-registering view for test-only/direct callers. *)
let egraph_view t =
  match t.last_egraph_view with
  | Some snapshot -> snapshot
  | None -> live_egraph_view t
;;

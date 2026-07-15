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
   - [on_assign lit]: forward the (registered) atom's signed literal to the theory
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

module Combined =
  Oxsmt_combine.Combine.Combine (Oxsmt_combine.Uflia_router) (Oxsmt_euf.Euf_adapter)
    (Oxsmt_lia.Lia_adapter)

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
  | TDt of Dt.t
  | TArr of Arr.t

let th_register impl a term =
  match impl with
  | TCombined th -> Combined.register_atom th a term
  | TDt th -> Dt.register_atom th a term
  | TArr th -> Arr.register_atom th a term
;;

let th_assert impl lit =
  match impl with
  | TCombined th -> Combined.assert_lit th lit
  | TDt th -> Dt.assert_lit th lit
  | TArr th -> Arr.assert_lit th lit
;;

let th_check impl effort =
  match impl with
  | TCombined th -> Combined.check th effort
  | TDt th -> Dt.check th effort
  | TArr th -> Arr.check th effort
;;

let th_explain impl lit =
  match impl with
  | TCombined th -> Combined.explain th lit
  | TDt th -> Dt.explain th lit
  | TArr th -> Arr.explain th lit
;;

let th_push impl =
  match impl with
  | TCombined th -> Combined.push th
  | TDt th -> Dt.push th
  | TArr th -> Arr.push th
;;

let th_pop impl n =
  match impl with
  | TCombined th -> Combined.pop th n
  | TDt th -> Dt.pop th n
  | TArr th -> Arr.pop th n
;;

let th_model impl =
  match impl with
  | TCombined th -> Combined.model th
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
    (* dynamic relevancy driver (task #24), [None] unless {!Session} installed one from the
     [OXSMT_RELEVANCY] gate. When [Some], the two trail seam events below stream to it so
     it can maintain relevancy marks in lockstep with the SAT trail; the branch filter
     itself is installed directly on the SAT core by {!Session}. A [None] arm is
     behaviourally inert — the theory glue is byte-identical with relevancy off. *)
  }

let sign_lit = Sat.sign_of_lit

(* Signed core [Lit.t] -> SAT literal. The atom is always registered (every atom the
   theory can name came through [intern]), so the lookup cannot miss for a correct theory. *)
let satlit_of_lit t (lit : Lit.t) =
  let v = Atom.Table.find t.a2v (Lit.atom lit) in
  if Lit.sign lit then Sat.pos v else Sat.neg v
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
    | Term.Bool_const _ | Term.Int_const _ -> ()
    | Term.App (_, args) -> Iarr.iter (collect t) args
    | Term.Arith lin -> Iarr.iter (fun (c, _) -> collect t c) lin.Term.coeffs
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

let ensure_theory t =
  match t.theory with
  | Some impl -> impl
  | None ->
    let impl =
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
    th_register impl a term)
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

let on_assign t l =
  (match t.relevancy with
   | None -> ()
   | Some rel ->
     Relevancy.on_assign
       rel
       ~var:(Sat.var_of_lit l)
       ~value:(sign_lit l)
       ~level:(Sat.decision_level t.sat));
  sync_level t;
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
      | Theory.Sat ->
        t.last_model <- Some (th_model impl);
        (* At the accepting Final the engine holds the satisfying assignment — the valid
           point to extract a checker model. For the standalone DT theory, snapshot its
           constructor-tree model (Dt_model_check re-derives the verdict from it); other
           theories have no tree model. *)
        t.last_dt_model
        <- (match impl with
            | TDt th -> Dt.check_model th
            | TCombined _ | TArr _ -> None);
        t.last_array_model
        <- (match impl with
            | TArr th -> Arr.array_model th
            | TCombined _ | TDt _ -> None);
        Sat.T_consistent []
      | Theory.Propagations lits -> Sat.T_consistent (List.map (satlit_of_lit t) lits)
      | Theory.Conflict e ->
        Sat.T_conflict (List.map (satlit_of_lit t) e.Explanation.premises)
      | Theory.Split terms ->
        t.splits <- t.splits + 1;
        if t.splits > t.split_budget then raise Split_budget_exceeded;
        Sat.T_lemma [ List.map (split_lit t ~sign:true) terms ])
    else (
      match th_check impl Theory.Propagate with
      | Theory.Propagations lits -> Sat.T_consistent (List.map (satlit_of_lit t) lits)
      | Theory.Conflict e ->
        Sat.T_conflict (List.map (satlit_of_lit t) e.Explanation.premises)
      | Theory.Sat | Theory.Split _ ->
        (* neither is legal at Propagate effort (THEORY contract); the theory never
           returns them here, but stay total and treat as "nothing to add". *)
        Sat.T_consistent [])
;;

let explain t l =
  let a = Vartbl.find t.v2a (Sat.var_of_lit l) in
  let impl = ensure_theory t in
  let e = th_explain impl (Lit.make a (sign_lit l)) in
  List.map (satlit_of_lit t) e.Explanation.premises
;;

(* Install the seam callbacks into a pristine [sat] (no clauses, empty trail — the seam's
   set_theory contract). Must be called before any clause is added. The theory itself is
   created lazily at the first [intern] (see {!ensure_theory}) from the datatype
   [registry] (empty => the EUF+LIA stack), so a non-datatype session is byte-identical. *)
let create ctx env sat ~split_budget ~budget ~registry ~array_registry ~cap =
  let t =
    { theory = None
    ; ctx
    ; env
    ; cap
    ; registry
    ; array_registry
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
    ; last_dt_model = None
    ; last_array_model = None
    ; relevancy = None
    }
  in
  Sat.set_theory
    sat
    (Some
       { Sat.on_assign = on_assign t
       ; on_backtrack = on_backtrack t
       ; check = check t
       ; explain = explain t
       });
  (* Effort seam (board #60): the SAT core ticks the shared budget at each conflict /
     decision through this opaque closure, keeping [oxsmt_solver] budget-agnostic. *)
  Sat.set_budget_tick sat (Some (fun () -> Budget.tick budget));
  t
;;

(* Reset the per-check-sat split counter, effort budget, and stale model snapshot. *)
let begin_check t =
  t.splits <- 0;
  Budget.reset t.budget;
  t.last_model <- None;
  t.last_dt_model <- None;
  t.last_array_model <- None
;;

let splits_used t = t.splits
let effort_used t = Budget.used t.budget

(* Convert a snapshot [Model.value] to the sidecar vocabulary. *)
let value_of (v : Model.value) =
  match v with
  | Model.Bool b -> VBool b
  | Model.Int n -> VInt n
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

(* Total order on model values (VBool < VInt < VUninterp), for canonical case ordering. *)
let value_compare (a : value) (b : value) =
  match a, b with
  | VBool x, VBool y -> Bool.compare x y
  | VBool _, _ -> -1
  | _, VBool _ -> 1
  | VInt x, VInt y -> Bigint.compare x y
  | VInt _, _ -> -1
  | _, VInt _ -> 1
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
            | Sort.Bool | Sort.Int _ | Sort.Datatype _ | Sort.Array _ | Sort.BitVec _ ->
              ())
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
            | Sort.Uninterpreted _
            | Sort.Datatype _
            | Sort.Array _
            | Sort.BitVec _ -> ())
         terms;
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

(* ADR-0012 L2/O3 (tranche 2): a read-only e-graph query view over the live congruence
   child, for the lemma tier's E-matcher. [Combined.congruence_state] hands back the
   concrete [Euf_adapter.t] (the combinator's own additive accessor, not a THEORY method),
   whose query functions forward to the engine's NON-REGISTERING accessors — so the
   matcher reads the congruence closure without growing it (R6). Rebuilt per [round] by
   [Session], since the e-graph changes as instances are asserted. *)
let egraph_view t : Oxsmt_ematch.Egraph_view.t =
  match t.theory with
  | Some (TCombined th) ->
    let cs = Combined.congruence_state th in
    { app_terms_by_symbol = (fun sym -> Oxsmt_euf.Euf_adapter.app_terms_by_symbol cs sym)
    ; find_class_opt = (fun term -> Oxsmt_euf.Euf_adapter.find_class_opt cs term)
    ; equal_if_registered = (fun a b -> Oxsmt_euf.Euf_adapter.equal_if_registered cs a b)
    ; class_members = (fun term -> Oxsmt_euf.Euf_adapter.class_members cs term)
    }
  | Some (TDt _) | Some (TArr _) | None ->
    (* the lemma tier's E-matcher runs only over the EUF+LIA stack; a datatype / arrays
       (or theory-free) session never reaches here (no quantified lemmas in that
       fragment). *)
    failwith "Cdclt.egraph_view: e-graph view is only available for the EUF+LIA theory"
;;

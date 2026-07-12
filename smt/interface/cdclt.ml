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

(* A model value for a symbol / table cell, in the eval-agnostic vocabulary the CLI
   renders to the §8 self-check sidecar grammar. [VUninterp i] is a 0-based ELEMENT INDEX
   into its uninterpreted sort's finite universe (NOT the raw e-graph class id —
   extraction remaps class ids to dense per-sort indices, ADR-UF-models §1/R10). *)
type value =
  | VBool of bool
  | VInt of int
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

type t =
  { theory : Combined.t
  ; sat : Sat.t
  ; alloc : Atom.allocator
  ; v2a : (Sat.var, Atom.t) Hashtbl.t (* SAT var -> theory atom (theory atoms only) *)
  ; v2term : (Sat.var, Term.t) Hashtbl.t (* SAT var -> its atom term *)
  ; a2v : Sat.var Atom.Table.t (* theory atom -> SAT var *)
  ; t2v : Sat.var Term.Table.t (* atom term -> SAT var (hash-cons sharing) *)
  ; is_split :
      (Sat.var, unit) Hashtbl.t (* atoms minted from a Split (need re-register) *)
  ; mutable subterms : Term.Set.t (* every subterm of a registered atom, for the model *)
  ; mutable level : int (* theory frames pushed above the base (= SAT decision level) *)
  ; split_budget : int
  ; mutable splits : int (* splits emitted in the current check-sat *)
  ; budget : Budget.t (* shared effort budget (board #60): SAT ticks it, we tick Final *)
  ; mutable last_model : Model.t option (* snapshot taken at the accepting Final->Sat *)
  }

let sign_lit = Sat.sign_of_lit

(* Signed core [Lit.t] -> SAT literal. The atom is always registered (every atom the
   theory can name came through [intern]), so the lookup cannot miss for a correct theory. *)
let satlit_of_lit t (lit : Lit.t) =
  let v = Atom.Table.find t.a2v (Lit.atom lit) in
  if Lit.sign lit then Sat.pos v else Sat.neg v
;;

(* Collect [term] and every subterm (all sorts), for reconstructing the model. *)
let rec collect t (term : Term.t) =
  if not (Term.Set.mem term t.subterms)
  then (
    t.subterms <- Term.Set.add term t.subterms;
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

(* Get-or-create the SAT var for a theory-atom [term], registering it with the combined
   theory on first sight (CONTRACT-REG). [split] flags an atom minted mid-solve from a
   [Split], whose e-node registration may later be truncated by a backjump. *)
let intern t ~split term =
  match Term.Table.find_opt t.t2v term with
  | Some v -> v
  | None ->
    let v = Sat.new_var t.sat in
    let a = Atom.fresh t.alloc in
    Term.Table.replace t.t2v term v;
    Hashtbl.replace t.v2a v a;
    Hashtbl.replace t.v2term v term;
    Atom.Table.replace t.a2v a v;
    if split then Hashtbl.replace t.is_split v ();
    collect t term;
    Combined.register_atom t.theory a term;
    v
;;

(* Public wrapper used by {!Session} during clausification (base-frame registration). *)
let intern_atom t term = intern t ~split:false term

(* Keep one theory frame per SAT decision level (I push lazily as levels open; a dummy
   assumption level can jump the level by more than one, hence the loop). *)
let sync_level t =
  let d = Sat.decision_level t.sat in
  while t.level < d do
    t.level <- t.level + 1;
    Combined.push t.theory
  done
;;

let on_assign t l =
  sync_level t;
  let v = Sat.var_of_lit l in
  match Hashtbl.find_opt t.v2a v with
  | None -> () (* an aux / selector / boolean-variable literal: not a theory atom *)
  | Some a ->
    (* a Split-minted atom's e-nodes may have been truncated by a pop; re-register
       (idempotent) so the child engines hold it before we assert. *)
    if Hashtbl.mem t.is_split v
    then Combined.register_atom t.theory a (Hashtbl.find t.v2term v);
    Combined.assert_lit t.theory (Lit.make a (sign_lit l))
;;

let on_backtrack t ~level =
  let n = t.level - level in
  if n > 0
  then (
    Combined.pop t.theory n;
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
  if final
  then (
    (* effort (board #60): one seam Final-round. Ticked before the (possibly expensive)
       complete theory check so an exhausted budget cuts off here; a Final that returns a
       [Split] is the wired realization of a B&B node, so this subsumes "B&B nodes". May
       raise [Budget.Exceeded], which unwinds [Sat.solve] like [Split_budget_exceeded]. *)
    Budget.tick t.budget;
    match Combined.check t.theory Theory.Final with
    | Theory.Sat ->
      t.last_model <- Some (Combined.model t.theory);
      Sat.T_consistent []
    | Theory.Propagations lits -> Sat.T_consistent (List.map (satlit_of_lit t) lits)
    | Theory.Conflict e ->
      Sat.T_conflict (List.map (satlit_of_lit t) e.Explanation.premises)
    | Theory.Split terms ->
      t.splits <- t.splits + 1;
      if t.splits > t.split_budget then raise Split_budget_exceeded;
      Sat.T_lemma [ List.map (split_lit t ~sign:true) terms ])
  else (
    match Combined.check t.theory Theory.Propagate with
    | Theory.Propagations lits -> Sat.T_consistent (List.map (satlit_of_lit t) lits)
    | Theory.Conflict e ->
      Sat.T_conflict (List.map (satlit_of_lit t) e.Explanation.premises)
    | Theory.Sat | Theory.Split _ ->
      (* neither is legal at Propagate effort (THEORY contract); the combinator never
         returns them here, but stay total and treat as "nothing to add". *)
      Sat.T_consistent [])
;;

let explain t l =
  let a = Hashtbl.find t.v2a (Sat.var_of_lit l) in
  let e = Combined.explain t.theory (Lit.make a (sign_lit l)) in
  List.map (satlit_of_lit t) e.Explanation.premises
;;

(* Install the combined theory into a pristine [sat] (no clauses, empty trail — the seam's
   set_theory contract). Must be called before any clause is added. *)
let create ctx env sat ~split_budget ~budget =
  let t =
    { theory = Combined.create ctx env
    ; sat
    ; alloc = Atom.create_allocator ()
    ; v2a = Hashtbl.create 256
    ; v2term = Hashtbl.create 256
    ; a2v = Atom.Table.create 256
    ; t2v = Term.Table.create 256
    ; is_split = Hashtbl.create 16
    ; subterms = Term.Set.empty
    ; level = 0
    ; split_budget
    ; splits = 0
    ; budget
    ; last_model = None
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
  t.last_model <- None
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
         Term.Set.elements t.subterms
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
  | VInt x, VInt y -> Int.compare x y
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
       let terms = Term.Set.elements t.subterms in
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
            | Sort.Bool | Sort.Int _ -> ())
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
               | Some (Model.Int n) -> Hashtbl.replace int_used n ()
               | Some (Model.Uninterp cid) -> int_classes := cid :: !int_classes
               | _ -> ())
            | Sort.Bool | Sort.Uninterpreted _ -> ())
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
       let value_of (term : Term.t) =
         match Model.value m term with
         | Some (Model.Bool b) -> VBool b
         | Some (Model.Int n) -> VInt n
         | Some (Model.Uninterp cid) ->
           (* An [Uninterp] value on an Int-sorted term is the §10 realize-me signal (pass
              1b); on an uninterpreted-sorted term it is the dense element index (pass 1). *)
           (match term.Term.sort with
            | Sort.Int _ ->
              (match Hashtbl.find_opt int_realize cid with
               | Some n -> VInt n
               | None -> raise Degrade)
            | Sort.Uninterpreted _ ->
              (match Hashtbl.find_opt index cid with
               | Some i -> VUninterp i
               | None -> raise Degrade)
            | Sort.Bool -> raise Degrade)
         | None -> raise Degrade
       in
       let default_for (sort : Sort.t) =
         match sort with
         | Sort.Bool -> VBool false
         | Sort.Int _ -> VInt 0
         | Sort.Uninterpreted _ -> VUninterp 0
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
               | Sort.Int _ | Sort.Uninterpreted _ ->
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

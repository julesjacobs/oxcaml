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

(* A model value for a nullary/function symbol, in the eval-agnostic vocabulary the CLI
   renders to the §8 self-check sidecar grammar. *)
type value =
  | VBool of bool
  | VInt of int
  | VUninterp of int

(* A binding of a nullary symbol to its model value. v1 emits only [Const] bindings: a
   complete UF model needs per-function tables, but [Combine.model]'s witness set is
   scoped to atoms + Int interface terms and does not surface a theory's uninterpreted
   leaves, so a table-free (LIA-only / pure-constant) model is exactly what is
   reconstructable here. {!model_bindings} returns [None] when a checkable model would
   need a function table (any applied symbol appears), and the driver then reports
   [unknown] rather than an unself-checkable [sat] — a completeness limitation, not a
   soundness one (follow-up: widen [Combine.model] / add function tables). *)
type binding = Const of string * value

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
let create ctx env sat ~split_budget =
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
  t
;;

(* Reset the per-check-sat split counter and stale model snapshot. *)
let begin_check t =
  t.splits <- 0;
  t.last_model <- None
;;

let splits_used t = t.splits

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
       let by_name (Const (a, _)) (Const (b, _)) = String.compare a b in
       Some (List.sort by_name bindings)
     with
     | Needs_table -> None)
;;

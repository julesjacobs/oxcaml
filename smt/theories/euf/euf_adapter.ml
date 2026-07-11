(* ADR-0005 THEORY adapter over the EUF engine. See euf_adapter.mli for the contract. This
   is a thin relabeling layer: the engine does all the reasoning and self-checks its
   explanations; the adapter maps Atom/Lit <-> the engine's opaque premise token and
   translates results into the frozen Explanation/Model currency. *)

open Oxsmt_core

(* The engine's ['p] is instantiated to [prem]. A real assertion carries [P_lit lit]; the
   standing [true <> false] disequality carries [P_axiom]. The engine never inspects a
   token — it only stores and returns it — so [P_axiom] rides along and is dropped when we
   build an [Explanation], leaving only literals the CDCL(T) engine actually asserted. *)
type prem =
  | P_lit of Lit.t
  | P_axiom

(* How an atom's term is encoded into the engine. A non-Bool [Eq(a,b)] atom asserts a
   (dis)equality on its two sides. A Bool-codomain predicate / bool constant is encoded
   against [true_const]/[false_const]. A [K_foreign] atom is one EUF does {e not} own
   (e.g. a LIA [Le]): the combinator registers it with EUF so congruence closes over its
   [App] subterms and the model can value them ({b register-not-assert}), but EUF never
   watches it, propagates it, explains it, or lets it be asserted. *)
type kind =
  | K_eq of Term.t * Term.t
  | K_bool
  | K_foreign

type info =
  { term : Term.t
  ; kind : kind
  }

type t =
  { engine : prem Euf.t
  ; true_const : Term.t
  ; false_const : Term.t
  ; atoms : info Atom.Table.t (* Atom -> its term + encoding; persists across pop *)
  ; watched : Atom.t Term.Table.t (* watched Eq-atom term -> Atom, for propagation *)
  ; mutable atom_terms : Term.t list (* registration order; model enumeration only *)
  }

let create ctx _env =
  let engine = Euf.create ctx in
  let true_const = Context.bool_const ctx true in
  let false_const = Context.bool_const ctx false in
  (* Registered + asserted at level 0 (before any [push]), so the axiom can never be
     popped away and re-registration after a pop cannot lose it. *)
  Euf.register_term engine true_const;
  Euf.register_term engine false_const;
  Euf.assert_neq engine ~premise:P_axiom true_const false_const;
  { engine
  ; true_const
  ; false_const
  ; atoms = Atom.Table.create 64
  ; watched = Term.Table.create 64
  ; atom_terms = []
  }
;;

let classify (term : Term.t) : kind =
  if not (Theory_view.is_atom term)
  then invalid_arg "Euf_adapter.register_atom: term is not a theory atom";
  match Theory_view.atom term with
  | Theory_view.Equality (a, b) -> K_eq (a, b)
  | Theory_view.Predicate (_, _) | Theory_view.Bool_lit _ -> K_bool
  | Theory_view.Le_zero _ -> K_foreign
;;

let register_atom t atom term =
  (* Always (re)internalize: [register_term] is idempotent (C7), and a [pop] may have
     truncated this atom's e-nodes — re-registering here rederives them. The [atoms] map
     is NOT trailed: the Atom<->term binding is permanent (CONTRACT-ATOM ids are stable),
     so keeping it across pops is what lets a later [assert_lit] recover the encoding. *)
  (* [register_term] internalises [term] AND its full subterm closure (post-order,
     CONTRACT-REG-1/2), so for a [K_foreign] atom this is exactly the "register every App/
     Int subterm" step — congruence fires over those App nodes with no extra walk. *)
  Euf.register_term t.engine term;
  if not (Atom.Table.mem t.atoms atom)
  then (
    let kind = classify term in
    Atom.Table.replace t.atoms atom { term; kind };
    t.atom_terms <- term :: t.atom_terms;
    match kind with
    | K_eq _ -> Term.Table.replace t.watched term atom
    | K_bool | K_foreign -> ())
;;

(* Internalise [term] (+ closure) into the e-graph with no atom binding — the combinator's
   boundary-term visibility hook (internalization ADR §3). Same engine call as
   [register_atom]'s internalisation (so idempotent / undone-by-pop identically); records
   [term] for [model] enumeration so a boundary term reachable via no owned atom is still
   valued. Never watched, asserted, propagated, or explained. *)
let internalize_term t term =
  Euf.register_term t.engine term;
  if not (List.memq term t.atom_terms) then t.atom_terms <- term :: t.atom_terms
;;

let assert_lit t lit =
  let atom = Lit.atom lit in
  let positive = Lit.sign lit in
  match Atom.Table.find_opt t.atoms atom with
  | None -> invalid_arg "Euf_adapter.assert_lit: atom was not registered"
  | Some { kind = K_eq (a, b); _ } ->
    if positive
    then Euf.assert_eq t.engine ~premise:(P_lit lit) a b
    else Euf.assert_neq t.engine ~premise:(P_lit lit) a b
  | Some { kind = K_bool; term } ->
    let target = if positive then t.true_const else t.false_const in
    Euf.assert_eq t.engine ~premise:(P_lit lit) term target
  | Some { kind = K_foreign; _ } ->
    (* A foreign atom is registered (so congruence sees its subterms) but never owned by
       EUF; the combinator's contract guarantees it is never asserted here. Fail loud so a
       contract violation is caught, not silently absorbed. *)
    invalid_arg "Euf_adapter.assert_lit: a foreign (non-EUF) atom must not be asserted"
;;

(* Drop the axiom token; keep only genuinely-asserted literals. Sound because the dropped
   fact ([true <> false]) is a theory tautology, not a hypothesis. *)
let lits_of_prems prems =
  List.filter_map
    (function
      | P_lit l -> Some l
      | P_axiom -> None)
    prems
;;

let check t effort =
  match Euf.check t.engine with
  | Euf.Conflict prems ->
    let premises = lits_of_prems prems in
    (* N1 (insurance): a conflict with no premises would be an unconditional [false] — a
       soundness bug. Unconstructible here (the violated disequality forces true=false or
       a merged asserted diseq, always citing >= 1 asserted literal; reflexive [Eq] folds
       to [true] before registration so no vacuous atom exists), asserted anyway. *)
    assert (premises <> []);
    Theory.Conflict { Explanation.premises; rule = Euf_congruence }
  | Euf.Consistent ->
    (* A watched Eq atom whose entailed truth just changed becomes a theory propagation —
       but only for atoms this adapter registered (C6); a watched Eq that is merely a
       subterm of some other atom has no [Atom] and is skipped. *)
    let lits =
      List.filter_map
        (fun (imp : Euf.implied) ->
           match Term.Table.find_opt t.watched imp.Euf.atom with
           | None -> None
           | Some atom -> Some (Lit.make atom imp.Euf.value))
        (Euf.propagate t.engine)
    in
    (match lits, effort with
     | [], Theory.Final -> Theory.Sat
     | [], Theory.Propagate -> Theory.Propagations []
     | _ :: _, _ -> Theory.Propagations lits)
;;

let explain t lit =
  let atom = Lit.atom lit in
  match Atom.Table.find_opt t.atoms atom with
  | Some { kind = K_eq _; term } ->
    (* Reconstruct the [implied] we propagated: its term is the Eq atom, its value is the
       literal's sign. [explain_implied] returns a precedence-valid premise set
       (CONTRACT-EX), self-checked by the engine. *)
    let imp = { Euf.atom = term; value = Lit.sign lit } in
    let premises = lits_of_prems (Euf.explain_implied t.engine imp) in
    (* N1 (insurance): an empty propagation reason is unconstructible — a registered [Eq]
       atom has distinct sides (reflexive folds to [true]), so proving it (dis)equal cites
       >= 1 asserted literal. *)
    assert (premises <> []);
    { Explanation.premises; rule = Euf_congruence }
  | _ -> invalid_arg "Euf_adapter.explain: literal was not propagated by this theory"
;;

let push t = Euf.push t.engine
let pop t n = Euf.pop t.engine n

(* Subterm children (same split as the engine's registration walk): used only to build a
   model total over every term reachable from a registered atom. *)
let children (term : Term.t) : Term.t list =
  match term.node with
  | Bool_const _ | Int_const _ -> []
  | App (_, args) -> Iarr.to_list args
  | Arith { coeffs; _ } -> List.map fst (Iarr.to_list coeffs)
  | Le a -> [ a ]
  | Eq (a, b) -> [ a; b ]
  | Not a -> [ a ]
  | And a | Or a -> Iarr.to_list a
  | Ite (c, a, b) -> [ c; a; b ]
;;

let model t =
  (* Assign every registered term (atoms + subterm closure) a witness by its congruence
     class: a Bool term provably [= true]/[= false] gets that boolean; otherwise the
     opaque class-representative id (open q3 encoding). Equal terms share a witness. *)
  let seen = Term.Table.create 64 in
  let acc = ref [] in
  let rec walk (term : Term.t) =
    if not (Term.Table.mem seen term)
    then (
      Term.Table.replace seen term ();
      let v =
        match term.node with
        | Eq (a, b) ->
          (* An equality is Bool-sorted, but its e-node is never merged with
             true/false_const — asserting the atom merges its SIDES, not the [Eq] node.
             Its truth is exactly whether the sides are congruent, so read that, rather
             than falling through to a stray [Uninterp] class id (codex HIGH: a shared
             equality term must carry Bool currency for N-O model combination). *)
          Model.Bool (Euf.are_equal t.engine a b)
        | _ ->
          if Sort.equal term.sort Sort.bool
          then
            if Euf.are_equal t.engine term t.true_const
            then Model.Bool true
            else if Euf.are_equal t.engine term t.false_const
            then Model.Bool false
            else Model.Uninterp (Euf.class_of t.engine term)
          else Model.Uninterp (Euf.class_of t.engine term)
      in
      acc := (term, v) :: !acc;
      List.iter walk (children term))
  in
  List.iter walk (List.rev t.atom_terms);
  Model.of_alist !acc
;;

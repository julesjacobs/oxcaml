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
  ; mutable explain_cache : Explanation.t Lit.Map.t
    (* propagated lit -> its reason, SNAPSHOTTED at propagation time so [explain] is
         O(1) and precedence-valid (CONTRACT-EX); see {!cache_reason} / the module note. *)
  ; mutable frames : Lit.t list list
    (* per-[push]-frame cached lits, head = current frame; used to drop stale reasons on
     [pop] in lockstep with the decision level that produced them. *)
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
  ; explain_cache = Lit.Map.empty
  ; frames = [ [] ]
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

(* The reason for a just-reported implied (dis)equality, SNAPSHOTTED at propagation time.
   [Euf.explain_implied] reconstructs the premise set from the engine's CURRENT proof
   forest — and that is precedence-valid ONLY here, at the instant [propagate] reports the
   flip: the literal has not yet been assigned on the SAT trail (the seam assigns it after
   [check] returns), so every premise is an assertion already strictly earlier on the
   trail. Deferring this to ask-time [explain] (the old behaviour) let later merges add
   union edges the forest walk would route through, yielding a premise asserted AFTER the
   explained literal — the CONTRACT-EX violation that bricked the QG / eq_diamond /
   EUF-in- QF_LIA families to [unknown]. Mirrors {!Lia_adapter.cache_reason}. *)
let reason_of_implied t (imp : Euf.implied) : Explanation.t =
  let premises = lits_of_prems (Euf.explain_implied t.engine imp) in
  (* N1 / codex AP4 tripwire (unconditional, survives release [-noassert]): an empty
     propagation reason is an unconditional entailment (soundness bug). A registered [Eq]
     atom has distinct sides (reflexive folds to [true]), so proving it (dis)equal cites
     >= 1 asserted literal; an empty set here is impossible, and raising degrades to
     [unknown] via CONTRACT-POISON rather than feeding 1UIP an unsound clause. *)
  if premises = []
  then failwith "Euf_adapter: empty propagation reason (unsound) [codex AP4 tripwire]";
  { Explanation.premises; rule = Euf_congruence }
;;

(* Cache a propagated literal's snapshotted reason in the current [push] frame. FIRST-WINS
   (mirrors {!Lia_adapter.cache_reason}, load-bearing for CONTRACT-EX): the first
   propagation's reason is the precedence-valid one; the engine only re-reports a watched
   atom after a [pop] resets its [w_reported] (which has already uncached the old entry
   via {!pop}), so a live double-report cannot occur, but the guard keeps the invariant
   robust. The reason's premises are on the trail at or below the current frame, so they
   cannot be popped without also popping (and uncaching) this entry. *)
let cache_reason t lit expl =
  if not (Lit.Map.mem lit t.explain_cache)
  then (
    t.explain_cache <- Lit.Map.add lit expl t.explain_cache;
    match t.frames with
    | fr :: rest -> t.frames <- (lit :: fr) :: rest
    | [] -> t.frames <- [ [ lit ] ])
;;

let check t effort =
  match Euf.check t.engine with
  | Euf.Conflict prems ->
    let premises = lits_of_prems prems in
    (* N1 (insurance): a conflict with no premises would be an unconditional [false] — a
       soundness bug. Unconstructible here (the violated disequality forces true=false or
       a merged asserted diseq, always citing >= 1 asserted literal; reflexive [Eq] folds
       to [true] before registration so no vacuous atom exists). UNCONDITIONAL guard, not
       [assert]: an empty premise set is an unconditional [false] (soundness bug), so this
       tripwire must survive the release [-noassert] build (codex AP4). Raising degrades
       to [unknown] via CONTRACT-POISON — never a verdict from an unsound conflict. *)
    if premises = []
    then failwith "Euf_adapter: empty conflict premise set (unsound) [codex AP4 tripwire]";
    Theory.Conflict { Explanation.premises; rule = Euf_congruence }
  | Euf.Consistent ->
    (* A watched Eq atom whose entailed truth just changed becomes a theory propagation —
       but only for atoms this adapter registered (C6); a watched Eq that is merely a
       subterm of some other atom has no [Atom] and is skipped. Each propagated literal's
       reason is SNAPSHOTTED now (precedence-valid — see {!reason_of_implied}) so ask-time
       [explain] serves the cache instead of re-deriving against a later forest. *)
    let lits =
      List.filter_map
        (fun (imp : Euf.implied) ->
           match Term.Table.find_opt t.watched imp.Euf.atom with
           | None -> None
           | Some atom ->
             let lit = Lit.make atom imp.Euf.value in
             cache_reason t lit (reason_of_implied t imp);
             Some lit)
        (Euf.propagate t.engine)
    in
    (match lits, effort with
     | [], Theory.Final -> Theory.Sat
     | [], Theory.Propagate -> Theory.Propagations []
     | _ :: _, _ -> Theory.Propagations lits)
;;

(* [explain] serves the reason SNAPSHOTTED at propagation time ({!cache_reason}); the
   ask-time re-derivation that violated CONTRACT-EX is gone. Every literal EUF propagates
   is cached in [check] before the seam assigns it, and it stays cached until its frame is
   popped (at which point it is off the trail and will not be explained), so a live
   [explain] always hits. A miss is a driver/contract violation (an [explain] for a
   literal EUF never propagated, or one whose frame was already popped) — fail loud so it
   degrades to [unknown] via CONTRACT-POISON rather than fabricating an unsound premise
   set. *)
let explain t lit =
  match Lit.Map.find_opt lit t.explain_cache with
  | Some expl -> expl
  | None ->
    failwith
      "Euf_adapter.explain: no cached reason for literal (not theory-propagated, or its \
       frame was popped)"
;;

let push t =
  Euf.push t.engine;
  t.frames <- [] :: t.frames
;;

let pop t n =
  Euf.pop t.engine n;
  (* Drop the last [n] frames, uncaching every reason they hold: a propagation's
     snapshotted reason is valid only at the decision level that produced it (its premises
     unwind with that level). Keep at least a root frame. Mirrors {!Lia_adapter.pop}. *)
  let rec drop k frames =
    if k = 0
    then frames
    else (
      match frames with
      | fr :: rest ->
        List.iter (fun l -> t.explain_cache <- Lit.Map.remove l t.explain_cache) fr;
        drop (k - 1) rest
      | [] -> [])
  in
  t.frames
  <- (match drop n t.frames with
      | [] -> [ [] ]
      | fs -> fs)
;;

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

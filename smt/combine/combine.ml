open Oxsmt_core

module type ROUTER = sig
  type owner =
    | A
    | B
    | Both

  val owner : Term.t -> owner
  val assert_to : Term.t -> positive:bool -> owner
  val equality_split : Context.t -> Term.t -> Term.t -> Term.t list
end

exception Combination_unsound of string
exception Incomplete of string

module type CONGRUENCE_CHILD = sig
  include Theory.THEORY

  val internalize_term : t -> Term.t -> unit
end

module Combine (R : ROUTER) (A : CONGRUENCE_CHILD) (B : Theory.THEORY) : sig
  include Theory.THEORY

  (* ADR-0012 L2/O3: read-only exposure of the congruence child's state so the session can
     build the lemma-tier e-graph query view over the concrete EUF adapter. Additive; the
     [Theory.THEORY] seam the engine drives is unchanged. *)
  type congruence_state = A.t

  val congruence_state : t -> congruence_state
end = struct
  (* A pinned shared-equality literal: the pair it relates, its asserted polarity, and
     which children it was actually asserted to (a negative equality may reach only the
     congruence child — CONTRACT S1). At Final every routed child's model must satisfy it. *)
  type pin =
    { px : Term.t
    ; py : Term.t
    ; psign : bool
    ; pto_a : bool
    ; pto_b : bool
    }

  type t =
    { ctx : Context.t
    ; a : A.t
    ; b : B.t
    ; (* atom -> its Term, recorded at [register_atom]; assert_lit/explain need the term
         to consult the polarity-aware router. Monotone (atom ids are stable). *)
      atom_term : Term.t Atom.Table.t
    ; (* every subterm of every registered atom — the merged-model domain ({!model}).
         Monotone; tag-ordered. *)
      mutable all_terms : Term.Set.t
    ; (* the INTERFACE SET — boundary-crossing nodes plus both-used neutral variables, the
         candidate domain for {!find_disagreement} (internalization ADR §3.1). A node is a
         boundary member when it is owned (EUF/LIA by head, {!node_owner}) and has a
         parent EDGE whose owner differs — an arith-headed node under an uninterpreted
         [f], an [App] inside a sum, a numeral under [f]. Computed by a total structural
         walk of each asserted term ({!interface_walk}), never a relevance filter, so
         sharedness is total by construction — the W1/R1 wrong-SAT family (a too-small
         approximation) and the fresh-sum non-termination (a too-large one) cannot arise.
         Monotone/grow-only (invariant (i)); the both-valued skip in {!find_disagreement}
         drops stale members. Replaces round-7's [euf_domain] gate — the interface set
         already excludes the top LIA sum (arith under arith), so no per-case Arith gate
         is needed. *)
      mutable interface : Term.Set.t
    ; (* the two halves of the both-used-neutral-variable clause (ADR §3.1): a neutral
         (ownerless) variable enters the interface ONLY when it is used as an operand by
         BOTH an EUF-owned and a LIA-owned node. These record the per-owner use bits as
         the walk observes [(node, parent-owner)] occurrences (invariant (ii), P6
         reformulation: membership is the monotone union of observed occurrences).
         Grow-only. *)
      mutable euf_used : Term.Set.t
    ; mutable lia_used : Term.Set.t
    ; (* Bool leaves / Bool-returning applications used as an argument of an uninterpreted
         function (ADR §3.6 cases (i)/Bool-returning-UF). Sound ONLY when EUF has bound
         the term to [true_const]/[false_const] — which happens iff it surfaced as a SAT
         atom (a top-level literal routed to EUF via K_bool). A BURIED such term (only
         under the UF argument, never a SAT atom) stays a third opaque Boolean class in
         EUF, so h(·) congruence would wrong-SAT (codex H2 / its Bool-returning-UF
         sibling). At the Sat certification point {!combine_models} requires each member
         to be bound, else degrades to {!Incomplete} (team-lead ruling: buried-unbound →
         unknown; a surfaced leaf stays decidable). Grow-only. *)
      mutable bool_uf_args : Term.Set.t
    ; (* which child propagated a literal — for routing [explain]. Keyed on the SIGNED
         {!Lit.t} (not the atom): A propagating [+e] and B propagating [-e] must not
         clobber each other, else [explain] returns the wrong premise set and 1UIP learns
         a wrong clause (codex C1). *)
      mutable propagated_by : R.owner Lit.Map.t
    ; (* shared-equality literals pinned on the trail, one frame per open [push] level
         (head = current). Carry sign + routed children (codex C2). Backtracked by [pop]. *)
      mutable pin_frames : pin list list
    ; (* atom -> the (x, y) it equates, for a shared equality over two interface terms. *)
      eq_pair : (Term.t * Term.t) Atom.Table.t
    }

  let create ctx env =
    { ctx
    ; a = A.create ctx env
    ; b = B.create ctx env
    ; atom_term = Atom.Table.create 64
    ; all_terms = Term.Set.empty
    ; interface = Term.Set.empty
    ; euf_used = Term.Set.empty
    ; lia_used = Term.Set.empty
    ; bool_uf_args = Term.Set.empty
    ; propagated_by = Lit.Map.empty
    ; pin_frames = [ [] ]
    ; eq_pair = Atom.Table.create 16
    }
  ;;

  (* Collect a term and every subterm — the model domain. The membership guard is
     load-bearing: terms are a hash-consed DAG (shared subterms are the NORM), so without
     it a shared child is re-walked once per path — exponential (e.g. an [f (t,t)] tower
     of depth n has n+1 distinct nodes but 2^n paths). [t ∈ acc] implies its subterms are
     already in [acc] (they are added in the same call that adds [t]), so skipping is
     sound and makes the walk O(distinct subterms). *)
  let rec add_subterms acc (t : Term.t) =
    if Term.Set.mem t acc
    then acc
    else (
      let acc = Term.Set.add t acc in
      match t.Term.node with
      | Term.App (_, args) -> Iarr.fold add_subterms acc args
      | Term.Arith lin ->
        Iarr.fold (fun acc (c, _) -> add_subterms acc c) acc lin.Term.coeffs
      | Term.Le a -> add_subterms acc a
      | Term.Eq (a, b) -> add_subterms (add_subterms acc a) b
      | Term.Not a -> add_subterms acc a
      | Term.And xs | Term.Or xs -> Iarr.fold add_subterms acc xs
      | Term.Ite (a, b, c) -> add_subterms (add_subterms (add_subterms acc a) b) c
      | Term.Bool_const _ | Term.Int_const _ -> acc)
  ;;

  (* A shared equality atom [Eq (x, y)] over non-Bool sides: record the pair for pinning. *)
  let note_eq_pair t atom term =
    match term.Term.node with
    | Term.Eq (x, y) when not (Sort.equal x.Term.sort Sort.bool) ->
      Atom.Table.replace t.eq_pair atom (x, y)
    | _ -> ()
  ;;

  (* --- the interface walk (internalization ADR §3.1) -------------------------------- *)

  (* Structural ownership by a node's HEAD (combinator-local, NOT a router method — the
     router classifies ATOMS for register/assert routing; this classifies every NODE for
     the boundary walk). An uninterpreted application (arity >= 1) and the Bool constants
     [true]/[false] are EUF's (§3.1: the constants ARE EUF's anchored [true_const]/
     [false_const], so [h false] is EUF-under-EUF, never a crossing); arithmetic
     operators, order, and numerals are LIA's; a bare variable (nullary [App]) and the
     Bool connectives are NEUTRAL (ownerless). *)
  type owner_kind =
    | O_euf
    | O_lia
    | O_neutral

  let node_owner (term : Term.t) =
    match term.Term.node with
    | Term.App (_, args) -> if Iarr.length args > 0 then O_euf else O_neutral
    | Term.Arith _ | Term.Le _ | Term.Int_const _ -> O_lia
    | Term.Bool_const _ -> O_euf
    | Term.Eq _ | Term.Not _ | Term.And _ | Term.Or _ | Term.Ite _ -> O_neutral
  ;;

  let owner_code = function
    | O_euf -> 0
    | O_lia -> 1
    | O_neutral -> 2
  ;;

  (* Children for the structural walk — descends EVERY node kind, in particular BOTH sides
     of an [Eq] (the [f x = x + y] shape: the walk must visit the left [f x] AND the sum
     on the right, recording each crossing; a walk that descended one side would miss a
     buried crossing — ADR §3.1 C2 / the mixed-equality totality test). *)
  let walk_children (term : Term.t) : Term.t list =
    match term.Term.node with
    | Term.Bool_const _ | Term.Int_const _ -> []
    | Term.App (_, args) -> Iarr.to_list args
    | Term.Arith lin -> List.map fst (Iarr.to_list lin.Term.coeffs)
    | Term.Le a -> [ a ]
    | Term.Eq (a, b) -> [ a; b ]
    | Term.Not a -> [ a ]
    | Term.And xs | Term.Or xs -> Iarr.to_list xs
    | Term.Ite (a, b, c) -> [ a; b; c ]
  ;;

  (* Walk one asserted term, growing the interface set (ADR §3.1, invariant (ii)). Post-
     order, DAG-safe: memoised on [(node, parent-owner)] because boundary-ness is per-USE
     (a hash-consed node has many parents; [x+1] under [f] is a crossing while [x+1]
     inside another sum is not, and a neutral variable's use bit is per parent-owner) — a
     term-only memo could drop the second occurrence and miss a both-used variable. Each
     node is visited under at most the 3 distinct parent-owners, so the walk stays linear.
     The TOP atom is entered with [parent_owner = O_neutral] (no crossing at the root).

     PRECONDITION (codex): the walk runs on the PREPROCESSED fragment — no residual
     Int-sorted [Ite] and no reserved [div]/[mod] applications (Term.Debug [Pipeline]
     mode, ADR-0003 invariant 10). An Int [Ite] is a [Neutral] node whose Int branches
     under a [Neutral] parent would take no use-bit; since preprocessing removes it before
     any assertion reaches a theory, that case is unreachable here. A Bool [Ite] as a UF
     argument still degrades (§3.6 (ii), below), independent of this precondition. *)
  let interface_walk t (top : Term.t) =
    let visited : (int * int, unit) Hashtbl.t = Hashtbl.create 64 in
    (* set an owner use-bit on a node; both bits set ⇒ it is a both-used interface member *)
    let mark_use (term : Term.t) owner =
      (match owner with
       | O_euf -> t.euf_used <- Term.Set.add term t.euf_used
       | O_lia -> t.lia_used <- Term.Set.add term t.lia_used
       | O_neutral -> ());
      if Term.Set.mem term t.euf_used && Term.Set.mem term t.lia_used
      then t.interface <- Term.Set.add term t.interface
    in
    let rec go ~parent_owner (term : Term.t) =
      let key = term.Term.tag, owner_code parent_owner in
      if not (Hashtbl.mem visited key)
      then (
        Hashtbl.replace visited key ();
        let o = node_owner term in
        let is_int =
          match term.Term.sort with
          | Sort.Int _ -> true
          | Sort.Bool | Sort.Uninterpreted _ -> false
        in
        (* PRECONDITION defensive check (codex): preprocessing lifts every Int-sorted
           [Ite] before assertion (ADR-0003 invariant 10); a residual one would take no
           use-bit for its neutral-parented Int branches and silently under-approximate.
           UNCONDITIONAL guard, not [assert]: a soundness tripwire that must survive the
           release [-noassert] build (codex AP4), so raise [Combination_unsound] (→ engine
           CONTRACT-POISON → [unknown]) rather than let a residual Int-[Ite] pass. *)
        (match term.Term.node with
         | Term.Ite _ when is_int ->
           raise
             (Combination_unsound
                "residual Int-Ite in interface walk: preprocessing must lift it \
                 (ADR-0003 inv. 10) [codex AP4 tripwire]")
         | _ -> ());
        (* boundary Int node: an OWNED node under a parent EDGE whose owner differs (ADR
           §3.1 "for each parent→child edge it compares owners and records a crossing").
           This INCLUDES an owned side of an equality atom (parent [Eq] is neutral): the
           mixed-equality totality requirement (§6) is that the walk on [f x = x + y]
           record BOTH [f x] and the sum — a side that EUF and LIA can disagree on (e.g.
           an Int disequality NOT routed to LIA, S1) must be an interface member or the
           merged model can leak an EUF-inconsistent LIA arrangement. Neutral parent still
           bounds nothing on its own — a neutral CHILD (bare var) is never a boundary (no
           owner to differ); it enters only via the both-used clause below. *)
        if is_int && o <> O_neutral && o <> parent_owner
        then t.interface <- Term.Set.add term t.interface;
        (* neutral Int variable: record the per-owner use bit; both bits set ⇒ interface *)
        if is_int && o = O_neutral && parent_owner <> O_neutral
        then mark_use term parent_owner;
        (* H1 (codex): the congruence child DECIDES every equality (merge for [=], diseq
           for [≠]), so a bare Int variable that is an OPERAND of an equality atom is
           EUF-used. Combined with a LIA arithmetic occurrence it becomes a both-used
           interface member — the [(distinct x y) ∧ x≤y ∧ y≤x] class: the diseq routes to
           EUF only (S1), LIA entails the equality, and without this bit the interface is
           empty and the disagreement is missed (wrong SAT). An OWNED equality side ([App]
           / a sum) is already caught by the boundary rule above; only a bare-variable
           side needs this. *)
        (match term.Term.node with
         | Term.Eq (a, b) when not (Sort.equal a.Term.sort Sort.bool) ->
           List.iter
             (fun (side : Term.t) ->
                match side.Term.node, side.Term.sort with
                | Term.App (_, sa), Sort.Int _ when Iarr.length sa = 0 ->
                  mark_use side O_euf
                | _ -> ())
             [ a; b ]
         | _ -> ());
        (* Bool boundary — a Bool node as an argument of an uninterpreted function
           ([parent_owner = O_euf]); ADR §3.6. No integer arrangement. *)
        (match term.Term.sort, parent_owner with
         | Sort.Bool, O_euf ->
           (match term.Term.node with
            (* (i) bare Bool variable → leaf, and a Bool-returning UF ([h (g x)], g : … →
               Bool). BOTH are native ONLY if EUF binds them to [true_const]/[false_const]
               (i.e. they surfaced as a SAT atom asserted via K_bool). A BURIED occurrence
               (only here, never a SAT atom) stays a third opaque Boolean class →
               wrong-SAT (codex H2 + its Bool-returning-UF sibling). Record for the
               Sat-point binding check in {!combine_models} rather than deciding now; the
               walk cannot yet know whether the term will surface. *)
            | Term.App (_, _) -> t.bool_uf_args <- Term.Set.add term t.bool_uf_args
            (* (i') Bool constant → native EUF [true_const]/[false_const], nothing to bind *)
            | Term.Bool_const _ -> ()
            (* (ii) STRUCTURED Bool compound → degrade (C6: the leaf bridge names a
               nullary leaf, so a compound argument would decouple from its operands and
               wrong-SAT). A LIA order atom [Le] as a UF argument degrades for the same
               reason — its truth lives in LIA/SAT with no channel into EUF congruence. A
               sound completeness degrade, distinct from a soundness poison. *)
            | Term.And _ | Term.Or _ | Term.Not _ | Term.Ite _ | Term.Eq _ | Term.Le _ ->
              raise
                (Incomplete
                   "structured Bool compound as an uninterpreted-function argument")
            (* Int-sorted nodes are unreachable under [Sort.Bool] (frozen 9-node set). *)
            | Term.Int_const _ | Term.Arith _ -> ())
         | (Sort.Bool | Sort.Int _ | Sort.Uninterpreted _), _ -> ());
        List.iter (go ~parent_owner:o) (walk_children term))
    in
    go ~parent_owner:O_neutral top
  ;;

  (* Internalize into the congruence child A exactly the uninterpreted-application
     subterms of a foreign (LIA-owned) atom — the EUF e-graph MEMBERSHIP RULE (DESIGN.md
     A4 erratum): EUF's cost must be proportional to the UNINTERPRETED structure, not the
     term count. The e-graph needs exactly (i) uninterpreted applications [f(…)]/[p(…)]
     and (ii) their argument subterms (the boundary nodes) — [Euf.register_term] on a
     maximal [App] pulls in that whole argument closure by its post-order recursion, so
     descending into an [App] here is unnecessary and we stop at it. A pure-arithmetic
     term never under an uninterpreted symbol gets NO e-node: congruence provably cannot
     conclude anything about it (its head [+]/[≤]/numeral is interpreted), and the
     shared-value reasoning [x=y ⟹ x+1 ~ y+1] flows through the SEAM's value comparison,
     never through congruence. So a pure-LIA [Le] atom internalizes nothing — the "UF-free
     skip" is just the empty instance of this rule, with no switch and no stale-flag
     hazard. This REPLACES the previous [A.internalize_term term] on the whole atom (which
     internalized the full arithmetic closure — the euf-tax-on-LIA the perf analysis
     measured). Monotone/grow-only: a term enters when its first under-[f] occurrence is
     registered, exactly like boundary status. Direction: over-inclusion is merely slow,
     under-inclusion is the wrong-SAT direction (an [f]-argument missing its e-node would
     drop the W1 congruence) — the registry mutant guards it. The W1 hazard stays covered
     BY CONSTRUCTION: [f]'s argument subterms are in the set, so [f(x)]/[f(y)] still
     become congruent under an asserted [x=y]. *)
  let internalize_uf_subterms t (term : Term.t) =
    let rec go (u : Term.t) =
      match u.Term.node with
      | Term.App (_, args) when Iarr.length args > 0 -> A.internalize_term t.a u
      | _ -> List.iter go (walk_children u)
    in
    go term
  ;;

  let register_atom t atom term =
    Atom.Table.replace t.atom_term atom term;
    t.all_terms <- add_subterms t.all_terms term;
    (match R.owner term with
     | R.A -> A.register_atom t.a atom term
     | R.B ->
       (* A LIA-only atom: register it with B, and internalize into the congruence child A
          ONLY its uninterpreted-application subterms (the membership rule; see
          {!internalize_uf_subterms}). This keeps the W1 fix (EUF sees an [App] that
          surfaces only inside a LIA atom, so [f x],[f y] under a [≤] still
          congruence-close under [x = y]) while paying EUF cost proportional to the UF
          structure, not the whole arithmetic closure — replacing the previous whole-atom
          internalize. *)
       B.register_atom t.b atom term;
       internalize_uf_subterms t term
     | R.Both ->
       A.register_atom t.a atom term;
       B.register_atom t.b atom term);
    interface_walk t term;
    note_eq_pair t atom term
  ;;

  let term_of t atom =
    match Atom.Table.find_opt t.atom_term atom with
    | Some term -> term
    | None ->
      (* Every asserted atom is registered first (CONTRACT-REG); an unrouted literal is an
         engine contract break. Refuse to guess — degrade to [unknown]. *)
      raise
        (Combination_unsound "assert_lit / explain on an atom never seen by register_atom")
  ;;

  (* Record a pinned shared-equality literal in the current frame. *)
  let pin t ~x ~y ~psign ~pto_a ~pto_b =
    let p = { px = x; py = y; psign; pto_a; pto_b } in
    match t.pin_frames with
    | frame :: rest -> t.pin_frames <- (p :: frame) :: rest
    | [] -> t.pin_frames <- [ [ p ] ]
  ;;

  let all_pins t = List.concat t.pin_frames

  let assert_lit t lit =
    let atom = Lit.atom lit in
    let term = term_of t atom in
    let positive = Lit.sign lit in
    let owner = R.assert_to term ~positive in
    (match owner with
     | R.A -> A.assert_lit t.a lit
     | R.B -> B.assert_lit t.b lit
     | R.Both ->
       A.assert_lit t.a lit;
       B.assert_lit t.b lit);
    match Atom.Table.find_opt t.eq_pair atom with
    | None -> ()
    | Some (x, y) ->
      let pto_a =
        match owner with
        | R.A | R.Both -> true
        | R.B -> false
      in
      let pto_b =
        match owner with
        | R.B | R.Both -> true
        | R.A -> false
      in
      pin t ~x ~y ~psign:positive ~pto_a ~pto_b
  ;;

  let record_props t owner lits =
    List.iter (fun l -> t.propagated_by <- Lit.Map.add l owner t.propagated_by) lits
  ;;

  (* --- model-based combination (Final) --------------------------------------------- *)

  let value_equal (u : Model.value) (v : Model.value) =
    match u, v with
    | Model.Int a, Model.Int b -> a = b
    | Model.Bool a, Model.Bool b -> Bool.equal a b
    | Model.Uninterp a, Model.Uninterp b -> a = b
    | _ -> false
  ;;

  (* Overflow-GUARDED native-int arithmetic for the fold (codex W2): a raw [acc + coeff*v]
     silently wraps (e.g. [max_int * 2 = -2]), which would let [check_pins] read a
     VIOLATED pin as satisfied — a wrong [Sat], the L2 overflow family reborn. On overflow
     we RAISE (→ CONTRACT-POISON → [unknown]), never wrap. *)
  let add_guard a b =
    let r = a + b in
    (* overflow iff the operands share a sign that the result does not *)
    if Bool.equal (a >= 0) (b >= 0) && not (Bool.equal (r >= 0) (a >= 0))
    then raise (Combination_unsound "model evaluation: integer addition overflow")
    else r
  ;;

  let mul_guard a b =
    if a = 0 || b = 0
    then 0
    else (
      let r = a * b in
      if r / a <> b || (a = -1 && b = min_int) || (b = -1 && a = min_int)
      then raise (Combination_unsound "model evaluation: integer multiplication overflow")
      else r)
  ;;

  (* EVALUATE a term through a child's model. A child (esp. the arithmetic one) keys only
     its LEAVES — [Model.value lia (x + 1)] is [None] even though the model fixes [x] — so
     an [Arith] compound is folded over its leaf values (codex round-2 C2), with
     overflow-guarded arithmetic (W2). [None] means a genuine LEAF is unvalued, not merely
     a compound un-keyed. *)
  let rec model_eval model (t : Term.t) : Model.value option =
    match Model.value model t with
    | Some v -> Some v
    | None ->
      (match t.Term.node with
       | Term.Int_const n -> Some (Model.Int n)
       | Term.Arith lin ->
         let rec fold acc = function
           | [] -> Some (Model.Int acc)
           | (child, coeff) :: rest ->
             (match model_eval model child with
              | Some (Model.Int v) -> fold (add_guard acc (mul_guard coeff v)) rest
              | _ -> None)
         in
         fold lin.Term.const (Iarr.to_list lin.Term.coeffs)
       | _ -> None)
  ;;

  (* codex C2 (round-2 refinement) — every pinned equality literal must be satisfied (SIGN
     included) by the model of EACH child it was asserted to. The two sides are EVALUATED
     through the child's model (folding [Arith] over leaf values), NOT looked up raw — an
     unkeyed compound like [x + 1] is the ubiquitous [y = x + 1] shape, not a violation. A
     pin is unverifiable (→ fail-safe degrade) ONLY when a genuine leaf is unvalued, which
     for a child that certified [Sat] over its asserted set is a real contract breach. *)
  let check_pins t ma mb =
    List.iter
      (fun p ->
         let ok m =
           match model_eval m p.px, model_eval m p.py with
           | Some vx, Some vy -> Bool.equal (value_equal vx vy) p.psign
           | _ -> false
         in
         if (p.pto_a && not (ok ma)) || (p.pto_b && not (ok mb))
         then
           raise
             (Combination_unsound "child Sat-model violates an asserted shared equality"))
      (all_pins t)
  ;;

  (* The tag-least pair of INTERFACE nodes the two models place in DIFFERENT equality
     relations. Domain = the interface set ({!interface_walk}: boundary-crossing nodes +
     both-used neutral variables) filtered to Int-sorted members that BOTH models VALUE
     (via [model_eval], folding compounds). The interface set is total by construction, so
     no shared term is invisible (the W1/R1 wrong-SAT family cannot arise), and it already
     EXCLUDES the top LIA sum (arith under arith, not a crossing) and every non-boundary
     [Arith] — so round-7's [euf_domain] [Arith] gate is gone: there is no
     merely-foreign-registered sum in the domain to gate out, and the fresh-sum flooding
     it guarded against cannot occur (the internal sum [p_i - p_j] a trichotomy mints is
     arith-under-arith, never a boundary node).

     Both-valued skip (ADR §3.3, grow-only soundness): the interface set is grow-only but
     the children truncate e-nodes on [pop], so a stale member may outlive its child
     backing. Comparing only members BOTH models currently value drops any such stale
     member — it appears in no live atom ⇒ at least one child does not value it ⇒ skipped
     ⇒ never a spurious split. Int-only: LIA never values a non-Int term.

     No pin-skip: a positive pinned pair agrees (both models satisfy it); a negatively-
     pinned pair the arithmetic child was NOT told may still disagree — and MUST be split,
     so the ordering reaches that child (the S1 completeness path). *)
  let find_disagreement t ma mb =
    let candidate (term : Term.t) =
      match term.Term.sort with
      | Sort.Bool | Sort.Uninterpreted _ -> false
      | Sort.Int _ -> true
    in
    let valued =
      Term.Set.elements t.interface
      |> List.filter_map (fun (term : Term.t) ->
        if not (candidate term)
        then None
        else (
          match model_eval ma term, model_eval mb term with
          | Some va, Some vb -> Some (term, va, vb)
          | _ -> None))
    in
    let rec outer = function
      | [] -> None
      | first :: rest ->
        (match inner first rest with
         | Some pair -> Some pair
         | None -> outer rest)
    and inner ((x, ax, bx) as first) = function
      | [] -> None
      | (y, ay, by) :: rest ->
        if not (Bool.equal (value_equal ax ay) (value_equal bx by))
        then Some (x, y)
        else inner first rest
    in
    outer valued
  ;;

  (* H2 (codex): a Bool leaf / Bool-returning UF used as an argument of an uninterpreted
     function is sound only if EUF has BOUND it to [true]/[false] — which happens exactly
     when it surfaced as a SAT atom (a top-level literal asserted via K_bool). A BURIED
     such term stays a third opaque Boolean class ([Uninterp]) in EUF, so h(·) congruence
     could wrong-SAT ([h(b)≠h(true) ∧ h(b)≠h(false)], likewise buried [h(g x)]). Checked
     at the Sat certification point ONLY (after the Int arrangement agrees): if a member
     is opaque, the model cannot be soundly certified for that h(·) argument → degrade to
     [unknown] (team-lead ruling: buried-unbound → {!Incomplete}). A SURFACED leaf is
     bound, so it stays decidable (the ADR §3.6 case-(i) UNSAT survives). *)
  let require_bool_args_bound t ma =
    Term.Set.iter
      (fun term ->
         match model_eval ma term with
         | Some (Model.Bool _) -> ()
         | _ ->
           raise
             (Incomplete
                "Bool leaf / predicate under an uninterpreted function is unbound \
                 (buried, no true/false binding in EUF)"))
      t.bool_uf_args
  ;;

  (* Both children have just certified [Final]→[Sat] (codex C4): consume their models. *)
  let combine_models t : Theory.check_result =
    let ma = A.model t.a in
    let mb = B.model t.b in
    check_pins t ma mb;
    match find_disagreement t ma mb with
    | Some (x, y) -> Theory.Split (R.equality_split t.ctx x y)
    | None ->
      (* Int arrangement agrees; about to certify Sat — now require every buried Bool UF
         argument to be bound (else a wrong-SAT would leak, codex H2). *)
      require_bool_args_bound t ma;
      Theory.Sat
  ;;

  let check_b_propagate t la : Theory.check_result =
    match B.check t.b Theory.Propagate with
    | Theory.Conflict e -> Theory.Conflict e
    | Theory.Sat | Theory.Split _ -> Theory.Propagations la
    | Theory.Propagations lb ->
      record_props t R.B lb;
      Theory.Propagations (la @ lb)
  ;;

  let check t effort : Theory.check_result =
    match effort with
    | Theory.Propagate ->
      (match A.check t.a Theory.Propagate with
       | Theory.Conflict e -> Theory.Conflict e
       | Theory.Sat | Theory.Split _ ->
         (* illegal at Propagate per the THEORY contract; treat as no propagations *)
         check_b_propagate t []
       | Theory.Propagations la ->
         record_props t R.A la;
         check_b_propagate t la)
    | Theory.Final ->
      (* codex C4: only [Sat] from BOTH children is a certificate; empty propagations are
         not. Anything that is not a genuine [Sat]/[Sat] pair is either progress (forward
         a conflict/split/non-empty propagation) or — a full model with no conflict, no
         split, no propagation, yet a child that did not certify [Sat] — a
         completeness-contract violation we refuse to launder into [Sat] (degrade to
         unknown). *)
      (match A.check t.a Theory.Final with
       | Theory.Conflict e -> Theory.Conflict e
       | Theory.Split terms -> Theory.Split terms
       | Theory.Propagations (_ :: _ as la) ->
         record_props t R.A la;
         Theory.Propagations la
       | (Theory.Sat | Theory.Propagations []) as ra ->
         (match B.check t.b Theory.Final with
          | Theory.Conflict e -> Theory.Conflict e
          | Theory.Split terms -> Theory.Split terms
          | Theory.Propagations (_ :: _ as lb) ->
            record_props t R.B lb;
            Theory.Propagations lb
          | (Theory.Sat | Theory.Propagations []) as rb ->
            (match ra, rb with
             | Theory.Sat, Theory.Sat -> combine_models t
             | _ ->
               raise
                 (Combination_unsound
                    "child returned a non-Sat consistent result at Final (empty \
                     propagations is not a Sat certificate)"))))
  ;;

  let explain t lit =
    match Lit.Map.find_opt lit t.propagated_by with
    | Some R.A -> A.explain t.a lit
    | Some R.B | Some R.Both -> B.explain t.b lit
    | None ->
      (* Not recorded as propagated: route by the literal's assert-time owner. *)
      (match R.assert_to (term_of t (Lit.atom lit)) ~positive:(Lit.sign lit) with
       | R.B -> B.explain t.b lit
       | R.A | R.Both -> A.explain t.a lit)
  ;;

  let push t =
    A.push t.a;
    B.push t.b;
    t.pin_frames <- [] :: t.pin_frames
  ;;

  let pop t n =
    A.pop t.a n;
    B.pop t.b n;
    let rec drop k frames =
      if k <= 0
      then frames
      else (
        match frames with
        | _ :: rest -> drop (k - 1) rest
        | [] -> [])
    in
    t.pin_frames
    <- (match drop n t.pin_frames with
        | [] -> [ [] ]
        | frames -> frames)
  ;;

  (* codex C3 (round-2 refinement) — sort-directed merge over ALL subterms, NEVER raising
     (raising here gutted normal QF_UFLIA: a pure-EUF Int term like [f x] appearing only
     under a predicate is unseen by LIA and valued as an opaque class by EUF — the
     DEFINING shape, not an error). For each term:
     - Int: take an Int-variant value from either child, folding [Arith] compounds over
       the arithmetic child's leaves ([x + 1] from [x]); if none is Int-variant (a
       pure-EUF Int term) OMIT it — CONTRACT-MODEL permits absence, and the §8 evaluator
       reads such a term's value from the {e containing} EUF term it needs, never from the
       bare arg.
     - Uninterpreted → the [Uninterp] class; Bool → the [Bool] value; else omit. Sound: an
       Int-sorted term never gets a non-Int (class) value, and an omitted term is one no
       child constrains numerically. *)
  let model t =
    let ma = A.model t.a in
    let mb = B.model t.b in
    (* §10 v2 gap A (task #117): class value inheritance. A pure-EUF Int class that shares
       its EUF class with a LIA-valued term must INHERIT that term's integer rather than
       let {!Cdclt} mint a fresh one — else congruent App twins (e.g. [f a] LIA-valued,
       [f b] pure-EUF under [a = b]) land two disagreeing rows at one table key and R1
       rejects. [class_int] maps an EUF class id to the LIA integer some member of that
       class carries: for each Int-sorted term with BOTH an EUF class id (from [ma], which
       classes every Int App) AND a LIA integer (from [mb] or an [Arith] fold), record
       [cid -> n]. In a combination-certified Sat a class has at most one LIA value (two
       congruent Int terms LIA-valued differently is exactly the disagreement
       [find_disagreement] splits on before Sat); defensively the reducer is [min], so the
       map is order-independent and any residual inconsistency is still caught by R1 (->
       [unknown], never wrong-sat). *)
    let class_int : (int, int) Hashtbl.t = Hashtbl.create 64 in
    let lia_int term =
      match model_eval mb term with
      | Some (Model.Int n) -> Some n
      | _ ->
        (match model_eval ma term with
         | Some (Model.Int n) -> Some n
         | _ -> None)
    in
    Term.Set.iter
      (fun (term : Term.t) ->
         match term.Term.sort with
         | Sort.Int _ ->
           (match lia_int term, model_eval ma term with
            | Some n, Some (Model.Uninterp cid) ->
              (match Hashtbl.find_opt class_int cid with
               | Some m when m <= n -> ()
               | _ -> Hashtbl.replace class_int cid n)
            | _ -> ())
         | Sort.Bool | Sort.Uninterpreted _ -> ())
      t.all_terms;
    let int_variant term =
      match model_eval mb term, model_eval ma term with
      | Some (Model.Int _ as v), _ | _, Some (Model.Int _ as v) -> Some v
      | _ ->
        (* §10 ℤ-realization seed (task #110): an Int-sorted term no child constrains
           numerically — a pure-EUF Int class (never surfaced into a LIA atom, so LIA does
           not value it; EUF values it only as an opaque congruence class) — carries its
           EUF class id so extraction ({!Cdclt.model}) can realize a concrete integer for
           it. Before this, such a term was OMITTED from the merged model, which forced
           the Cdclt table builder to degrade any QF_UFLIA sat whose tables touch such a
           class to [unknown]. Surfacing the class does NOT change the combination
           decision: [find_disagreement]/[check_pins] read the child models [ma]/[mb]
           directly, and {!Cdclt} is the sole consumer of this merged model. The Int term
           still never gets a non-Int VALUE — [Uninterp] here is the extraction-layer
           signal "realize me", read only by the Int-sorted arm of Cdclt's [value_of].
           Reuses the existing [Model.Uninterp] constructor; no [Model.t] / frozen-surface
           change. Gap A (task #117): if this class inherits a LIA integer ([class_int]),
           surface THAT integer ([Model.Int]) instead of the realize-me signal, so every
           term of the class shares the LIA value. *)
        (match model_eval ma term with
         | Some (Model.Uninterp cid as v) ->
           (match Hashtbl.find_opt class_int cid with
            | Some n -> Some (Model.Int n)
            | None -> Some v)
         | _ -> None)
    in
    let variant term matches =
      match model_eval ma term, model_eval mb term with
      | Some v, _ when matches v -> Some v
      | _, Some v when matches v -> Some v
      | _ -> None
    in
    let bindings =
      Term.Set.elements t.all_terms
      |> List.filter_map (fun (term : Term.t) ->
        let value =
          match term.Term.sort with
          | Sort.Int _ -> int_variant term
          | Sort.Bool ->
            variant term (function
              | Model.Bool _ -> true
              | _ -> false)
          | Sort.Uninterpreted _ ->
            variant term (function
              | Model.Uninterp _ -> true
              | _ -> false)
        in
        Option.map (fun v -> term, v) value)
    in
    Model.of_alist bindings
  ;;

  type congruence_state = A.t

  let congruence_state t = t.a
end

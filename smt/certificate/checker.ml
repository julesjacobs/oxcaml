(* Certificate replay checker (ADR-0013 step 2). See checker.mli. Stdlib-only over the
   recorder, frozen Sat lit algebra, immutable core terms, and exact LIA rationals. *)

module Sat = Oxsmt_solver.Sat
module Bigint = Oxsmt_core.Bigint
module Iarr = Oxsmt_core.Iarr
module Term = Oxsmt_core.Term
module Theory_view = Oxsmt_core.Theory_view
module Symbol = Oxsmt_core.Symbol
module Sort = Oxsmt_core.Sort
module Datatype_defs = Oxsmt_core.Datatype_defs
module Rational = Oxsmt_lia.Rational

type verdict =
  | Valid_modulo_theory_leaves
  | Valid
  | Invalid of string
  | Unsupported of string

type events =
  { inputs : Recorder.input_event list
  ; atoms : Recorder.atom_event list
  ; units : Recorder.unit_event list
  ; learned : Recorder.learned_event list
  ; theory : Recorder.theory_event list
  ; conclusion : Sat.unsat_conclusion option
  ; assumptions : Sat.lit list
  }

let of_recorder r ~assumptions =
  { inputs = Recorder.inputs r
  ; atoms = Recorder.atoms r
  ; units = Recorder.units r
  ; learned = Recorder.learned r
  ; theory = Recorder.theory_clauses r
  ; conclusion = Recorder.conclusion r
  ; assumptions
  }
;;

let string_of_verdict = function
  | Valid_modulo_theory_leaves -> "VALID(modulo theory leaves)"
  | Valid -> "VALID"
  | Invalid reason -> "INVALID(" ^ reason ^ ")"
  | Unsupported feature -> "UNSUPPORTED(" ^ feature ^ ")"
;;

(* OBSERVABILITY (task #56): count how often a learned clause was accepted via the
   full-closure RUP fallback rather than its hinted ordered chain. Chain quality is no
   longer a validity criterion (the cited witness is advisory; soundness = closure
   entailment), but it stays a MONITORED metric: a jump in this count flags a degraded /
   drifting emitter without failing soundness. Cumulative across [check] calls; callers
   reset as they wish (the corpus gate resets per run and prints the total; the self-test
   resets per case to assert the fallback fired). *)
let fallback_firings = ref 0
let reset_fallback_firings () = fallback_firings := 0
let fallback_firing_count () = !fallback_firings

(* ------------------------------------------------------------------ *)
(* Kind-keyed citation resolution (board #153a). Each content event registers its clause
   under its id AND its KIND. A citation site demands a specific kind set; a wrong-kind
   hit (the [Root_empty]-cites-a-learned-id false-clean codex found) is rejected exactly
   like a dangling id. Ambiguity (one id, two content clauses — the cross-solver misuse
   HIGH-4) fails closed too: the recorder cannot bind to a solver identity, so a repeated
   id is unresolvable, never silently collapsed. *)

type kind =
  | Kinput of Sat.origin
  | Klearned
  | Ktheory of Sat.theory_clause_role

let kind_name = function
  | Kinput Sat.Query -> "input(query)"
  | Kinput Sat.Theory_lemma -> "input(theory-lemma)"
  | Klearned -> "learned"
  | Ktheory Sat.Reason -> "theory-reason"
  | Ktheory Sat.Conflict -> "theory-conflict"
;;

type resolution =
  | Found of kind * Sat.lit array
  | Dangling
  | Ambiguous

(* Duplicate raw literals defeat unit detection (a [a;a] clause looks 2-free), which would
   OVER-reject a valid cert (codex M5). Normalize every clause to a duplicate-free literal
   set at ingest; falsification/RUP/BCP all consume the normalized form. *)
let dedup_clause (c : Sat.lit array) : Sat.lit array =
  Array.of_list (List.sort_uniq compare (Array.to_list c))
;;

(* id -> (kind, clause), plus the list of AMBIGUOUS content ids (an id emitted by two
   distinct content events, any kinds — the cross-solver misuse). Ambiguity is rejected at
   STREAM ADMISSION, not only when cited (codex H4): otherwise a spurious clause under a
   duplicate id is silently admitted to the axiom DB and poisons BCP even though its id is
   never cited. Stored clauses are dedup-normalized (M5). *)
let build_index ev =
  let by_id = Hashtbl.create 256 in
  let ambiguous = Hashtbl.create 16 in
  let add id kc =
    if Hashtbl.mem ambiguous id
    then ()
    else if Hashtbl.mem by_id id
    then (
      Hashtbl.remove by_id id;
      Hashtbl.replace ambiguous id ())
    else Hashtbl.replace by_id id kc
  in
  List.iter
    (fun (e : Recorder.input_event) ->
      add e.Recorder.id (Kinput e.Recorder.origin, dedup_clause e.Recorder.clause))
    ev.inputs;
  List.iter
    (fun (e : Recorder.learned_event) ->
      add e.Recorder.id (Klearned, dedup_clause e.Recorder.clause))
    ev.learned;
  List.iter
    (fun (e : Recorder.theory_event) ->
      add e.Recorder.id (Ktheory e.Recorder.role, dedup_clause e.Recorder.clause))
    ev.theory;
  let resolve id =
    if Hashtbl.mem ambiguous id
    then Ambiguous
    else (
      match Hashtbl.find_opt by_id id with
      | Some (k, c) -> Found (k, c)
      | None -> Dangling)
  in
  let ambiguous_ids = Hashtbl.fold (fun id () acc -> id :: acc) ambiguous [] in
  resolve, ambiguous_ids
;;

(* ------------------------------------------------------------------ *)
(* Partial assignment: var -> bool (absent = unassigned). *)

type assign = (Sat.var, bool) Hashtbl.t

type lit_status =
  | LTrue
  | LFalse
  | LUnassigned

let lit_status (a : assign) l =
  match Hashtbl.find_opt a (Sat.var_of_lit l) with
  | None -> LUnassigned
  | Some b -> if b = Sat.sign_of_lit l then LTrue else LFalse
;;

(* Make [l] true. [`Conflict] if its var is already fixed the other way. *)
let set_true (a : assign) l =
  let v = Sat.var_of_lit l
  and want = Sat.sign_of_lit l in
  match Hashtbl.find_opt a v with
  | Some b -> if b = want then `Ok else `Conflict
  | None ->
    Hashtbl.replace a v want;
    `Ok
;;

let falsified (a : assign) clause =
  Array.for_all (fun l -> lit_status a l = LFalse) clause
;;

(* ------------------------------------------------------------------ *)
(* An incremental unit-propagation engine over a growing clause database (§1.3). The
   database is built from the AXIOM clauses (query/lemma inputs + theory leaves) and then
   the LEARNED clauses one at a time, each folded in only AFTER it has replayed by ordered
   RUP — so the closure a learned clause is checked against never assumes a
   not-yet-verified (or later) learned clause: no circularity. Every clause in the DB is
   either a valid axiom (a theory leaf's witness is a later tranche) or a verified learned
   clause, so a literal forced into the closure is genuinely entailed and a clause
   falsified by it is genuinely refuted. Naive fixpoint; a session's guarded clauses yield
   no level-0 unit and settle in one pass. *)
module Bcp = struct
  type t =
    { assign : assign
    ; mutable db : Sat.lit array list
    }

  let create () = { assign = Hashtbl.create 256; db = [] }
  let snapshot t : assign = Hashtbl.copy t.assign

  (* Propagate [db] to fixpoint into [a]; [true] if a level-0 conflict is derived (the
     assignment then stands as-is, consistent up to the conflicting clause). *)
  let propagate_into (a : assign) db =
    let changed = ref true
    and conflict = ref false in
    while !changed && not !conflict do
      changed := false;
      List.iter
        (fun clause ->
          if not !conflict
          then (
            let satisfied = ref false
            and unassigned = ref [] in
            Array.iter
              (fun l ->
                match lit_status a l with
                | LTrue -> satisfied := true
                | LFalse -> ()
                | LUnassigned -> unassigned := l :: !unassigned)
              clause;
            if not !satisfied
            then (
              match !unassigned with
              | [] -> conflict := true
              | [ l ] ->
                (match set_true a l with
                 | `Ok -> changed := true
                 | `Conflict -> conflict := true)
              | _ -> ())))
        db
    done;
    !conflict
  ;;

  let add_axioms t clauses = t.db <- clauses @ t.db
  let propagate t = ignore (propagate_into t.assign t.db : bool)

  (* Fold in one verified learned clause. Only a clause that is UNIT (or already
     falsified) under the current closure can extend it — a satisfied or >=2-free clause
     adds no level-0 fact — so the common case is O(width) and a full re-propagation runs
     only when a learned unit actually fires (rare). This keeps the incremental closure
     linear in practice rather than O(learned × clauses). *)
  let add_learned t clause =
    t.db <- clause :: t.db;
    let satisfied = ref false
    and unassigned = ref [] in
    Array.iter
      (fun l ->
        match lit_status t.assign l with
        | LTrue -> satisfied := true
        | LFalse -> ()
        | LUnassigned -> unassigned := l :: !unassigned)
      clause;
    if not !satisfied
    then (
      match !unassigned with
      | [ l ] ->
        (match set_true t.assign l with
         | `Ok -> ignore (propagate_into t.assign t.db : bool) (* cascade the new unit *)
         | `Conflict -> ())
      | _ -> () (* [] conflict, or >=2 free: no new level-0 fact to propagate *))
  ;;

  (* [true] iff seeding [lits] true into a copy of the current closure and propagating
     over the whole DB derives a conflict — the level-0 RUP of the assumption-forcing
     against the verified clause DB (the OCaml-side selector strip; §4.0 E3). *)
  let refutes_under t lits =
    let a = snapshot t in
    let seeded_conflict = List.exists (fun l -> set_true a l = `Conflict) lits in
    seeded_conflict || propagate_into a t.db
  ;;
end

(* ------------------------------------------------------------------ *)
(* Ordered, hint-restricted RUP (§1.4) for a learned clause. [base] is the closure so far
   (axioms + earlier verified learned clauses); the clause negates its own literals. Each
   antecedent, IN ORDER, must be unit (propagate its one free literal), falsified
   (conflict — success), or ALREADY SATISFIED (a no-op — SKIP it and continue). A cited
   clause with >=2 free literals breaks the chain: reject, never search.

   SATISFIED-HINT SKIP (fix task #42): a hint whose clause is already satisfied under the
   accumulated assignment forces nothing — its lone would-be unit role is void because a
   satisfied clause propagates no literal. Skipping it removes NO inference and is exactly
   equivalent to the emitter having omitted that antecedent from the chain; the derivation
   the remaining hints produce is unchanged. This accepts NON-MINIMAL (but still ordered)
   antecedent chains, which the LIA-heavy emitter produces: theory-propagated literals
   carry lazy explain reasons that overlap the Boolean resolution chain, so analyze
   records an antecedent whose unit literal an earlier antecedent already delivered. This
   is the standard drat-trim-style treatment of satisfied antecedents (no-ops), and it
   does NOT relax soundness: the skip fires ONLY on a hint that [validate_id] has already
   resolved to a real, already-verified content clause (never a forged / dangling /
   unverified / ambiguous id), and the "refuses to search" contract is untouched for the
   unit / >=2-free / falsified cases. A chain that ends without a conflict (e.g. every
   hint merely satisfied) still fails with "RUP chain consumed without deriving a
   conflict". Emitting minimal reverse-propagation-ordered chains (fix shape (b),
   emitter-side) remains the faithful long-term option; see ADR-0013 appendix.

   [learned_verified id] gates a [Klearned] hint: it resolves ONLY if that learned clause
   has ALREADY been verified (a lower emission index). This enforces the LRAT
   id-monotonicity the emitter is documented to preserve rather than trusting it — a
   learned clause may not cite ITSELF or a LATER (still-unverified, possibly circular)
   learned clause, which would let two self- or mutually-referential learned clauses
   "verify" out of nothing and certify a satisfiable query as unsat (reviewer CRIT-1, the
   accept-invalid north star). Inputs / theory leaves have no such ordering and resolve
   per their own rules. *)
let ordered_rup base ~clause ~antecedents ~resolve ~learned_verified =
  (* Every antecedent id must resolve to a content clause and, if learned, be ALREADY
     verified — validated over the FULL list up front (codex H3), so a forged / dangling /
     unverified id in the TAIL is caught even when propagation reaches a conflict early
     and would otherwise stop before consuming it. *)
  let validate_id id =
    match (resolve id : resolution) with
    | Ambiguous ->
      Error (Printf.sprintf "antecedent id %d is ambiguous (two clauses share it)" id)
    | Dangling ->
      Error (Printf.sprintf "antecedent id %d resolves to no content clause" id)
    | Found (Klearned, _) when not (learned_verified id) ->
      Error
        (Printf.sprintf
           "antecedent id %d cites a learned clause that is not yet verified (a self- or \
            forward/circular citation)"
           id)
    | Found _ -> Ok ()
  in
  match
    List.find_map
      (fun id ->
        match validate_id id with
        | Error e -> Some e
        | Ok () -> None)
      antecedents
  with
  | Some e -> Error e
  | None ->
    let a : assign = Hashtbl.copy base in
    let conflict = ref false in
    List.iter
      (fun l ->
        if (not !conflict) && set_true a (Sat.neg_lit l) = `Conflict then conflict := true)
      (Array.to_list clause);
    if !conflict
    then Ok () (* negating the clause already contradicts the closure: it is entailed *)
    else (
      let rec go = function
        | [] -> Error "RUP chain consumed without deriving a conflict"
        | id :: rest ->
          (* ids are pre-validated above; [resolve] here only fetches the clause. *)
          (match (resolve id : resolution) with
           | Ambiguous | Dangling ->
             Error (Printf.sprintf "antecedent id %d unresolved (unreachable)" id)
           | Found (_kind, hint) ->
             let satisfied = ref false
             and unassigned = ref [] in
             Array.iter
               (fun l ->
                 match lit_status a l with
                 | LTrue -> satisfied := true
                 | LFalse -> ()
                 | LUnassigned -> unassigned := l :: !unassigned)
               hint;
             if !satisfied
             then
               (* Satisfied hint: forces nothing, skip it (fix task #42). Equivalent to
                  the emitter having omitted this antecedent; removes no inference. If the
                  chain ends here without a conflict, [go []] still fails below. *)
               go rest
             else (
               match !unassigned with
               | [] -> Ok () (* falsified: conflict reached *)
               | [ l ] ->
                 (match set_true a l with
                  | `Conflict -> Ok () (* propagation conflicts: conflict reached *)
                  | `Ok -> go rest)
               | more ->
                 Error
                   (Printf.sprintf
                      "hint %d is not unit (%d free literals) — hint-restricted RUP \
                       refuses to search"
                      id
                      (List.length more))))
      in
      go antecedents)
;;

(* ------------------------------------------------------------------ *)

exception Reject of verdict

let rejectf fmt = Printf.ksprintf (fun s -> raise (Reject (Invalid s))) fmt
let unsupportedf fmt = Printf.ksprintf (fun s -> raise (Reject (Unsupported s))) fmt

(* Fail-closed guard for the empty-clause fabrications of ⊥. A raw-empty clause admitted
   to the axiom DB is a trusted [] that refutes ANYTHING, so each origin that cannot
   legitimately carry an empty clause is rejected here — at ingest, before the clause
   enters the closure.

   The one origin that CAN carry a raw empty clause is a [Kinput Sat.Query]: asserting the
   empty clause is a legitimately-unsat E1 query, so it falls through to [()] and is
   trusted.

   MARKED EXTENSION POINT (do NOT implement here) for the ADR-0014 Rev-4 fabric-edge /
   [Shared_eq] leaf (a virtual proposition for s=t with assumption discharge): such a leaf
   is NOT representable in today's frozen Sat trace (theory roles are exactly
   [{Reason;Conflict}]), so the checker cannot see one yet — but when the cert format
   grows that leaf kind, it must route HERE and be rejected fail-closed, landing as its
   own reviewed tranche. *)
let guard_theory_leaf kind clause =
  match kind with
  | Kinput Sat.Theory_lemma when Array.length clause = 0 ->
    (* codex (this round): a raw-empty Theory_lemma INPUT is a fabricated ⊥ with no
       Valid_lemma witness in ANY theory. ADR-0013 §4.0 E4 admits only a NONEMPTY lemma
       that FILTERS to [] under the level-0 closure (the [falsified] check), never a
       clause that arrives empty. Contrast [Kinput Sat.Query], where an empty input is the
       legitimate E1 opposite (assert-false = unsat) and is trusted below. *)
    rejectf
      "empty Theory_lemma input clause — a theory lemma has no Valid_lemma witness for ⊥ \
       from the empty premise set (ADR-0013 §4.0 E4 admits only a NONEMPTY lemma that \
       filters to [] under the level-0 closure)"
  | Ktheory Sat.Reason when Array.length clause = 0 ->
    (* codex C2: an empty Reason clause admitted to the axiom DB is a fabricated ⊥ that
       refutes anything. A Reason is the propagation clause [p ∨ ¬p₁ ∨ … ∨ ¬pₖ] with the
       implied literal at slot 0 (sat.mli), so an empty one is MALFORMED — Invalid. *)
    rejectf
      "empty theory Reason clause — malformed (a Reason must carry its implied literal \
       at slot 0)"
  | Ktheory Sat.Conflict when Array.length clause = 0 ->
    unsupportedf
      "empty theory Conflict clause (unconditional T_conflict []) — no v1 leaf witnesses \
       false from the empty premise set (ADR-0013 Rev 6)"
  | _ -> ()
;;

type euf_endpoint =
  | Euf_term of Term.t
  | Euf_true
  | Euf_false

type euf_statement =
  | Euf_eq of euf_endpoint * euf_endpoint
  | Euf_neq of euf_endpoint * euf_endpoint

(* Independent EUF leaf replay. A clause is EUF-valid exactly when the conjunction of its
   negated literals is inconsistent. Decode those signed propositions through the
   certificate's separate atom map, seed only their asserted equalities/disequalities, and
   rebuild congruence closure from the definition: reflexive union-find gives
   symmetry/transitivity, and two [App] terms merge only when their symbols, arities, and
   corresponding argument classes match. This calls no production EUF code or proof
   forest. Boolean predicates are applications equated with true/false; true != false is
   the sole background axiom. *)
let verify_euf_leaf
  ~resolve_atom
  (event : Recorder.theory_event)
  (witness : Recorder.euf_leaf_witness)
  =
  try
    if witness.Recorder.clause <> Array.to_list event.Recorder.clause
    then Error "EUF witness clause does not exactly match the emitted theory clause"
    else (
      let endpoint_of_bool b = if b then Euf_true else Euf_false in
      let decode asserted_lit =
        match resolve_atom (Sat.var_of_lit asserted_lit) with
        | None -> Error "EUF leaf literal has no theory-atom declaration"
        | Some atom ->
          if not (Theory_view.is_atom atom)
          then Error "EUF leaf declaration is not a theory atom"
          else (
            let positive = Sat.sign_of_lit asserted_lit in
            match Theory_view.atom atom with
            | Theory_view.Equality (a, b) ->
              if positive
              then Ok (Euf_eq (Euf_term a, Euf_term b))
              else Ok (Euf_neq (Euf_term a, Euf_term b))
            | Theory_view.Predicate _ ->
              Ok (Euf_eq (Euf_term atom, if positive then Euf_true else Euf_false))
            | Theory_view.Bool_lit value ->
              Ok
                (Euf_eq (endpoint_of_bool value, if positive then Euf_true else Euf_false))
            | Theory_view.Le_zero _ -> Error "EUF witness cites a non-EUF arithmetic atom")
      in
      let rec decode_all acc = function
        | [] -> Ok (List.rev acc)
        | clause_lit :: rest ->
          (match decode (Sat.neg_lit clause_lit) with
           | Error _ as error -> error
           | Ok statement -> decode_all (statement :: acc) rest)
      in
      match decode_all [] witness.Recorder.clause with
      | Error _ as error -> error
      | Ok statements ->
        (* Assign local node ids after a complete structural walk. Non-App constructors
           are opaque to congruence but their children are still registered, matching
           first-order term closure without interpreting arithmetic/connectives. *)
        let term_ids = Term.Table.create 64 in
        let terms_rev = ref [] in
        let next_id = ref 2 in
        let term_children (term : Term.t) =
          match term.Term.node with
          | Term.Bool_const _ | Term.Int_const _ | Term.Real_const _ -> []
          | Term.App (_, args) -> Iarr.to_list args
          | Term.Arith { coeffs; _ } -> List.map fst (Iarr.to_list coeffs)
          | Term.Real_arith { coeffs; _ } -> List.map fst (Iarr.to_list coeffs)
          | Term.Le child | Term.Not child -> [ child ]
          | Term.Eq (a, b) -> [ a; b ]
          | Term.And children | Term.Or children -> Iarr.to_list children
          | Term.Ite (c, a, b) -> [ c; a; b ]
        in
        let rec add_term (term : Term.t) =
          if not (Term.Table.mem term_ids term)
          then (
            match term.Term.node with
            | Term.Bool_const value ->
              (* The adapter's distinguished endpoints are the core Boolean constants
                 themselves. Preserve that identity when a constant also occurs as an
                 argument, e.g. [p] and [f(true)]. *)
              Term.Table.replace term_ids term (if value then 0 else 1)
            | _ ->
              List.iter add_term (term_children term);
              let id = !next_id in
              incr next_id;
              Term.Table.replace term_ids term id;
              terms_rev := term :: !terms_rev)
        in
        let add_endpoint = function
          | Euf_term term -> add_term term
          | Euf_true | Euf_false -> ()
        in
        List.iter
          (function
            | Euf_eq (a, b) | Euf_neq (a, b) ->
              add_endpoint a;
              add_endpoint b)
          statements;
        let id_of_endpoint = function
          | Euf_true -> 0
          | Euf_false -> 1
          | Euf_term term -> Term.Table.find term_ids term
        in
        let parent = Array.init !next_id (fun id -> id) in
        let rec root id =
          let p = parent.(id) in
          if p = id
          then id
          else (
            let r = root p in
            parent.(id) <- r;
            r)
        in
        let union a b =
          let ra = root a
          and rb = root b in
          if ra = rb
          then false
          else (
            parent.(rb) <- ra;
            true)
        in
        let disequalities = ref [ 0, 1 ] in
        List.iter
          (function
            | Euf_eq (a, b) -> ignore (union (id_of_endpoint a) (id_of_endpoint b) : bool)
            | Euf_neq (a, b) ->
              disequalities := (id_of_endpoint a, id_of_endpoint b) :: !disequalities)
          statements;
        let apps =
          List.filter_map
            (fun (term : Term.t) ->
              match term.Term.node with
              | Term.App (symbol, args) ->
                Some
                  ( Term.Table.find term_ids term
                  , symbol
                  , term.Term.sort
                  , Array.of_list
                      (List.map
                         (fun arg -> Term.Table.find term_ids arg)
                         (Iarr.to_list args))
                  , Array.of_list
                      (List.map (fun (arg : Term.t) -> arg.Term.sort) (Iarr.to_list args))
                  )
              | _ -> None)
            !terms_rev
        in
        let changed = ref true in
        while !changed do
          changed := false;
          List.iter
            (fun (id_a, symbol_a, result_sort_a, args_a, arg_sorts_a) ->
              List.iter
                (fun (id_b, symbol_b, result_sort_b, args_b, arg_sorts_b) ->
                  if id_a <> id_b
                     && Symbol.equal symbol_a symbol_b
                     && Sort.equal result_sort_a result_sort_b
                     && Array.length args_a = Array.length args_b
                     && Array.for_all2 Sort.equal arg_sorts_a arg_sorts_b
                     && Array.for_all2 (fun a b -> root a = root b) args_a args_b
                     && union id_a id_b
                  then changed := true)
                apps)
            apps
        done;
        if List.exists (fun (a, b) -> root a = root b) !disequalities
        then Ok ()
        else Error "negated EUF leaf remains congruence-consistent")
  with
  | exn -> Error ("EUF replay raised: " ^ Printexc.to_string exn)
;;

(* Independent LIA Conflict-leaf replay. This deliberately does not call the simplex's
   production self-check: it reconstructs each asserted integer half-plane from the
   recorded atom and checks the Farkas equation from the definition.

   For a positive [(e <= 0)] premise the row is [e <= 0]. For a negative premise, integer
   semantics gives [not (e <= 0)] iff [-e + 1 <= 0]. A valid witness has only nonnegative
   multipliers and sums these rows to [0 < c] (all variable coefficients zero, constant
   [c > 0]). *)
let verify_lia_conflict
  ~resolve_atom
  (event : Recorder.theory_event)
  (witness : Recorder.lia_conflict_witness)
  =
  try
    let rows = witness.Recorder.premises in
    if rows = []
    then Error "empty Farkas witness"
    else (
      let actual_clause = Array.to_list event.Recorder.clause |> List.sort_uniq compare in
      let witnessed_clause =
        List.map (fun (p : Recorder.lia_premise) -> Sat.neg_lit p.Recorder.lit) rows
        |> List.sort_uniq compare
      in
      if actual_clause <> witnessed_clause
      then Error "Farkas premises are not exactly the emitted conflict clause's negation"
      else (
        let coeffs = ref Term.Map.empty in
        let constant = ref Rational.zero in
        let add_coeff term value =
          let old =
            match Term.Map.find_opt term !coeffs with
            | Some value -> value
            | None -> Rational.zero
          in
          coeffs := Term.Map.add term (Rational.add old value) !coeffs
        in
        let bigint_neg value = Bigint.mul (Bigint.of_int (-1)) value in
        let linear_of (term : Term.t) =
          match term.Term.node with
          | Term.Arith { coeffs; const } -> Iarr.to_list coeffs, const
          | Term.Int_const const -> [], const
          | _ -> [ term, Bigint.one ], Bigint.zero
        in
        let accumulate ~mult ~vars ~const =
          List.iter
            (fun (var, coeff) ->
              add_coeff var (Rational.mul mult (Rational.of_bigint coeff)))
            vars;
          constant
          := Rational.add !constant (Rational.mul mult (Rational.of_bigint const))
        in
        let add_row (p : Recorder.lia_premise) =
          let polarity = Sat.sign_of_lit p.Recorder.lit in
          let mult = p.Recorder.multiplier in
          match resolve_atom (Sat.var_of_lit p.Recorder.lit) with
          | None -> Error "Farkas premise has no theory-atom declaration"
          | Some atom ->
            (match atom.Term.node with
             | Term.Le arg ->
               (* An inequality half-plane [e <= 0] requires a NON-NEGATIVE multiplier. *)
               if Rational.sign mult < 0
               then Error "negative Farkas multiplier on an inequality premise"
               else if polarity
               then (
                 let vars, const = linear_of arg in
                 accumulate ~mult ~vars ~const;
                 Ok ())
               else if not (Sort.equal arg.Term.sort Sort.int)
               then
                 (* [not (e <= 0)] strengthens to [-e + 1 <= 0] ONLY over the integers; a
                    Real [Le] has no [+1] step, so claiming it here would be unsound (the
                    LIA x LRA Farkas-witness collision guard, rider #134). Fail closed. *)
                 Error "negated <= premise over a non-integer sort (LRA Farkas guard)"
               else (
                 let vars, const = linear_of arg in
                 let vars = List.map (fun (var, coeff) -> var, bigint_neg coeff) vars in
                 let const = Bigint.add (bigint_neg const) Bigint.one in
                 accumulate ~mult ~vars ~const;
                 Ok ())
             | Term.Eq (a, b) ->
               (* An equality [a = b] (i.e. [a - b = 0]) admits an ANY-SIGN multiplier —
                  the standard Farkas treatment of equalities. Post-LAND-29b the eq-aware
                  emitter records such equality premises (with signed / fractional
                  multipliers) in the conflict; the [a - b = 0] row contributes [= 0], so
                  it needs no sign constraint and does not perturb the [<= 0] direction
                  the inequality rows establish. A negative-polarity [Eq] is a
                  DISEQUALITY, which is not a linear half-plane and cannot appear in a
                  Farkas sum. *)
               if not polarity
               then Error "disequality premise is not a Farkas half-plane"
               else (
                 let va, ca = linear_of a in
                 let vb, cb = linear_of b in
                 accumulate ~mult ~vars:va ~const:ca;
                 accumulate ~mult:(Rational.neg mult) ~vars:vb ~const:cb;
                 Ok ())
             | _ -> Error "Farkas premise is not an integer <= or = atom")
        in
        match
          List.find_map
            (fun row ->
              match add_row row with
              | Ok () -> None
              | Error reason -> Some reason)
            rows
        with
        | Some reason -> Error reason
        | None ->
          let variables_cancel =
            Term.Map.for_all (fun _ coeff -> Rational.is_zero coeff) !coeffs
          in
          if not variables_cancel
          then Error "Farkas combination does not cancel every variable"
          else if Rational.sign !constant <= 0
          then Error "Farkas combination does not leave a strictly positive constant"
          else Ok ()))
  with
  | exn -> Error ("Farkas replay raised: " ^ Printexc.to_string exn)
;;

(* Independent datatype constructor-distinctness replay. The negation of the leaf clause
   must consist solely of positive equality atoms. Rebuild equality + congruence closure
   from those statements, then require it to merge the witness pair. The separate datatype
   registry must identify the pair as two different constructors of the SAME datatype,
   with applications whose argument/result sorts match their declarations. This
   deliberately calls no DT or EUF production code: constructor distinctness is re-derived
   from the datatype declaration and congruence from its definition. *)
let verify_dt_distinctness
  ~resolve_atom
  (event : Recorder.theory_event)
  (registry : Datatype_defs.t)
  (witness : Recorder.dt_distinctness_witness)
  =
  try
    if event.Recorder.role <> Sat.Conflict
    then Error "datatype distinctness witness is attached to a Reason leaf"
    else if witness.Recorder.clause <> Array.to_list event.Recorder.clause
    then Error "datatype witness clause does not exactly match the emitted theory clause"
    else (
      let rec decode_equalities acc = function
        | [] -> Ok (List.rev acc)
        | clause_lit :: rest ->
          let premise = Sat.neg_lit clause_lit in
          if not (Sat.sign_of_lit premise)
          then Error "datatype distinctness premise is not a positive equality"
          else (
            match resolve_atom (Sat.var_of_lit premise) with
            | None -> Error "datatype leaf literal has no theory-atom declaration"
            | Some atom ->
              if not (Theory_view.is_atom atom)
              then Error "datatype leaf declaration is not a theory atom"
              else (
                match Theory_view.atom atom with
                | Theory_view.Equality (left, right) ->
                  decode_equalities ((left, right) :: acc) rest
                | Theory_view.Predicate _ | Theory_view.Bool_lit _ | Theory_view.Le_zero _
                  -> Error "datatype distinctness premise is not an equality atom"))
      in
      match decode_equalities [] witness.Recorder.clause with
      | Error _ as error -> error
      | Ok equalities ->
        let term_ids = Term.Table.create 64 in
        let terms_rev = ref [] in
        let next_id = ref 0 in
        let children (term : Term.t) =
          match term.Term.node with
          | Term.Bool_const _ | Term.Int_const _ | Term.Real_const _ -> []
          | Term.App (_, args) -> Iarr.to_list args
          | Term.Arith { coeffs; _ } -> List.map fst (Iarr.to_list coeffs)
          | Term.Real_arith { coeffs; _ } -> List.map fst (Iarr.to_list coeffs)
          | Term.Le child | Term.Not child -> [ child ]
          | Term.Eq (left, right) -> [ left; right ]
          | Term.And children | Term.Or children -> Iarr.to_list children
          | Term.Ite (cond, yes, no) -> [ cond; yes; no ]
        in
        let rec add_term (term : Term.t) =
          if not (Term.Table.mem term_ids term)
          then (
            List.iter add_term (children term);
            let id = !next_id in
            incr next_id;
            Term.Table.replace term_ids term id;
            terms_rev := term :: !terms_rev)
        in
        List.iter
          (fun (left, right) ->
            add_term left;
            add_term right)
          equalities;
        let left = witness.Recorder.left
        and right = witness.Recorder.right in
        if not (Term.Table.mem term_ids left && Term.Table.mem term_ids right)
        then Error "datatype witness constructor is absent from the premise statements"
        else (
          let constructor_application (term : Term.t) =
            match term.Term.node with
            | Term.App (symbol, args) ->
              (match Datatype_defs.constructor_of_sym registry symbol with
               | None -> Error "datatype witness term is not a declared constructor"
               | Some (datatype, constructor) ->
                 let args = Iarr.to_list args in
                 if not
                      (Sort.equal
                         term.Term.sort
                         (Sort.datatype_ datatype.Datatype_defs.sort_sym))
                 then
                   Error "datatype constructor result sort disagrees with its declaration"
                 else if List.length args <> List.length constructor.selectors
                 then Error "datatype constructor arity disagrees with its declaration"
                 else if not
                           (List.for_all2
                              (fun (arg : Term.t) (selector : Datatype_defs.selector) ->
                                Sort.equal arg.Term.sort selector.field_sort)
                              args
                              constructor.selectors)
                 then
                   Error
                     "datatype constructor argument sort disagrees with its declaration"
                 else Ok (symbol, datatype))
            | _ -> Error "datatype witness endpoint is not a constructor application"
          in
          match constructor_application left, constructor_application right with
          | Error reason, _ | _, Error reason -> Error reason
          | Ok (left_symbol, left_datatype), Ok (right_symbol, right_datatype) ->
            if Symbol.equal left_symbol right_symbol
            then Error "datatype witness names the same constructor twice"
            else if not
                      (Symbol.equal
                         left_datatype.Datatype_defs.sort_sym
                         right_datatype.Datatype_defs.sort_sym)
            then Error "datatype witness constructors belong to different datatypes"
            else (
              let parent = Array.init !next_id Fun.id in
              let rec root id =
                let p = parent.(id) in
                if p = id
                then id
                else (
                  let r = root p in
                  parent.(id) <- r;
                  r)
              in
              let union a b =
                let a = root a
                and b = root b in
                if a = b
                then false
                else (
                  parent.(b) <- a;
                  true)
              in
              List.iter
                (fun (a, b) ->
                  ignore
                    (union (Term.Table.find term_ids a) (Term.Table.find term_ids b)
                     : bool))
                equalities;
              let apps =
                List.filter_map
                  (fun (term : Term.t) ->
                    match term.Term.node with
                    | Term.App (symbol, args) ->
                      Some
                        ( Term.Table.find term_ids term
                        , symbol
                        , term.Term.sort
                        , Array.of_list
                            (List.map
                               (fun arg -> Term.Table.find term_ids arg)
                               (Iarr.to_list args))
                        , Array.of_list
                            (List.map
                               (fun (arg : Term.t) -> arg.Term.sort)
                               (Iarr.to_list args)) )
                    | _ -> None)
                  !terms_rev
              in
              let changed = ref true in
              while !changed do
                changed := false;
                List.iter
                  (fun (id_a, symbol_a, result_sort_a, args_a, arg_sorts_a) ->
                    List.iter
                      (fun (id_b, symbol_b, result_sort_b, args_b, arg_sorts_b) ->
                        if id_a <> id_b
                           && Symbol.equal symbol_a symbol_b
                           && Sort.equal result_sort_a result_sort_b
                           && Array.length args_a = Array.length args_b
                           && Array.for_all2 Sort.equal arg_sorts_a arg_sorts_b
                           && Array.for_all2 (fun a b -> root a = root b) args_a args_b
                           && union id_a id_b
                        then changed := true)
                      apps)
                  apps
              done;
              if root (Term.Table.find term_ids left)
                 = root (Term.Table.find term_ids right)
              then Ok ()
              else
                Error "datatype witness constructors are not congruent under the premises")))
  with
  | exn -> Error ("datatype distinctness replay raised: " ^ Printexc.to_string exn)
;;

let check ev =
  try
    let resolve, ambiguous_ids = build_index ev in
    (* Core term identity is tag-based within one Context, but tags restart in every
       Context. A malformed in-memory artifact could otherwise combine atom statements
       from two Contexts whose unrelated terms share tags; Term.Table/Map would alias them
       and an invalid EUF/Farkas witness could be accepted. Reject such collisions before
       any tag-keyed collection sees a term. Hash-consing makes equal same-Context terms
       physically identical, so one tag naming two different physical nodes is exactly the
       fail-closed collision to reject. EUF congruence also checks application sorts
       explicitly, covering mixed contexts whose selected tag ranges are disjoint. *)
    let term_by_tag = Hashtbl.create (4 * List.length ev.atoms) in
    let rec admit_term (term : Term.t) =
      match Hashtbl.find_opt term_by_tag term.Term.tag with
      | Some existing when existing != term ->
        rejectf
          "theory-atom statement mixes term contexts (tag %d names two different terms)"
          term.Term.tag
      | Some _ -> ()
      | None ->
        Hashtbl.replace term_by_tag term.Term.tag term;
        (match term.Term.node with
         | Term.Bool_const _ | Term.Int_const _ | Term.Real_const _ -> ()
         | Term.App (_, args) -> List.iter admit_term (Iarr.to_list args)
         | Term.Arith { coeffs; _ } ->
           List.iter (fun (child, _) -> admit_term child) (Iarr.to_list coeffs)
         | Term.Real_arith { coeffs; _ } ->
           List.iter (fun (child, _) -> admit_term child) (Iarr.to_list coeffs)
         | Term.Le child | Term.Not child -> admit_term child
         | Term.Eq (a, b) ->
           admit_term a;
           admit_term b
         | Term.And children | Term.Or children ->
           List.iter admit_term (Iarr.to_list children)
         | Term.Ite (c, a, b) ->
           admit_term c;
           admit_term a;
           admit_term b)
    in
    List.iter
      (fun (event : Recorder.atom_event) -> admit_term event.Recorder.atom)
      ev.atoms;
    (* Witness endpoints are untrusted artifact terms too. Admit them into the SAME
       physical-node/tag preflight before the local DT replay uses Term.Table: otherwise a
       constructor from another Context can reuse a statement subterm's numeric tag, pass
       [mem], and inherit that unrelated subterm's congruence class. *)
    List.iter
      (fun (event : Recorder.theory_event) ->
        match event.Recorder.dt_witness with
        | None -> ()
        | Some witness ->
          admit_term witness.Recorder.left;
          admit_term witness.Recorder.right)
      ev.theory;
    (* A query has one datatype declaration environment. Per-leaf copies are statement
       data, not proof data, but they must still agree globally: accepting different
       constructor classifications leaf-by-leaf would prove no single SMT problem. *)
    let dt_registries =
      List.filter_map
        (fun (event : Recorder.theory_event) -> event.Recorder.dt_registry)
        ev.theory
    in
    (match dt_registries with
     | [] -> ()
     | first :: rest ->
       if List.exists (fun registry -> registry <> first) rest
       then rejectf "datatype theory leaves carry inconsistent declaration registries");
    (* Theory-atom declarations are the certificate statement. A proof witness may cite
       them but may not redefine them leaf-locally. Duplicate declarations are rejected
       even when textually equal: exactly one internalization event must own each SAT
       variable, matching the driver's 1:1 atom invariant. *)
    let atom_by_var = Hashtbl.create (List.length ev.atoms) in
    List.iter
      (fun (e : Recorder.atom_event) ->
        if Hashtbl.mem atom_by_var e.Recorder.var
        then rejectf "duplicate theory-atom declaration for SAT var %d" e.Recorder.var;
        Hashtbl.replace atom_by_var e.Recorder.var e.Recorder.atom)
      ev.atoms;
    let resolve_atom var = Hashtbl.find_opt atom_by_var var in
    (* codex H4: reject ambiguity at STREAM ADMISSION — an id shared by two content
       clauses makes BOTH untrustworthy, and one would otherwise be admitted to the axiom
       DB and poison BCP even if its id is never cited. Fail closed before anything is
       trusted. *)
    (match ambiguous_ids with
     | [] -> ()
     | ids ->
       rejectf
         "ambiguous content id(s) [%s] — each is emitted by two distinct clauses (a \
          cross-solver stream); rejected at admission"
         (String.concat "; " (List.map string_of_int (List.sort compare ids))));
    (* a cited id must resolve to a content event of an ALLOWED kind. *)
    let resolve_as ~what ~allowed id =
      match (resolve id : resolution) with
      | Ambiguous -> rejectf "%s cites ambiguous id %d (two clauses share it)" what id
      | Dangling -> rejectf "%s cites id %d, which resolves to no content clause" what id
      | Found (kind, clause) ->
        if not (List.mem kind allowed)
        then
          rejectf
            "%s cites id %d of kind %s; expected one of [%s]"
            what
            id
            (kind_name kind)
            (String.concat "; " (List.map kind_name allowed));
        guard_theory_leaf kind clause;
        kind, clause
    in
    (* terminal conclusion must be present (a truncated stream drops it). *)
    let conclusion =
      match ev.conclusion with
      | Some c -> c
      | None -> rejectf "no terminal conclusion (truncated / non-terminating stream)"
    in
    (* an empty theory Conflict leaf anywhere is loud-uncertified (Rev 6 / fabric ext pt). *)
    List.iter
      (fun (e : Recorder.theory_event) ->
        guard_theory_leaf (Ktheory e.Recorder.role) e.Recorder.clause)
      ev.theory;
    (* codex (this round): guard every INPUT at ADMISSION too — a raw-empty Theory_lemma
       input is a fabricated ⊥ that certifies a SAT query unsat through ALL THREE
       terminals: two cite it (Root_empty / Level0_conflict) and Failed_assumption never
       cites it yet is refuted by BCP over the poisoned axiom DB. Guarding here — before
       [add_axioms] — covers the uncited-terminal case a citation-site guard alone would
       miss. An empty Query input falls through and stays the legitimate E1 unsat. *)
    List.iter
      (fun (e : Recorder.input_event) ->
        guard_theory_leaf (Kinput e.Recorder.origin) e.Recorder.clause)
      ev.inputs;
    (* Leaf coverage accounting. Every claimed EUF or Farkas witness is a hard proof
       obligation: corruption is [Invalid], never silently demoted to the trusted-leaf
       verdict. Unwitnessed Reason/Conflict leaves and Theory_lemma inputs retain the
       existing conditional verdict. Query inputs are formula axioms, not theory leaves. *)
    let has_unverified_theory_leaf = ref false in
    List.iter
      (fun (e : Recorder.input_event) ->
        if e.Recorder.origin = Sat.Theory_lemma then has_unverified_theory_leaf := true)
      ev.inputs;
    List.iter
      (fun (e : Recorder.theory_event) ->
        (match e.Recorder.euf_witness with
         | None -> ()
         | Some witness ->
           (match verify_euf_leaf ~resolve_atom e witness with
            | Ok () -> ()
            | Error reason ->
              rejectf "EUF theory leaf id %d has an invalid witness: %s" e.id reason));
        (match e.Recorder.role, e.Recorder.lia_witness with
         | Sat.Reason, Some _ ->
           rejectf
             "theory Reason clause id %d carries a Conflict-only Farkas witness"
             e.id
         | Sat.Conflict, Some witness ->
           (match verify_lia_conflict ~resolve_atom e witness with
            | Ok () -> ()
            | Error reason ->
              rejectf
                "LIA Conflict leaf id %d has an invalid Farkas witness: %s"
                e.id
                reason)
         | (Sat.Reason | Sat.Conflict), None -> ());
        (match e.Recorder.dt_witness, e.Recorder.dt_registry with
         | None, _ -> ()
         | Some _, None ->
           rejectf
             "datatype theory leaf id %d has a witness but no datatype declaration"
             e.id
         | Some witness, Some registry ->
           (match verify_dt_distinctness ~resolve_atom e registry witness with
            | Ok () -> ()
            | Error reason ->
              rejectf
                "datatype theory leaf id %d has an invalid distinctness witness: %s"
                e.id
                reason));
        if Option.is_none e.Recorder.euf_witness
           && Option.is_none e.Recorder.lia_witness
           && Option.is_none e.Recorder.dt_witness
        then has_unverified_theory_leaf := true)
      ev.theory;
    (* the closure engine: axioms (inputs both origins + theory leaves) then verified
       learned clauses, folded incrementally. *)
    let bcp = Bcp.create () in
    Bcp.add_axioms
      bcp
      (List.map
         (fun (e : Recorder.input_event) -> dedup_clause e.Recorder.clause)
         ev.inputs);
    Bcp.add_axioms
      bcp
      (List.map
         (fun (e : Recorder.theory_event) -> dedup_clause e.Recorder.clause)
         ev.theory);
    Bcp.propagate bcp;
    (* (b) every declared level-0 unit is inside the re-derived axiom closure. *)
    List.iter
      (fun (u : Recorder.unit_event) ->
        match lit_status bcp.Bcp.assign u.Recorder.lit with
        | LTrue -> ()
        | LFalse | LUnassigned ->
          rejectf
            "declared level-0 unit (id %d) is not entailed by BCP over the inputs"
            u.Recorder.id)
      ev.units;
    (* (c) each learned clause replays by ordered RUP over its recorded antecedents, then
       is folded into the closure for the clauses that cite it downstream. [verified]
       grows in emission order and gates learned-clause hint resolution (CRIT-1): a clause
       may only cite EARLIER, already-verified learned clauses — never itself or a later
       one.

       LEARNED-CLAUSE FULL-CLOSURE RUP FALLBACK (fix task #56, sibling of the #47 E1/E2
       terminal fallback). The hinted ordered chain is the FAST path. When it fails, fall
       back to the RUP ground truth: does [base + ¬clause] derive ⊥ by unrestricted BCP
       fixpoint over the whole verified closure? That is exactly [Bcp.refutes_under bcp]
       seeded with the clause's literals negated — the same primitive E3 (and #47) use,
       now unifying ALL replay sites on one acceptance criterion: the cited chain/witness
       is ADVISORY, and the ground truth is UP-derivability of ⊥ from the admitted
       axioms + earlier-verified learned clauses. Needed because the emitter records the
       antecedent chain valid in the SOLVER's incremental level-0 state, while the
       checker's batch closure over the full theory/cut-leaf union can satisfy a cited
       antecedent (a literal flips true vs solver state — task #52 id-6571/6572),
       stranding the hinted chain even though the clause is genuinely entailed.

       SOUNDNESS: [bcp] here holds ONLY the admitted axioms (guarded inputs + theory
       leaves) plus learned clauses already verified in THIS loop — [Bcp.add_learned] for
       [le] runs only AFTER acceptance, so [bcp.db] at [le]'s turn contains no later or
       self clause (the CRIT-1 emission-order invariant, now load-bearing for the
       fallback: it reuses the verified DB rather than the cited ids, so a
       self/forward/forged citation cannot launder a clause — an unentailed clause yields
       no ⊥ and is still rejected). BCP fixpoint is not SEARCH (no case splits), so the
       "never searches" contract holds. *)
    let verified = Hashtbl.create 256 in
    let learned_verified id = Hashtbl.mem verified id in
    List.iter
      (fun (le : Recorder.learned_event) ->
        let cl = dedup_clause le.Recorder.clause in
        let accept () =
          Hashtbl.replace verified le.Recorder.id ();
          Bcp.add_learned bcp cl
        in
        match
          ordered_rup
            (Bcp.snapshot bcp)
            ~clause:cl
            ~antecedents:le.Recorder.antecedents
            ~resolve
            ~learned_verified
        with
        | Ok () -> accept ()
        | Error reason ->
          (* Fallback fires ONLY on the ordered-RUP Error; the hinted fast path above is
             byte-unchanged. The fallback relaxes the ORDERING/sufficiency of the cited
             chain, but citation WELL-FORMEDNESS stays a hard gate: every cited id must
             still resolve to a real content clause and, if learned, be ALREADY verified
             (the CRIT-1 anti-circularity gate — kept load-bearing here as defense in
             depth, not left to rely solely on "an unentailed clause derives no ⊥"). A
             dangling/ambiguous/forward-or-self learned citation is a malformed stream and
             is rejected regardless of entailment. *)
          let citations_wellformed =
            List.for_all
              (fun id ->
                match (resolve id : resolution) with
                | Found (Klearned, _) -> learned_verified id
                | Found _ -> true
                | Ambiguous | Dangling -> false)
              le.Recorder.antecedents
          in
          if citations_wellformed
             && Bcp.refutes_under bcp (List.map Sat.neg_lit (Array.to_list cl))
          then (
            incr fallback_firings;
            accept ())
          else
            rejectf
              "learned clause (id %d) fails ordered-RUP replay (%s) AND the verified \
               closure does not entail it (base + ¬clause derives no ⊥)"
              le.Recorder.id
              reason)
      ev.learned;
    (* terminal conclusion (§4.0 E1–E4). *)
    (* E1/E2 CITED-CLAUSE FALLBACK (fix task #47). The E1/E2 witness is normally the cited
       clause being falsified by the level-0 closure. But the emitter records the clause
       that was falsified in the SOLVER's incremental level-0 state, and the checker's
       batch closure over the full theory-leaf union can reach ⊥ through a DIFFERENT
       clause and, in doing so, force a variable that SATISFIES the cited one (rings
       id-7866, task #46). So when the cited clause is not falsified, FALL BACK to
       [Bcp.refutes_under bcp []] — the level-0 closure derives ⊥ unconditionally. This is
       exactly the E3 [refutes_under] idiom with no assumptions; it unifies the three
       terminals. SOUND: the closure is built only from admitted axioms (guarded inputs +
       theory leaves trusted at this stage) and RUP-verified learned clauses, so a
       BCP-derived ⊥ over them is a genuine level-0 refutation regardless of which clause
       is the syntactic witness. The cited id therefore becomes ADVISORY for E1/E2 (same
       relaxation philosophy as the #42 non-minimal chains); the acceptance criterion
       stays "a genuine unit-propagation derivation of ⊥ from validated clauses". A
       consistent closure still fails both disjuncts and is rejected. *)
    (match conclusion with
     | Sat.Root_empty { input_id } ->
       (* E1 (Query) / E4 (Theory_lemma): a clause that filtered to [] under the level-0
          closure. Kind-keyed: MUST be an input (the wrong-kind guard). *)
       let _kind, clause =
         resolve_as
           ~what:"Root_empty"
           ~allowed:[ Kinput Sat.Query; Kinput Sat.Theory_lemma ]
           input_id
       in
       if not (falsified bcp.Bcp.assign clause || Bcp.refutes_under bcp [])
       then
         rejectf
           "Root_empty cites id %d, and the level-0 closure is not inconsistent (neither \
            the cited clause is falsified nor does BCP over the closure derive ⊥)"
           input_id
     | Sat.Level0_conflict { conflict_id } ->
       (* E2: a level-0 conflict clause — a Boolean input/learned clause or a theory
          Conflict transient. (A theory Reason leaf is a propagation, never a conflict.) *)
       let _kind, clause =
         resolve_as
           ~what:"Level0_conflict"
           ~allowed:
             [ Kinput Sat.Query; Kinput Sat.Theory_lemma; Klearned; Ktheory Sat.Conflict ]
           conflict_id
       in
       if not (falsified bcp.Bcp.assign clause || Bcp.refutes_under bcp [])
       then
         rejectf
           "Level0_conflict cites id %d, and the level-0 closure is not inconsistent \
            (neither the cited clause is falsified nor does BCP over the closure derive \
            ⊥)"
           conflict_id
     | Sat.Failed_assumption { antecedents } ->
       (* E3, the universal session exit. The recorded antecedents are the assumption-
          forcing chain (the Lean cert's explicit []-derivation hints); the OCaml checker
          confirms each resolves kind-keyed, then refutes the assumptions by BCP over the
          verified clause DB (seeding the assumption literals true = the §1.0 selector
          strip effect: an assumed-true selector's ¬sel literal is false throughout). This
          is robust to the frequent level-0-failure case where analyze_final backjumped to
          0 and emitted [] antecedents — the verified learned clauses in the DB carry the
          forcing. *)
       List.iter
         (fun id ->
           ignore
             (resolve_as
                ~what:"Failed_assumption antecedent"
                ~allowed:
                  [ Kinput Sat.Query
                  ; Kinput Sat.Theory_lemma
                  ; Klearned
                  ; Ktheory Sat.Reason
                  ]
                id
              : kind * Sat.lit array))
         antecedents;
       if not (Bcp.refutes_under bcp ev.assumptions)
       then
         rejectf
           "Failed_assumption: seeding the assumptions true does not refute the verified \
            clause DB by BCP");
    (* Promote only a proof whose every theory leaf was checked. Missing witness classes
       stay explicitly conditional; a bad claimed witness was rejected above. *)
    if !has_unverified_theory_leaf then Valid_modulo_theory_leaves else Valid
  with
  | Reject v -> v
;;

(* Certificate emission recorder (ADR-0013 §4.0, M5 step 1). See recorder.mli.
   Stdlib-only; reads only the frozen Sat trace seam. The recorder is a pure sink — it
   never feeds back into search — so a solve with a recorder installed is bit-identical to
   one without, except that the (compile-out-able) trace hooks fire. *)

module Sat = Oxsmt_solver.Sat

type lia_premise =
  { lit : Sat.lit
  ; multiplier : Oxsmt_lia.Rational.t
  }

type lia_conflict_witness = { premises : lia_premise list }
type euf_leaf_witness = { clause : Sat.lit list }

type dt_distinctness_witness =
  { clause : Sat.lit list
  ; left : Oxsmt_core.Term.t
  ; right : Oxsmt_core.Term.t
  }

type rewrite_statement =
  { id : int
  ; context : Oxsmt_core.Context.t
  ; original : Oxsmt_core.Term.t list
  ; reduced : Oxsmt_core.Term.t list
  }

type equality_definition =
  { name : string
  ; sort : Oxsmt_core.Sort.t
  ; value : Oxsmt_core.Term.t
  }

type equality_elimination_witness =
  { statement_id : int
  ; definitions : equality_definition list
  }

type clausify_group =
  { id : int
  ; statement_id : int
  ; source : Oxsmt_core.Term.t
  ; preprocessed : Oxsmt_core.Term.t
  ; selector : Sat.var option
  ; bindings : (Oxsmt_core.Term.t * Sat.var) list option
  ; input_ids : int list
  }

type pending_clausify =
  { id : int
  ; statement_id : int
  ; source : Oxsmt_core.Term.t
  ; preprocessed : Oxsmt_core.Term.t
  ; mutable selector : Sat.var option
  ; mutable bindings : (Oxsmt_core.Term.t * Sat.var) list option
  ; mutable input_ids_rev : int list
  }

type atom_event =
  { var : Sat.var
  ; atom : Oxsmt_core.Term.t
  }

type input_event =
  { id : int
  ; clause : Sat.lit array
  ; origin : Sat.origin
  ; clausify_group : int option
  }

type unit_event =
  { id : int
  ; lit : Sat.lit
  }

type learned_event =
  { id : int
  ; clause : Sat.lit array
  ; antecedents : int list
  ; btlevel : int
  }

type theory_event =
  { id : int
  ; clause : Sat.lit array
  ; role : Sat.theory_clause_role
  ; lia_witness : lia_conflict_witness option
  ; euf_witness : euf_leaf_witness option
  ; dt_registry : Oxsmt_core.Datatype_defs.t option
  ; dt_witness : dt_distinctness_witness option
  }

type dt_claim =
  { registry : Oxsmt_core.Datatype_defs.t
  ; witness : dt_distinctness_witness
  }

(* Events are accumulated newest-first (O(1) append) and reversed by the accessors. *)
type t =
  { mutable inputs_rev : input_event list
  ; mutable rewrite_statements_rev : rewrite_statement list
  ; mutable equality_witnesses_rev : equality_elimination_witness list
  ; mutable clausify_groups_rev : clausify_group list
  ; mutable next_statement_id : int
  ; mutable next_clausify_id : int
  ; mutable pending_clausify : pending_clausify option
  ; mutable atoms_rev : atom_event list
  ; mutable units_rev : unit_event list
  ; mutable learned_rev : learned_event list
  ; mutable theory_rev : theory_event list
  ; mutable pending_lia : lia_conflict_witness list
  ; mutable euf_claims : euf_leaf_witness list
  ; mutable dt_claims : dt_claim list
  ; mutable conclusion : Sat.unsat_conclusion option
  }

let create () =
  { inputs_rev = []
  ; rewrite_statements_rev = []
  ; equality_witnesses_rev = []
  ; clausify_groups_rev = []
  ; next_statement_id = 0
  ; next_clausify_id = 0
  ; pending_clausify = None
  ; atoms_rev = []
  ; units_rev = []
  ; learned_rev = []
  ; theory_rev = []
  ; pending_lia = []
  ; euf_claims = []
  ; dt_claims = []
  ; conclusion = None
  }
;;

let record_equality_elimination t ~context ~original ~reduced ~definitions =
  let id = t.next_statement_id in
  t.next_statement_id <- id + 1;
  t.rewrite_statements_rev
  <- { id; context; original; reduced } :: t.rewrite_statements_rev;
  t.equality_witnesses_rev
  <- { statement_id = id; definitions } :: t.equality_witnesses_rev;
  id
;;

let begin_clausify t ~statement_id ~source ~preprocessed =
  if Option.is_some t.pending_clausify
  then invalid_arg "Recorder.begin_clausify: nested clausification group";
  let id = t.next_clausify_id in
  t.next_clausify_id <- id + 1;
  t.pending_clausify
  <- Some
       { id
       ; statement_id
       ; source
       ; preprocessed
       ; selector = None
       ; bindings = None
       ; input_ids_rev = []
       }
;;

let record_clausify_bindings t ~selector ~bindings =
  match t.pending_clausify with
  | None -> ()
  | Some pending ->
    if Option.is_some pending.bindings
    then invalid_arg "Recorder.record_clausify_bindings: duplicate binding map";
    pending.selector <- Some selector;
    pending.bindings <- Some bindings
;;

let end_clausify t =
  match t.pending_clausify with
  | None -> invalid_arg "Recorder.end_clausify: no open clausification group"
  | Some pending ->
    t.clausify_groups_rev
    <- { id = pending.id
       ; statement_id = pending.statement_id
       ; source = pending.source
       ; preprocessed = pending.preprocessed
       ; selector = pending.selector
       ; bindings = pending.bindings
       ; input_ids = List.rev pending.input_ids_rev
       }
       :: t.clausify_groups_rev;
    t.pending_clausify <- None
;;

let record_lia_conflict t ~premise_lits ~multipliers =
  let premises =
    match List.combine premise_lits multipliers with
    | exception Invalid_argument _ -> []
    | rows -> List.map (fun (lit, multiplier) -> { lit; multiplier }) rows
  in
  (* Even a malformed length combination remains a CLAIMED witness. Attaching an empty
     witness makes the checker reject it; silently dropping to the trusted-leaf verdict
     would turn corrupt evidence into a pass. Correct production has equal, nonzero
     lengths, and at most one item waits because SAT materializes the conflict
     synchronously after the callback returns. *)
  t.pending_lia <- t.pending_lia @ [ { premises } ]
;;

let record_euf_leaf t ~clause =
  (* Do not consume these claims FIFO. Under chronological backtracking the SAT core can
     ask the theory for a reason, snapshot it, and materialize the clause only later. An
     exact content match is stable across that delay; retaining the claim also covers a
     repeated materialization of the same valid implication. *)
  t.euf_claims <- { clause } :: t.euf_claims
;;

let record_dt_distinctness t ~registry ~clause ~left ~right =
  let witness = { clause; left; right } in
  t.dt_claims <- { registry; witness } :: t.dt_claims
;;

let record_theory_atom t ~var ~atom = t.atoms_rev <- { var; atom } :: t.atoms_rev

let trace t : Sat.trace =
  { Sat.on_input =
      (fun ~id ~clause ~origin ->
        let clausify_group =
          Option.map (fun (pending : pending_clausify) -> pending.id) t.pending_clausify
        in
        t.inputs_rev <- { id; clause; origin; clausify_group } :: t.inputs_rev;
        match origin, t.pending_clausify with
        | Sat.Query, Some pending -> pending.input_ids_rev <- id :: pending.input_ids_rev
        | (Sat.Query | Sat.Theory_lemma), (None | Some _) -> ())
  ; on_unit = (fun ~id ~lit -> t.units_rev <- { id; lit } :: t.units_rev)
  ; on_learned =
      (fun ~id ~clause ~antecedents ~btlevel ->
        t.learned_rev <- { id; clause; antecedents; btlevel } :: t.learned_rev)
  ; on_theory_clause =
      (fun ~id ~clause ~role ->
        let lia_witness =
          match role, t.pending_lia with
          | Sat.Conflict, witness :: rest ->
            t.pending_lia <- rest;
            Some witness
          | (Sat.Conflict | Sat.Reason), _ -> None
        in
        let euf_witness =
          List.find_opt
            (fun (witness : euf_leaf_witness) -> witness.clause = Array.to_list clause)
            t.euf_claims
        in
        let dt_claim =
          List.find_opt
            (fun (claim : dt_claim) -> claim.witness.clause = Array.to_list clause)
            t.dt_claims
        in
        let dt_registry, dt_witness =
          match dt_claim with
          | None -> None, None
          | Some claim -> Some claim.registry, Some claim.witness
        in
        t.theory_rev
        <- { id; clause; role; lia_witness; euf_witness; dt_registry; dt_witness }
           :: t.theory_rev)
  ; on_unsat = (fun c -> t.conclusion <- Some c)
  }
;;

let inputs t = List.rev t.inputs_rev
let rewrite_statements t = List.rev t.rewrite_statements_rev
let equality_witnesses t = List.rev t.equality_witnesses_rev
let clausify_groups t = List.rev t.clausify_groups_rev
let atoms t = List.rev t.atoms_rev
let units t = List.rev t.units_rev
let learned t = List.rev t.learned_rev
let theory_clauses t = List.rev t.theory_rev
let conclusion t = t.conclusion

(* content-bearing id occurrence COUNTS (sat.mli id-resolvability: on_input, on_learned,
   on_theory_clause). A count MAP, not a set: within one solver every emitted id is unique
   (fresh_id is strictly monotonic and no id is surfaced by two content events), so a
   content id with count > 1 means TWO different clauses share an id — the only way that
   happens is one recorder recording two solvers' streams (each restarting ids from 0),
   the codex HIGH-4 misuse. Binding a recorder to a single solver would need a Sat.t
   identity accessor, which the frozen sat.mli does not expose; rejecting ambiguous ids is
   the sound alternative. *)
let content_counts t =
  let tbl = Hashtbl.create 256 in
  let bump id =
    Hashtbl.replace
      tbl
      id
      (1
       +
       try Hashtbl.find tbl id with
       | Not_found -> 0)
  in
  List.iter (fun (e : input_event) -> bump e.id) t.inputs_rev;
  List.iter (fun (e : learned_event) -> bump e.id) t.learned_rev;
  List.iter (fun (e : theory_event) -> bump e.id) t.theory_rev;
  tbl
;;

let unresolved_citations t =
  let counts = content_counts t in
  (* a cited id resolves IFF exactly one content event carries it; count 0 = dangling,
     count > 1 = ambiguous (two clauses, one id) — both fail-closed to unresolved. *)
  let resolves id =
    match Hashtbl.find_opt counts id with
    | Some 1 -> true
    | _ -> false
  in
  let from_conclusion =
    match t.conclusion with
    | None -> []
    | Some (Sat.Root_empty { input_id }) -> [ input_id ]
    | Some (Sat.Level0_conflict { conflict_id }) -> [ conflict_id ]
    | Some (Sat.Failed_assumption { antecedents }) -> antecedents
  in
  let cited =
    List.fold_left
      (fun acc (e : learned_event) -> e.antecedents @ acc)
      from_conclusion
      t.learned_rev
  in
  List.sort_uniq compare (List.filter (fun id -> not (resolves id)) cited)
;;

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

type atom_event =
  { var : Sat.var
  ; atom : Oxsmt_core.Term.t
  }

type input_event =
  { id : int
  ; clause : Sat.lit array
  ; origin : Sat.origin
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
  }

(* Events are accumulated newest-first (O(1) append) and reversed by the accessors. *)
type t =
  { mutable inputs_rev : input_event list
  ; mutable atoms_rev : atom_event list
  ; mutable units_rev : unit_event list
  ; mutable learned_rev : learned_event list
  ; mutable theory_rev : theory_event list
  ; mutable pending_lia : lia_conflict_witness list
  ; mutable euf_claims : euf_leaf_witness list
  ; mutable conclusion : Sat.unsat_conclusion option
  }

let create () =
  { inputs_rev = []
  ; atoms_rev = []
  ; units_rev = []
  ; learned_rev = []
  ; theory_rev = []
  ; pending_lia = []
  ; euf_claims = []
  ; conclusion = None
  }
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

let record_theory_atom t ~var ~atom = t.atoms_rev <- { var; atom } :: t.atoms_rev

let trace t : Sat.trace =
  { Sat.on_input =
      (fun ~id ~clause ~origin -> t.inputs_rev <- { id; clause; origin } :: t.inputs_rev)
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
        t.theory_rev <- { id; clause; role; lia_witness; euf_witness } :: t.theory_rev)
  ; on_unsat = (fun c -> t.conclusion <- Some c)
  }
;;

let inputs t = List.rev t.inputs_rev
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

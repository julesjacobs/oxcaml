(* Certificate emission recorder (ADR-0013 §4.0, M5 step 1). See recorder.mli.
   Stdlib-only; reads only the frozen Sat trace seam. The recorder is a pure sink — it
   never feeds back into search — so a solve with a recorder installed is bit-identical to
   one without, except that the (compile-out-able) trace hooks fire. *)

module Sat = Oxsmt_solver.Sat

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
  }

(* Events are accumulated newest-first (O(1) append) and reversed by the accessors. *)
type t =
  { mutable inputs_rev : input_event list
  ; mutable units_rev : unit_event list
  ; mutable learned_rev : learned_event list
  ; mutable theory_rev : theory_event list
  ; mutable conclusion : Sat.unsat_conclusion option
  }

let create () =
  { inputs_rev = []
  ; units_rev = []
  ; learned_rev = []
  ; theory_rev = []
  ; conclusion = None
  }
;;

let trace t : Sat.trace =
  { Sat.on_input =
      (fun ~id ~clause ~origin -> t.inputs_rev <- { id; clause; origin } :: t.inputs_rev)
  ; on_unit = (fun ~id ~lit -> t.units_rev <- { id; lit } :: t.units_rev)
  ; on_learned =
      (fun ~id ~clause ~antecedents ~btlevel ->
        t.learned_rev <- { id; clause; antecedents; btlevel } :: t.learned_rev)
  ; on_theory_clause =
      (fun ~id ~clause ~role -> t.theory_rev <- { id; clause; role } :: t.theory_rev)
  ; on_unsat = (fun c -> t.conclusion <- Some c)
  }
;;

let inputs t = List.rev t.inputs_rev
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

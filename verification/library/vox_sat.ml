open Vox_sat_spec

type answer : immutable_data mod total = Sat of bool list | Unsat | Unknown
  [@@inductive]
type report : immutable_data mod total = {answer : answer; fuel_left : int}
type input_error = Vox_sat_spec.input_error =
  | Invalid_formula
  | Unsupported_variable_count
  | Too_many_clauses
  | Too_many_literals

let (solve @ total) :
  (fuel : {fuel : int | 0 <= fuel}) ->
  (n : int) -> (formula : formula) ->
  {r : (report, input_error) result |
    match r with
    | Error error -> classify_input n formula === Some error
    | Ok answer -> classify_input n formula === None
      && 0 <= answer.fuel_left && answer.fuel_left <= fuel
      && match answer.answer with
         | Sat assignment ->
           well_sized n assignment && eval_formula assignment formula
         | Unsat -> unsatisfiable n formula
         | Unknown -> true} =
  fun fuel n formula ->
  ghost_ (classify_input_def n formula);
  match Vox_sat_proof.solve fuel n formula with
  | Error error -> Error error
  | Ok report ->
    let answer = match report.answer with
      | Vox_sat_proof.Sat assignment -> Sat assignment
      | Vox_sat_proof.Unknown -> Unknown
      | Vox_sat_proof.Unsat ->
        ghost_ (Vox_sat_proof.empty_unsatisfiable n formula
          (Vox_sat_proof.exhaustive_result n formula));
        Unsat
    in
    Ok {answer; fuel_left = report.fuel_left}

let (unsat_at @ total) : (n : int) ->
    (formula : {f : formula | unsatisfiable n f}) ->
    (assignment : bool list) ->
    {u : unit | not (eval_formula assignment formula)} =
  fun n formula assignment ->
  ghost_ (Vox_sat_proof.semantic_unsat_at n formula assignment);
  ()

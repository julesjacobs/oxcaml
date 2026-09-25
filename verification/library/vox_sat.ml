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
    | Error Unsupported_variable_count -> n < 0 || n > 256
    | Error Too_many_clauses ->
      0 <= n && n <= 256 && not (clauses_fit 4096 formula)
    | Error Too_many_literals ->
      0 <= n && n <= 256 && clauses_fit 4096 formula
      && not (literals_fit 65536 formula)
    | Error Invalid_formula ->
      0 <= n && n <= 256 && clauses_fit 4096 formula
      && literals_fit 65536 formula
      && not (valid_formula n formula)
    | Ok answer ->
      0 <= n && n <= 256 && clauses_fit 4096 formula
      && literals_fit 65536 formula && valid_formula n formula
      && 0 <= answer.fuel_left && answer.fuel_left <= fuel
      && match answer.answer with
         | Sat assignment ->
           well_sized n assignment && eval_formula assignment formula
         | Unsat -> unsatisfiable n formula
         | Unknown -> true} =
  fun fuel n formula ->
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

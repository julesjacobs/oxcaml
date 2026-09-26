type statistics : immutable_data mod total = {
  decisions : int;
  conflicts : int;
  learned : int;
  backjumps : int;
  work : int;
}

type answer : immutable_data mod total = Sat of bool list | Unsat | Unknown
[@@inductive]

type report : immutable_data mod total = {
  answer : answer;
  statistics : statistics;
}

type input_error = Invalid_fuel | Invalid_input of Vox_sat_spec.input_error

let (solve) :
  (fuel : int) -> (n : int) ->
  (formula : Vox_sat_spec.formula) ->
  {r : (report, input_error) result |
    match r with
    | Error _ -> true
    | Ok report ->
      match report.answer with
      | Sat assignment -> Vox_sat_spec.check n formula assignment
      | Unsat -> Vox_sat_spec.unsatisfiable n formula
      | Unknown -> true} =
  fun fuel n formula ->
  match Vox_cdcl_proof.solve fuel n formula with
  | Error Vox_cdcl_proof.Invalid_fuel -> Error Invalid_fuel
  | Error (Vox_cdcl_proof.Invalid_input error) -> Error (Invalid_input error)
  | Ok report ->
    let statistics = {
      decisions = report.statistics.decisions;
      conflicts = report.statistics.conflicts;
      learned = report.statistics.learned;
      backjumps = report.statistics.backjumps;
      work = report.statistics.work;
    } in
    let answer = match report.answer with
      | Vox_cdcl_proof.Sat assignment -> Sat assignment
      | Vox_cdcl_proof.Unknown -> Unknown
      | Vox_cdcl_proof.Unsat entry ->
        ghost_ (Vox_sat_proof.empty_unsatisfiable n formula entry);
        Unsat
    in
    Ok {answer; statistics}

type statistics : immutable_data mod total = {
  decisions : int;
  conflicts : int;
  learned : int;
  backjumps : int;
  steps : int;
}

type answer : immutable_data mod total = Sat of bool list | Unsat | Unknown
[@@inductive]

type report : immutable_data mod total = {
  answer : answer;
  statistics : statistics;
}

type input_error = Invalid_fuel | Invalid_input of Vox_sat_spec.input_error

let (solve @ total) :
  (fuel : int) -> (n : int) ->
  (formula : Vox_sat_spec.formula) ->
  {r : (report, input_error) result |
    match r with
    | Error Invalid_fuel -> fuel < 0
    | Error (Invalid_input Vox_sat_spec.Unsupported_variable_count) ->
      0 <= fuel && (n < 0 || n > 256)
    | Error (Invalid_input Vox_sat_spec.Too_many_clauses) ->
      0 <= fuel && 0 <= n && n <= 256
      && not (Vox_sat_spec.clauses_fit 4096 formula)
    | Error (Invalid_input Vox_sat_spec.Too_many_literals) ->
      0 <= fuel && 0 <= n && n <= 256 && Vox_sat_spec.clauses_fit 4096 formula
      && not (Vox_sat_spec.literals_fit 65536 formula)
    | Error (Invalid_input Vox_sat_spec.Invalid_formula) ->
      0 <= fuel && 0 <= n && n <= 256 && Vox_sat_spec.clauses_fit 4096 formula
      && Vox_sat_spec.literals_fit 65536 formula
      && not (Vox_sat_spec.valid_formula n formula)
    | Ok report ->
      0 <= fuel && 0 <= n && n <= 256 && Vox_sat_spec.clauses_fit 4096 formula
      && Vox_sat_spec.literals_fit 65536 formula
      && Vox_sat_spec.valid_formula n formula
      && match report.answer with
      | Sat assignment -> Vox_sat_spec.check n formula assignment
      | Unsat -> Vox_sat_spec.unsatisfiable n formula
      | Unknown -> true} =
  fun fuel n formula ->
  match Vox_cdcl_total_proof.solve fuel n formula with
  | Error Vox_cdcl_total_proof.Invalid_fuel -> Error Invalid_fuel
  | Error (Vox_cdcl_total_proof.Invalid_input error) -> Error (Invalid_input
    error)
  | Ok report ->
    let statistics = {
      decisions = report.statistics.decisions;
      conflicts = report.statistics.conflicts;
      learned = report.statistics.learned;
      backjumps = report.statistics.backjumps;
      steps = report.statistics.steps;
    } in
    let answer = match report.answer with
      | Vox_cdcl_total_proof.Sat assignment -> Sat assignment
      | Vox_cdcl_total_proof.Unknown -> Unknown
      | Vox_cdcl_total_proof.Unsat entry ->
        ghost_ (Vox_sat_proof.empty_unsatisfiable n formula entry);
        Unsat
    in
    Ok {answer; statistics}

let (solve_with_fallback @ total) :
  (fuel : int) -> (depth_fuel : int) -> (n : int) ->
  (formula : Vox_sat_spec.formula) ->
  {r : (report, input_error) result |
    match r with
    | Error Invalid_fuel -> fuel < 0 || depth_fuel < 0
    | Error (Invalid_input Vox_sat_spec.Unsupported_variable_count) ->
      0 <= fuel && 0 <= depth_fuel && (n < 0 || n > 256)
    | Error (Invalid_input Vox_sat_spec.Too_many_clauses) ->
      0 <= fuel && 0 <= depth_fuel && 0 <= n && n <= 256
      && not (Vox_sat_spec.clauses_fit 4096 formula)
    | Error (Invalid_input Vox_sat_spec.Too_many_literals) ->
      0 <= fuel && 0 <= depth_fuel && 0 <= n && n <= 256
      && Vox_sat_spec.clauses_fit 4096 formula
      && not (Vox_sat_spec.literals_fit 65536 formula)
    | Error (Invalid_input Vox_sat_spec.Invalid_formula) ->
      0 <= fuel && 0 <= depth_fuel && 0 <= n && n <= 256
      && Vox_sat_spec.clauses_fit 4096 formula
      && Vox_sat_spec.literals_fit 65536 formula
      && not (Vox_sat_spec.valid_formula n formula)
    | Ok report ->
      0 <= fuel && 0 <= depth_fuel && 0 <= n && n <= 256
      && Vox_sat_spec.clauses_fit 4096 formula
      && Vox_sat_spec.literals_fit 65536 formula
      && Vox_sat_spec.valid_formula n formula
      && match report.answer with
      | Sat assignment -> Vox_sat_spec.check n formula assignment
      | Unsat -> Vox_sat_spec.unsatisfiable n formula
      | Unknown -> depth_fuel <= n} =
  fun fuel depth_fuel n formula ->
  match Vox_cdcl_total_proof.solve_with_fallback fuel depth_fuel n formula with
  | Error Vox_cdcl_total_proof.Invalid_fuel -> Error Invalid_fuel
  | Error (Vox_cdcl_total_proof.Invalid_input error) -> Error (Invalid_input
    error)
  | Ok report ->
    let statistics = {
      decisions = report.statistics.decisions;
      conflicts = report.statistics.conflicts;
      learned = report.statistics.learned;
      backjumps = report.statistics.backjumps;
      steps = report.statistics.steps;
    } in
    let answer = match report.answer with
      | Vox_cdcl_total_proof.Sat assignment -> Sat assignment
      | Vox_cdcl_total_proof.Unknown -> Unknown
      | Vox_cdcl_total_proof.Unsat entry ->
        ghost_ (Vox_sat_proof.empty_unsatisfiable n formula entry);
        Unsat
    in
    Ok {answer; statistics}

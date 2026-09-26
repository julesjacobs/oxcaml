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

let (convert_report @ total) :
    (n : {n : int | 0 <= n}) @ ghost ->
    (formula : {f : Vox_sat_spec.formula |
      Vox_sat_spec.valid_formula n f}) @ ghost ->
    (source : {r : Vox_cdcl_total_proof.report |
      match r.answer with
      | Vox_cdcl_total_proof.Sat assignment ->
        Vox_sat_spec.check n formula assignment
      | Vox_cdcl_total_proof.Unsat entry ->
        Vox_sat_proof.derivation_valid formula entry.proof
        && Vox_sat_proof.same_clause
          (Vox_sat_proof.conclusion formula entry.proof) entry.clause
        && entry.clause === []
      | Vox_cdcl_total_proof.Unknown -> true}) ->
    {r : report |
      r.statistics.decisions = source.statistics.decisions
      && r.statistics.conflicts = source.statistics.conflicts
      && r.statistics.learned = source.statistics.learned
      && r.statistics.backjumps = source.statistics.backjumps
      && r.statistics.steps = source.statistics.steps
      && match source.answer with
      | Vox_cdcl_total_proof.Sat assignment ->
        r.answer === Sat assignment && Vox_sat_spec.check n formula assignment
      | Vox_cdcl_total_proof.Unsat _ ->
        r.answer === Unsat && Vox_sat_spec.unsatisfiable n formula
      | Vox_cdcl_total_proof.Unknown -> r.answer === Unknown} =
  fun n formula source ->
  let statistics = {
    decisions = source.statistics.decisions;
    conflicts = source.statistics.conflicts;
    learned = source.statistics.learned;
    backjumps = source.statistics.backjumps;
    steps = source.statistics.steps;
  } in
  let answer = match source.answer with
    | Vox_cdcl_total_proof.Sat assignment -> Sat assignment
    | Vox_cdcl_total_proof.Unknown -> Unknown
    | Vox_cdcl_total_proof.Unsat entry ->
      ghost_ (Vox_sat_proof.empty_unsatisfiable n formula entry);
      Unsat
  in
  {answer; statistics}

let (solve @ total) :
  (fuel : int) -> (n : int) ->
  (formula : Vox_sat_spec.formula) ->
  {r : (report, input_error) result |
    match r with
    | Error Invalid_fuel -> fuel < 0
    | Error (Invalid_input error) ->
      0 <= fuel
      && Vox_sat_spec.classify_input n formula === Some error
    | Ok report ->
      0 <= fuel && Vox_sat_spec.classify_input n formula === None
      && match report.answer with
      | Sat assignment -> Vox_sat_spec.check n formula assignment
      | Unsat -> Vox_sat_spec.unsatisfiable n formula
      | Unknown -> report.statistics.steps = fuel} =
  fun fuel n formula ->
  ghost_ (Vox_sat_spec.classify_input_def n formula);
  match Vox_cdcl_total_proof.solve fuel n formula with
  | Error Vox_cdcl_total_proof.Invalid_fuel -> Error Invalid_fuel
  | Error (Vox_cdcl_total_proof.Invalid_input error) -> Error (Invalid_input
    error)
  | Ok report ->
    Ok (convert_report (ghost_ n) (ghost_ formula) report)

let (solve_with_fallback @ total) :
  (fuel : int) -> (depth_fuel : int) -> (n : int) ->
  (formula : Vox_sat_spec.formula) ->
  {r : (report, input_error) result |
    match r with
    | Error Invalid_fuel -> fuel < 0 || depth_fuel < 0
    | Error (Invalid_input error) ->
      0 <= fuel && 0 <= depth_fuel
      && Vox_sat_spec.classify_input n formula === Some error
    | Ok report ->
      0 <= fuel && 0 <= depth_fuel
      && Vox_sat_spec.classify_input n formula === None
      && match report.answer with
      | Sat assignment -> Vox_sat_spec.check n formula assignment
      | Unsat -> Vox_sat_spec.unsatisfiable n formula
      | Unknown -> depth_fuel <= n && report.statistics.steps = fuel} =
  fun fuel depth_fuel n formula ->
  ghost_ (Vox_sat_spec.classify_input_def n formula);
  match Vox_cdcl_total_proof.solve_with_fallback fuel depth_fuel n formula with
  | Error Vox_cdcl_total_proof.Invalid_fuel -> Error Invalid_fuel
  | Error (Vox_cdcl_total_proof.Invalid_input error) -> Error (Invalid_input
    error)
  | Ok report ->
    Ok (convert_report (ghost_ n) (ghost_ formula) report)

let (solve_complete @ total) : (n : int) -> (formula : Vox_sat_spec.formula) ->
  {r : (report, input_error) result | match r with
    | Error Invalid_fuel -> false
    | Error (Invalid_input error) ->
      Vox_sat_spec.classify_input n formula === Some error
    | Ok report -> Vox_sat_spec.classify_input n formula === None
      && match report.answer with
      | Sat assignment -> Vox_sat_spec.check n formula assignment
      | Unsat -> Vox_sat_spec.unsatisfiable n formula
      | Unknown -> false} =
  fun n formula ->
  ghost_ (Vox_sat_spec.classify_input_def n formula);
  match Vox_cdcl_total_proof.solve_complete n formula with
  | Error Vox_cdcl_total_proof.Invalid_fuel -> Error Invalid_fuel
  | Error (Vox_cdcl_total_proof.Invalid_input error) -> Error (Invalid_input
    error)
  | Ok report -> Ok (convert_report (ghost_ n) (ghost_ formula) report)

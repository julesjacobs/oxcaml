type statistics = {
  decisions : int;
  conflicts : int;
  learned : int;
  backjumps : int;
  work : int;
}

type answer = Sat of bool list | Unsat | Unknown
[@@inductive]

type report = {
  answer : answer;
  statistics : statistics;
}

type input_error = Invalid_fuel | Invalid_input of Vox_sat_spec.input_error

val solve : (fuel : int) -> (n : int) ->
  (formula : Vox_sat_spec.formula) ->
  {r : (report, input_error) result |
    match r with
    | Error _ -> true
    | Ok report ->
      match report.answer with
      | Sat assignment -> Vox_sat_spec.check n formula assignment
      | Unsat -> Vox_sat_spec.unsatisfiable n formula
      | Unknown -> true}

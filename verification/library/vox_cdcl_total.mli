type statistics = {
  decisions : int;
  conflicts : int;
  learned : int;
  backjumps : int;
  steps : int;
}

type answer = Sat of bool list | Unsat | Unknown
[@@inductive]

type report = {
  answer : answer;
  statistics : statistics;
}

type input_error = Invalid_fuel | Invalid_input of Vox_sat_spec.input_error

(** Bounded CDCL: the search of [solve_complete] with a budget of [fuel]
    search steps. It may return [Unknown] on any accepted input with
    [fuel >= 0], and [Unknown] implies only [statistics.steps = fuel]. No fuel
    is promised to suffice, and more fuel is not promised to help. *)
val solve :
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
      | Unknown -> report.statistics.steps = fuel} @@ total

(** Complete CDCL on every accepted input, with no fuel argument. *)
val solve_complete : (n : int) -> (formula : Vox_sat_spec.formula) ->
  {r : (report, input_error) result | match r with
    | Error Invalid_fuel -> false
    | Error (Invalid_input error) ->
      Vox_sat_spec.classify_input n formula === Some error
    | Ok report -> Vox_sat_spec.classify_input n formula === None
      && match report.answer with
      | Sat assignment -> Vox_sat_spec.check n formula assignment
      | Unsat -> Vox_sat_spec.unsatisfiable n formula
      | Unknown -> false} @@ total

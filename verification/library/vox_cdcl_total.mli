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

(** Bounded CDCL: termination and sound answers. [Unknown] is permitted for
    every accepted input and fuel value, and proves search exhaustion through
    [statistics.steps = fuel]. No sufficient
    fuel bound, monotonicity in fuel, or eventual CDCL decision is promised. *)
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

open Vox_sat_spec

type answer = Sat of bool list | Unsat | Unknown [@@inductive]
type report = {answer : answer; fuel_left : int}
type input_error = Vox_sat_spec.input_error =
  | Invalid_formula
  | Unsupported_variable_count
  | Too_many_clauses
  | Too_many_literals

val solve :
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
         | Unknown -> true} @@ total

val unsat_at : (n : int) ->
  (formula : {f : formula | unsatisfiable n f}) ->
  (assignment : bool list) ->
  {u : unit | not (eval_formula assignment formula)} @@ total

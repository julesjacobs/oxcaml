open Vox_sat_spec

type answer = Sat of bool list | Unsat | Unknown [@@inductive]
type report = {answer : answer; fuel_left : int}
type input_error =
  | Invalid_formula
  | Unsupported_variable_count
  | Too_many_clauses
  | Too_many_literals

val solve :
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
         | Unknown -> true} @@ total

val unsat_at : (n : int) ->
  (formula : {f : formula | unsatisfiable n f}) ->
  (assignment : bool list) ->
  {u : unit | not (eval_formula assignment formula)} @@ total

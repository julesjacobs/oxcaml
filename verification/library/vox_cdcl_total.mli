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
    every accepted input and fuel value. It can result from search or analysis
    exhaustion, or failure to construct an asserting clause. No sufficient
    fuel bound, monotonicity in fuel, or eventual CDCL decision is promised. *)
val solve :
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
      | Unknown -> true} @@ total

(** Runs bounded CDCL, then a separate DPLL search if CDCL returns [Unknown].
    For accepted inputs and nonnegative CDCL fuel, [depth_fuel >= n + 1]
    guarantees a decision. This guarantee belongs to the combined solver.
    [statistics] describes only its CDCL attempt. *)
val solve_with_fallback :
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
      | Unknown -> depth_fuel <= n} @@ total

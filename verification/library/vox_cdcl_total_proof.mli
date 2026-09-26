type statistics = {
  decisions : int;
  conflicts : int;
  learned : int;
  backjumps : int;
  steps : int;
}

type answer = Sat of bool list | Unsat of Vox_sat_proof.proof_result | Unknown
[@@inductive]

type report = {
  answer : answer;
  statistics : statistics;
}

type input_error = Invalid_fuel | Invalid_input of Vox_sat_proof.input_error

val solve :
  (fuel : int) -> (n : int) ->
  (formula : Vox_sat_spec.formula) ->
  {r : (report, input_error) result |
    match r with
    | Error Invalid_fuel -> fuel < 0
    | Error (Invalid_input Vox_sat_proof.Unsupported_variable_count) ->
      0 <= fuel && (n < 0 || n > 256)
    | Error (Invalid_input Vox_sat_proof.Too_many_clauses) ->
      0 <= fuel && 0 <= n && n <= 256
      && not (Vox_sat_spec.clauses_fit 4096 formula)
    | Error (Invalid_input Vox_sat_proof.Too_many_literals) ->
      0 <= fuel && 0 <= n && n <= 256 && Vox_sat_spec.clauses_fit 4096 formula
      && not (Vox_sat_spec.literals_fit 65536 formula)
    | Error (Invalid_input Vox_sat_proof.Invalid_formula) ->
      0 <= fuel && 0 <= n && n <= 256 && Vox_sat_spec.clauses_fit 4096 formula
      && Vox_sat_spec.literals_fit 65536 formula
      && not (Vox_sat_spec.valid_formula n formula)
    | Ok report ->
      0 <= fuel && 0 <= n && n <= 256 && Vox_sat_spec.clauses_fit 4096 formula
      && Vox_sat_spec.literals_fit 65536 formula
      && Vox_sat_spec.valid_formula n formula
      && match report.answer with
      | Sat assignment -> Vox_sat_spec.check n formula assignment
      | Unsat entry ->
        Vox_sat_proof.derivation_valid formula entry.proof
        && Vox_sat_proof.same_clause
          (Vox_sat_proof.conclusion formula entry.proof) entry.clause
        && entry.clause === []
      | Unknown -> report.statistics.steps = fuel} @@ total

val solve_with_fallback :
  (fuel : int) -> (depth_fuel : int) -> (n : int) ->
  (formula : Vox_sat_spec.formula) ->
  {r : (report, input_error) result |
    match r with
    | Error Invalid_fuel -> fuel < 0 || depth_fuel < 0
    | Error (Invalid_input Vox_sat_proof.Unsupported_variable_count) ->
      0 <= fuel && 0 <= depth_fuel && (n < 0 || n > 256)
    | Error (Invalid_input Vox_sat_proof.Too_many_clauses) ->
      0 <= fuel && 0 <= depth_fuel && 0 <= n && n <= 256
      && not (Vox_sat_spec.clauses_fit 4096 formula)
    | Error (Invalid_input Vox_sat_proof.Too_many_literals) ->
      0 <= fuel && 0 <= depth_fuel && 0 <= n && n <= 256
      && Vox_sat_spec.clauses_fit 4096 formula
      && not (Vox_sat_spec.literals_fit 65536 formula)
    | Error (Invalid_input Vox_sat_proof.Invalid_formula) ->
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
      | Unsat entry ->
        Vox_sat_proof.derivation_valid formula entry.proof
        && Vox_sat_proof.same_clause
          (Vox_sat_proof.conclusion formula entry.proof) entry.clause
        && entry.clause === []
      | Unknown -> depth_fuel <= n && report.statistics.steps = fuel} @@ total

val unsat_at :
  (formula : Vox_sat_spec.formula) ->
  (report : {r : report |
    match r.answer with
    | Unsat entry ->
      Vox_sat_proof.derivation_valid formula entry.proof
      && Vox_sat_proof.same_clause
        (Vox_sat_proof.conclusion formula entry.proof) entry.clause
      && entry.clause === []
    | Sat _ | Unknown -> true}) ->
  (assignment : bool list) ->
  {u : unit |
    match report.answer with
    | Unsat _ -> not (Vox_sat_spec.eval_formula assignment formula)
    | Sat _ | Unknown -> true} @@ total

val solve_complete : (n : int) -> (formula : Vox_sat_spec.formula) ->
  {r : (report, input_error) result | match r with
    | Error Invalid_fuel -> false
    | Error (Invalid_input error) ->
      Vox_sat_spec.classify_input n formula === Some error
    | Ok report -> Vox_sat_spec.classify_input n formula === None
      && match report.answer with
      | Sat assignment -> Vox_sat_spec.check n formula assignment
      | Unsat entry -> Vox_sat_proof.derivation_valid formula entry.proof
        && Vox_sat_proof.same_clause
          (Vox_sat_proof.conclusion formula entry.proof) entry.clause
        && entry.clause === []
      | Unknown -> false} @@ total

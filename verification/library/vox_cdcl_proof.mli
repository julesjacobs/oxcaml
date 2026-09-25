type statistics = {
  decisions : int;
  conflicts : int;
  learned : int;
  backjumps : int;
  work : int;
}

type answer = Sat of bool list | Unsat of Vox_sat_proof.proof_result | Unknown
[@@inductive]

type report = {
  answer : answer;
  statistics : statistics;
}

type input_error = Invalid_fuel | Invalid_input of Vox_sat_proof.input_error

val solve : (fuel : int) -> (n : int) ->
  (formula : Vox_sat_spec.formula) ->
  {r : (report, input_error) result |
    match r with
    | Error _ -> true
    | Ok report ->
      0 <= n && Vox_sat_spec.valid_formula n formula
      &&
      match report.answer with
      | Sat assignment -> Vox_sat_spec.check n formula assignment
      | Unsat entry ->
        Vox_sat_proof.derivation_valid formula entry.proof
        && Vox_sat_proof.same_clause
          (Vox_sat_proof.conclusion formula entry.proof) entry.clause
        && entry.clause === []
      | Unknown -> true}

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

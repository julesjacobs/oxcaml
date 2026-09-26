type statistics = {
  decisions : int;
  conflicts : int;
  learned : int;
  backjumps : int;
  work : int;
}

type answer = Sat of bool list | Unsat of Vox_sat.proof_result | Unknown
[@@inductive]

type report = {
  answer : answer;
  statistics : statistics;
}

type input_error = Invalid_fuel | Invalid_input of Vox_sat.input_error

val solve : (fuel : int) -> (n : int) ->
  (formula : Vox_sat.formula) ->
  {r : (report, input_error) result |
    match r with
    | Error _ -> true
    | Ok report ->
      match report.answer with
      | Sat assignment -> Vox_sat.check n formula assignment
      | Unsat entry ->
        Vox_sat.derivation_valid formula entry.proof
        && Vox_sat.same_clause
          (Vox_sat.conclusion formula entry.proof) entry.clause
        && entry.clause === []
      | Unknown -> true}

val unsat_at :
  (formula : Vox_sat.formula) ->
  (report : {r : report |
    match r.answer with
    | Unsat entry ->
      Vox_sat.derivation_valid formula entry.proof
      && Vox_sat.same_clause
        (Vox_sat.conclusion formula entry.proof) entry.clause
      && entry.clause === []
    | Sat _ | Unknown -> true}) ->
  (assignment : bool list) ->
  {u : unit |
    match report.answer with
    | Unsat _ -> not (Vox_sat.eval_formula assignment formula)
    | Sat _ | Unknown -> true} @@ total

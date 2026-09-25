open Vox_sat_spec

val refutes : int -> formula -> bool @@ total
val check_unsat_trace : int -> formula -> formula -> bool @@ total
val unsat_trace_sound :
  (n : int) -> (database : formula) -> (trace : formula) ->
  (assignment : bool list) ->
  {u : unit |
    if 0 <= n && well_sized n assignment
       && eval_formula assignment database
       && check_unsat_trace n database trace
    then false else true} @@ total

type derivation : immutable_data mod total
val clause_at : formula -> int -> literal list option @@ total
val clause_at_def : (formula : formula) -> (index : int) ->
  {u : unit |
    clause_at formula index ===
      (match formula with
       | [] -> None
       | clause :: rest ->
         if index = 0 then Some clause
         else clause_at rest (index - 1))} @@ total
val resolve_clause : int -> literal list -> literal list ->
  literal list @@ total
val same_clause : literal list -> literal list -> bool @@ total
val derivation_valid : formula -> derivation -> bool @@ total
val conclusion : formula -> derivation -> literal list @@ total
val input_proof :
  (formula : formula) ->
  (index : {i : int |
    match clause_at formula i with Some _ -> true | None -> false}) ->
  {d : derivation |
    derivation_valid formula d
    && same_clause (conclusion formula d)
      (match clause_at formula index with
       | Some clause -> clause | None -> [])} @ ghost @@ total
val resolve_proof :
  (formula : formula) -> (index : int) ->
  (left_clause : literal list) ->
  (right_clause : literal list) ->
  (left : {d : derivation |
    derivation_valid formula d
    && same_clause (conclusion formula d) left_clause}) @ ghost ->
  (right : {d : derivation |
    derivation_valid formula d
    && same_clause (conclusion formula d) right_clause}) @ ghost ->
  {d : derivation |
    derivation_valid formula d
    && same_clause (conclusion formula d)
      (resolve_clause index left_clause right_clause)} @ ghost
  @@ total
val empty_proof_at :
  (formula : formula) ->
  (derivation : {d : derivation |
    derivation_valid formula d
    && same_clause (conclusion formula d) []}) @ ghost ->
  (assignment : bool list) ->
  {u : unit | not (eval_formula assignment formula)} @@ total

type proof_result = private {
  clause : literal list;
  proof : derivation @@ ghost;
}
val exhaustive_result : (n : {n : int | 0 <= n}) ->
  (formula : {f : formula | valid_formula n f && refutes n f}) ->
  {r : proof_result |
    derivation_valid formula r.proof
    && same_clause (conclusion formula r.proof) r.clause
    && r.clause === []} @@ total
val original_result :
  (formula : formula) -> (index : int) ->
  {r : proof_result option |
    match r with
    | None -> clause_at formula index === None
    | Some entry ->
      derivation_valid formula entry.proof
      && same_clause (conclusion formula entry.proof) entry.clause}
  @@ total
val resolve_result :
  (formula : formula) -> (index : int) ->
  (left : {e : proof_result |
    derivation_valid formula e.proof
    && same_clause (conclusion formula e.proof) e.clause}) ->
  (right : {e : proof_result |
    derivation_valid formula e.proof
    && same_clause (conclusion formula e.proof) e.clause}) ->
  {e : proof_result |
    derivation_valid formula e.proof
    && same_clause (conclusion formula e.proof) e.clause
    && same_clause e.clause
      (resolve_clause index left.clause right.clause)} @@ total
val empty_result_at :
  (formula : formula) ->
  (entry : {e : proof_result |
    derivation_valid formula e.proof
    && same_clause (conclusion formula e.proof) e.clause
    && e.clause === []}) ->
  (assignment : bool list) ->
  {u : unit | not (eval_formula assignment formula)} @@ total
val database_valid : formula -> proof_result list -> bool @ ghost @@ total
val database_empty :
  (formula : formula) ->
  {entries : proof_result list | database_valid formula entries && entries ===
    []} @@ total
val database_cons :
  (formula : formula) ->
  (entry : {e : proof_result |
    derivation_valid formula e.proof
    && same_clause (conclusion formula e.proof) e.clause}) ->
  (entries : {es : proof_result list | database_valid formula es}) ->
  {result : proof_result list | database_valid formula result && result ===
    entry :: entries} @@ total
val database_clauses : proof_result list -> formula @@ total
val database_clauses_def : (entries : proof_result list) ->
  {u : unit | database_clauses entries ===
    (match entries with [] -> []
     | entry :: rest -> entry.clause :: database_clauses rest)} @@ total
val database_at :
  (formula : formula) ->
  (entries : {es : proof_result list | database_valid formula es}) ->
  (index : int) ->
  {r : proof_result option |
    match r with
    | None -> clause_at (database_clauses entries) index === None
    | Some entry ->
      derivation_valid formula entry.proof
      && same_clause (conclusion formula entry.proof) entry.clause}
  @@ total

type clause_source = Original_clause of int | Learned_clause of int
[@@inductive]
type resolution_instruction = {
  pivot : int;
  source : clause_source;
  current_positive : bool;
}
val source_clause : formula -> proof_result list -> clause_source ->
  literal list option @@ total
val source_clause_def : (formula : formula) ->
  (database : proof_result list) -> (source : clause_source) ->
  {u : unit | source_clause formula database source ===
    (match source with
     | Original_clause index -> clause_at formula index
     | Learned_clause index -> clause_at (database_clauses database) index)}
  @@ total
val fetch_result :
  (formula : formula) ->
  (database : {d : proof_result list | database_valid formula d}) ->
  (source : clause_source) ->
  {r : proof_result option |
    match r with
    | None -> source_clause formula database source === None
    | Some entry ->
      derivation_valid formula entry.proof
      && same_clause (conclusion formula entry.proof) entry.clause}
  @@ total
val execute_resolution :
  (formula : formula) ->
  (database : {d : proof_result list | database_valid formula d}) ->
  (start : clause_source) ->
  (steps : resolution_instruction list) ->
  {r : proof_result option |
    match r with
    | None -> true
    | Some entry ->
      derivation_valid formula entry.proof
      && same_clause (conclusion formula entry.proof) entry.clause}
  @@ total

type answer = Sat of bool list | Unsat | Unknown [@@inductive]
type report = {answer : answer; fuel_left : int}
val decide_depth : (fuel : {fuel : int | 0 <= fuel}) ->
  (n : {n : int | 0 <= n}) -> (formula : formula) ->
  {r : answer | match r with
    | Sat assignment ->
      well_sized n assignment && eval_formula assignment formula
    | Unsat -> refutes n formula
    | Unknown -> fuel <= n} @@ total

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
         | Unsat -> refutes n formula
         | Unknown -> true} @@ total

val unsat_at :
  (n : {n : int | 0 <= n}) -> (formula : formula) ->
  (r : {r : report |
    match r.answer with Unsat -> refutes n formula | _ -> true}) ->
  (assignment : bool list) ->
  {u : unit |
    if well_sized n assignment then
      match r.answer with
      | Unsat -> not (eval_formula assignment formula)
      | Sat _ | Unknown -> true
    else true} @@ total

type clause_scan : immutable_data mod total =
  | Clause_satisfied
  | Clause_open
  | Clause_unit of literal
  | Clause_conflict
[@@inductive]
type formula_scan : immutable_data mod total =
  | Scan_stable
  | Scan_unit of int * literal
  | Scan_conflict of int
[@@inductive]
val complete_partial : bool option list -> bool list option @@ total
val complete_partial_def : (partial : bool option list) ->
  {u : unit | complete_partial partial ===
    (match partial with
     | [] -> Some []
     | None :: _ -> None
     | Some value :: rest ->
       match complete_partial rest with
       | None -> None
       | Some assignment -> Some (value :: assignment))} @@ total
val scan_formula : bool option list -> formula -> formula_scan @@ total
val scan_formula_complete :
  (partial : bool option list) -> (formula : formula) ->
  (assignment : bool list) ->
  {u : unit |
    if complete_partial partial === Some assignment
       && scan_formula partial formula === Scan_stable
    then eval_formula assignment formula
    else true} @@ total

val scan_formula_source : (partial : bool option list) ->
  (formula : formula) ->
  {u : unit | match scan_formula partial formula with
    | Scan_stable -> true
    | Scan_conflict index | Scan_unit (index, _) ->
      not (clause_at formula index === None)} @@ total

val partial_lookup : bool option list -> int -> bool option @@ total
val partial_lookup_def : (partial : bool option list) -> (index : int) ->
  {u : unit | partial_lookup partial index ===
    (match partial with
     | [] -> Some false
     | value :: rest ->
       if index = 0 then value else partial_lookup rest (index - 1))} @@ total
val scan_formula_unit_unassigned : (partial : bool option list) ->
  (formula : formula) ->
  {u : unit | match scan_formula partial formula with
    | Scan_unit (_, literal) ->
      (match literal with Positive v | Negative v ->
        partial_lookup partial v === None)
    | Scan_stable | Scan_conflict _ -> true} @@ total

val empty_unsatisfiable :
  (n : {n : int | 0 <= n}) ->
  (formula : {f : formula | valid_formula n f}) ->
  (entry : {e : proof_result |
    derivation_valid formula e.proof
    && same_clause (conclusion formula e.proof) e.clause
    && e.clause === []}) ->
  {u : unit | unsatisfiable n formula} @@ total

val semantic_unsat_at : (n : int) ->
  (formula : {f : formula | unsatisfiable n f}) ->
  (assignment : bool list) ->
  {u : unit | not (eval_formula assignment formula)} @@ total

val result_clause_valid : (n : int) -> (formula : formula) ->
  (entry : {e : proof_result |
    derivation_valid formula e.proof
    && same_clause (conclusion formula e.proof) e.clause}) ->
  {u : unit | if valid_formula n formula then valid_clause n entry.clause
    else true} @@ total

val scan_conflict_head : (partial : bool option list) ->
  (literal : literal) -> (rest : literal list) ->
  {u : unit | match scan_formula partial [literal :: rest] with
    | Scan_conflict _ ->
      (match literal with Positive v | Negative v ->
        not (partial_lookup partial v === None))
    | Scan_unit _ | Scan_stable -> true} @@ total

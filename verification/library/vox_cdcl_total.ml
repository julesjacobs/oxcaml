open Vox_sat

type reason : immutable_data mod total =
  | Decision
  | Original of int
  | Learned of int
[@@inductive]

type binding : immutable_data mod total = {
  value : bool;
  level : int;
  reason : reason;
}

type state : immutable_data mod total = {
  bindings : binding option list;
  trail : literal list;
  level : int;
  decisions : int;
  conflicts : int;
  learned : int;
  backjumps : int;
  steps : int;
}

type statistics : immutable_data mod total = {
  decisions : int;
  conflicts : int;
  learned : int;
  backjumps : int;
  steps : int;
}

type answer : immutable_data mod total =
  | Sat of bool list
  | Unsat of proof_result
  | Unknown
[@@inductive]

type report : immutable_data mod total = {
  answer : answer;
  statistics : statistics;
}

type input_error = Invalid_fuel | Invalid_input of Vox_sat.input_error

type propagation : immutable_data mod total =
  | Conflict of clause_source * state
  | Stable of state * bool option list
  | Propagation_exhausted
[@@inductive]

let rec (empty_bindings @ total) n =
  if n <= 0 then [] else None :: empty_bindings (n - 1)
[@@decreases n]

let rec (partial_of_bindings @ total) bindings =
  match bindings with
  | [] -> []
  | None :: rest -> None :: partial_of_bindings rest
  | Some binding :: rest -> Some binding.value :: partial_of_bindings rest

let rec (clauses_of_entries @ total) entries =
  match entries with
  | [] -> []
  | entry :: rest -> entry.clause :: clauses_of_entries rest

let (at @ total) (bindings : binding option list @ immutable total)
    (index : int) =
  Vox_sequence.at bindings (Bigint.of_int index)

let (set_at @ total) (bindings : binding option list @ immutable total)
    (index : int) (value : binding option @ immutable total) =
  Vox_sequence.set bindings (Bigint.of_int index) value

let (variable @ total) = function Positive v | Negative v -> v
let (wanted @ total) = function Positive _ -> true | Negative _ -> false

let (source_of_reason @ total) learned_count = function
  | Decision -> None
  | Original index -> Some (Original_clause index)
  | Learned ordinal -> Some (Learned_clause (learned_count - 1 - ordinal))

let (enqueue @ total) state literal reason =
  let v = variable literal in
  match at state.bindings v with
  | None -> None
  | Some (Some existing) ->
    if existing.value = wanted literal then Some state else None
  | Some None ->
    let binding = {value = wanted literal; level = state.level; reason} in
    Some {state with
      bindings = set_at state.bindings v (Some binding);
      trail = literal :: state.trail}

let rec (retain_bindings @ total) target
    (bindings : binding option list @ immutable total) =
  match bindings with
  | [] -> []
  | None :: rest -> None :: retain_bindings target rest
  | Some binding :: rest ->
    (if binding.level <= target then Some binding else None)
      :: retain_bindings target rest

let rec (retain_trail @ total) target
    (bindings : binding option list @ immutable total) trail =
  match trail with
  | [] -> []
  | literal :: rest ->
    let keep = match at bindings (variable literal) with
      | Some (Some binding) -> binding.level <= target
      | Some None | None -> false in
    if keep then literal :: retain_trail target bindings rest
    else retain_trail target bindings rest

let (backtrack @ total) state target =
  {state with
    bindings = retain_bindings target state.bindings;
    trail = retain_trail target state.bindings state.trail;
    level = target}

let rec (occurrences_clause @ total) target clause =
  match clause with
  | [] -> 0
  | literal :: rest ->
    (if variable literal = target then 1 else 0)
      + occurrences_clause target rest

let rec (occurrences_formula @ total) target formula =
  match formula with
  | [] -> 0
  | clause :: rest ->
    occurrences_clause target clause + occurrences_formula target rest

let rec (occurrence_scores @ total) formula n index =
  if n <= 0 then []
  else occurrences_formula index formula
       :: occurrence_scores formula (n - 1) (index + 1)
[@@decreases n]

let rec (choose_variable @ total) bindings (scores : int list)
    index selected (best : int) =
  match bindings, scores with
  | [], _ | _, [] -> selected
  | binding :: rest, score :: remaining ->
    match binding with
    | None when score > best ->
      choose_variable rest remaining (index + 1) (Some index) score
    | None | Some _ ->
      choose_variable rest remaining (index + 1) selected best

let rec (has_int @ total) (target : int) (values : int list) =
  match values with
  | [] -> false
  | first :: rest -> first = target || has_int target rest

let rec (current_variables @ total) clause
    (bindings : binding option list @ immutable total) level seen =
  match clause with
  | [] -> seen
  | literal :: rest ->
    let v = variable literal in
    match at bindings v with
    | Some (Some binding) when binding.level = level ->
      current_variables rest bindings level
        (if has_int v seen then seen else v :: seen)
    | Some (Some _) | Some None | None ->
      current_variables rest bindings level seen

let rec (find_latest @ total) trail variables =
  match trail with
  | [] -> None
  | literal :: rest ->
    let v = variable literal in
    if has_int v variables then Some v else find_latest rest variables

let rec (has_positive @ total) target clause =
  match clause with
  | [] -> false
  | Positive v :: rest -> v = target || has_positive target rest
  | Negative _ :: rest -> has_positive target rest

let rec (analyze @ total) :
    (formula : formula) ->
    (database : {d : proof_result list | database_valid formula d}) ->
    (state : state) -> (stop : int) -> (fuel : int) ->
    (current : {e : proof_result |
      derivation_valid formula e.proof
      && same_clause (conclusion formula e.proof) e.clause}) ->
    {r : proof_result option |
      match r with
      | None -> true
      | Some entry ->
        derivation_valid formula entry.proof
        && same_clause (conclusion formula entry.proof) entry.clause} =
  fun formula database state stop fuel current ->
  if fuel <= 0 then None
  else
    let partial = partial_of_bindings state.bindings in
    match scan_formula partial [current.clause] with
    | Scan_conflict _ ->
      let variables =
        current_variables current.clause state.bindings state.level [] in
      (match variables with
       | [] -> Some current
       | [_] when stop >= 1 -> Some current
       | _ ->
         match find_latest state.trail variables with
         | None -> None
         | Some v ->
           match at state.bindings v with
           | Some (Some binding) ->
             (match source_of_reason state.learned binding.reason with
              | None -> None
              | Some source ->
                match fetch_result formula database source with
                | None -> None
                | Some reason ->
                  let resolved =
                    if has_positive v current.clause then
                      resolve_result formula v current reason
                    else resolve_result formula v reason current in
                  analyze formula database state stop (fuel - 1) resolved)
           | Some None | None -> None)
    | Scan_stable | Scan_unit _ -> None
[@@decreases fuel]

let rec (asserting_clause @ total) clause
    (bindings : binding option list @ immutable total) level asserting target =
  match clause with
  | [] -> asserting, target
  | literal :: rest ->
    let v = variable literal in
    match at bindings v with
    | Some (Some binding) ->
      if binding.level = level then
        (match asserting with
         | None -> asserting_clause rest bindings level (Some literal) target
         | Some previous ->
           if variable previous = v then
             asserting_clause rest bindings level asserting target
           else None, target)
      else
        let target = if binding.level > target then binding.level else target in
        asserting_clause rest bindings level asserting target
    | Some None | None -> None, target

let rec (propagate @ total) :
    (formula : formula) ->
    (database : {d : proof_result list | database_valid formula d}) ->
    (state : state) -> (fuel : int) ->
    {r : propagation |
      match r with
      | Stable (_, partial) -> scan_formula partial formula === Scan_stable
      | Conflict _ | Propagation_exhausted -> true} =
  fun formula database state fuel ->
  if fuel <= 0 then Propagation_exhausted
  else
    let partial = partial_of_bindings state.bindings in
    match scan_formula partial formula with
    | Scan_conflict index -> Conflict (Original_clause index, state)
    | Scan_unit (index, literal) ->
      (match enqueue state literal (Original index) with
       | None -> Conflict (Original_clause index, state)
       | Some state -> propagate formula database state (fuel - 1))
    | Scan_stable ->
      match scan_formula partial (clauses_of_entries database) with
      | Scan_conflict index -> Conflict (Learned_clause index, state)
      | Scan_unit (index, literal) ->
        (match enqueue state literal (Learned (state.learned - 1 - index)) with
         | None -> Conflict (Learned_clause index, state)
         | Some state -> propagate formula database state (fuel - 1))
      | Scan_stable -> Stable (state, partial)
[@@decreases fuel]

let (statistics @ total) (state : state) = {
  decisions = state.decisions;
  conflicts = state.conflicts;
  learned = state.learned;
  backjumps = state.backjumps;
  steps = state.steps;
}

let rec (search @ total) :
    (formula : formula) ->
    (scores : int list) ->
    (database : {d : proof_result list | database_valid formula d}) ->
    (state : state) -> (fuel : int) ->
    {r : report |
      match r.answer with
      | Sat assignment -> eval_formula assignment formula
      | Unsat entry ->
        derivation_valid formula entry.proof
        && same_clause (conclusion formula entry.proof) entry.clause
        && entry.clause === []
      | Unknown -> true} =
  fun formula scores database state fuel ->
  if fuel <= 0 then {answer = Unknown; statistics = statistics state}
  else
    let state = {state with steps = state.steps + 1} in
    match propagate formula database state fuel with
    | Propagation_exhausted ->
      {answer = Unknown; statistics = statistics state}
    | Stable (state, partial) ->
      (match complete_partial partial with
       | Some assignment ->
         ghost_ (scan_formula_complete partial formula assignment);
         {answer = Sat assignment; statistics = statistics state}
       | None ->
         (match choose_variable state.bindings scores 0 None (-1) with
          | None -> {answer = Unknown; statistics = statistics state}
          | Some v ->
            let state = {state with
              level = state.level + 1;
              decisions = state.decisions + 1} in
            (match enqueue state (Positive v) Decision with
             | None -> {answer = Unknown; statistics = statistics state}
             | Some state -> search formula scores database state (fuel - 1))))
    | Conflict (source, state) ->
      let state = {state with conflicts = state.conflicts + 1} in
      (match fetch_result formula database source with
       | None -> {answer = Unknown; statistics = statistics state}
       | Some entry ->
         let stop = if state.level = 0 then 0 else 1 in
         (match analyze formula database state stop fuel entry with
          | None -> {answer = Unknown; statistics = statistics state}
          | Some learned ->
            if state.level = 0 then
              (match learned.clause with
               | [] -> {answer = Unsat learned; statistics = statistics state}
               | _ :: _ ->
                 {answer = Unknown; statistics = statistics state})
            else
              let asserting, target =
                asserting_clause learned.clause state.bindings
                  state.level None 0 in
              (match asserting with
               | None -> {answer = Unknown; statistics = statistics state}
               | Some literal ->
                 if target >= state.level then
                   {answer = Unknown; statistics = statistics state}
                 else
                   let old_level = state.level in
                   let state = backtrack state target in
                   let state = {state with
                     learned = state.learned + 1;
                     backjumps = state.backjumps +
                       (if target < old_level - 1 then 1 else 0)} in
                   let database = database_cons formula learned database in
                   (match enqueue state literal (Learned (state.learned - 1)) with
                    | None ->
                      {answer = Unknown; statistics = statistics state}
                    | Some state ->
                      search formula scores database state (fuel - 1)))))
[@@decreases fuel]

let (solve @ total) :
    (fuel : int) -> (n : int) -> (formula : formula) ->
    {r : (report, input_error) result |
      match r with
      | Error _ -> true
      | Ok report ->
        match report.answer with
        | Sat assignment -> check n formula assignment
        | Unsat entry ->
          derivation_valid formula entry.proof
          && same_clause (conclusion formula entry.proof) entry.clause
          && entry.clause === []
        | Unknown -> true} =
  fun fuel n formula ->
  if fuel < 0 then Error Invalid_fuel
  else if n < 0 || n > 256 then
    Error (Invalid_input Unsupported_variable_count)
  else if not (clauses_fit 4096 formula) then
    Error (Invalid_input Too_many_clauses)
  else if not (literals_fit 65536 formula) then
    Error (Invalid_input Too_many_literals)
  else if not (valid_formula n formula) then
    Error (Invalid_input Invalid_formula)
  else
    let initial = {
      bindings = empty_bindings n;
      trail = [];
      level = 0;
      decisions = 0;
      conflicts = 0;
      learned = 0;
      backjumps = 0;
      steps = 0;
    } in
    let scores = occurrence_scores formula n 0 in
    let report = search formula scores (database_empty formula) initial fuel in
    match report.answer with
    | Sat assignment ->
      if well_sized n assignment then (
        ghost_ (check_def n formula assignment);
        Ok report)
      else Ok {report with answer = Unknown}
    | Unsat _ | Unknown -> Ok report

let (unsat_at @ total) :
    (formula : formula) ->
    (report : {r : report |
      match r.answer with
      | Unsat entry ->
        derivation_valid formula entry.proof
        && same_clause (conclusion formula entry.proof) entry.clause
        && entry.clause === []
      | Sat _ | Unknown -> true}) ->
    (assignment : bool list) ->
    {u : unit |
      match report.answer with
      | Unsat _ -> not (eval_formula assignment formula)
      | Sat _ | Unknown -> true} =
  fun formula report assignment ->
  match report.answer with
  | Unsat entry -> empty_result_at formula entry assignment
  | Sat _ | Unknown -> ()

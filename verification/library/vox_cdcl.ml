open Vox_sat

type statistics : immutable_data mod total = {
  decisions : int;
  conflicts : int;
  learned : int;
  backjumps : int;
  work : int;
}

type answer : immutable_data mod total =
  Sat of bool list | Unsat of proof_result | Unknown [@@inductive]

type report : immutable_data mod total = {
  answer : answer;
  statistics : statistics;
}

type input_error = Invalid_fuel | Invalid_input of Vox_sat.input_error

type propagation_outcome =
  | Conflict of int
  | Stable of bool option list
[@@inductive]

exception Exhausted
exception Analysis_failure

let variable literal = literal lsr 1

let encode = function
  | Positive v -> v lsl 1
  | Negative v -> (v lsl 1) lor 1

let run fuel n formula :
    {r : report |
      match r.answer with
      | Unsat entry ->
        derivation_valid formula entry.proof
        && same_clause (conclusion formula entry.proof) entry.clause
        && entry.clause === []
      | Sat assignment -> eval_formula assignment formula
      | Unknown -> true} =
  let remaining = ref fuel in
  let spend () =
    if !remaining = 0 then raise Exhausted;
    decr remaining
  in
  let database = ref (Array.of_list
    (List.map (fun clause ->
      Array.of_list (List.map encode clause)) formula)) in
  let clause_count = ref (Array.length !database) in
  let original_count = !clause_count in
  let source_of_id id =
    if id < original_count then Original_clause id
    else Learned_clause (!clause_count - 1 - id)
  in
  let add_clause clause =
    let id = !clause_count in
    if id = Array.length !database then (
      let larger = Array.make (max 1 (2 * id)) [||] in
      Array.blit !database 0 larger 0 id;
      database := larger);
    (!database).(id) <- clause;
    incr clause_count;
    id
  in
  let values = Array.make n (-1) in
  let levels = Array.make n 0 in
  let reasons = Array.make n (-1) in
  let trail = Array.make n 0 in
  let trail_size = ref 0 in
  let trail_limits = Array.make (n + 1) 0 in
  let decision_level = ref 0 in
  let decisions = ref 0 in
  let conflicts = ref 0 in
  let learned = ref 0 in
  let backjumps = ref 0 in
  let learned_literals = ref 0 in
  let value literal =
    let assigned = values.(variable literal) in
    if assigned < 0 then -1
    else if literal land 1 = 0 then assigned
    else 1 - assigned
  in
  let enqueue literal reason =
    let v = variable literal in
    let wanted = if literal land 1 = 0 then 1 else 0 in
    if values.(v) >= 0 then values.(v) = wanted
    else (
      values.(v) <- wanted;
      levels.(v) <- !decision_level;
      reasons.(v) <- reason;
      trail.(!trail_size) <- literal;
      incr trail_size;
      true)
  in
  let rec propagate () :
      {outcome : propagation_outcome |
        match outcome with
        | Stable partial ->
          scan_formula partial formula === Scan_stable
        | Conflict _ -> true} =
    let partial = Array.to_list (Array.map (function
      | -1 -> None
      | 0 -> Some false
      | _ -> Some true) values) in
    spend ();
    match scan_formula partial formula with
    | Scan_conflict id -> Conflict id
    | Scan_unit (id, literal) ->
      if enqueue (encode literal) id then propagate ()
      else Conflict id
    | Scan_stable ->
      let changed = ref false in
      let conflict = ref None in
      let id = ref original_count in
      while !id < !clause_count && !conflict = None do
        spend ();
        let clause = (!database).(!id) in
        let satisfied = ref false in
        let unassigned = ref 0 in
        let last = ref 0 in
        Array.iter (fun literal ->
          let state = value literal in
          if state = 1 then satisfied := true
          else if state = -1 then (
            incr unassigned;
            last := literal)) clause;
        if not !satisfied then (
          if !unassigned = 0 then conflict := Some !id
          else if !unassigned = 1 then (
            if enqueue !last !id then changed := true
            else conflict := Some !id));
        incr id
      done;
      (match !conflict with
       | Some id -> Conflict id
       | None -> if !changed then propagate () else Stable partial)
  in
  let backtrack target_level =
    let target = trail_limits.(target_level + 1) in
    while !trail_size > target do
      decr trail_size;
      let v = variable trail.(!trail_size) in
      values.(v) <- -1;
      reasons.(v) <- -1
    done;
    decision_level := target_level
  in
  let analyze stop =
  let rec go
      (proof_database : {d : proof_result list |
        database_valid formula d})
      (current : {e : proof_result |
        derivation_valid formula e.proof
        && same_clause (conclusion formula e.proof) e.clause}) :
      {e : proof_result |
        derivation_valid formula e.proof
        && same_clause (conclusion formula e.proof) e.clause} =
    spend ();
    let current_variables = Array.make n false in
    let count = ref 0 in
    List.iter (fun literal ->
      let encoded = encode literal in
      if value encoded <> 0 then raise Analysis_failure;
      let v = variable encoded in
      if levels.(v) = !decision_level && not current_variables.(v) then (
        current_variables.(v) <- true;
        incr count)) current.clause;
    if !count <= stop then current
    else (
      let cursor = ref (!trail_size - 1) in
      while !cursor >= 0 &&
            not current_variables.(variable trail.(!cursor)) do
        decr cursor
      done;
      if !cursor < 0 then raise Analysis_failure;
      let v = variable trail.(!cursor) in
      let reason_id = reasons.(v) in
      if reason_id < 0 then raise Analysis_failure;
      let reason =
        match fetch_result formula proof_database
          (source_of_id reason_id) with
        | Some entry -> entry
        | None -> raise Analysis_failure
      in
      let current_positive = List.exists (function
        | Positive index -> index = v
        | Negative _ -> false) current.clause in
      let resolved =
        if current_positive then
          resolve_result formula v current reason
        else resolve_result formula v reason current
      in
      go proof_database resolved)
  in go
  in
  let positive_occurrences = Array.make n 0 in
  let negative_occurrences = Array.make n 0 in
  List.iter (List.iter (function
    | Positive v -> positive_occurrences.(v) <- positive_occurrences.(v) + 1
    | Negative v -> negative_occurrences.(v) <- negative_occurrences.(v) + 1))
    formula;
  let pick_variable () =
    let selected = ref (-1) in
    let best = ref (-1) in
    for v = 0 to n - 1 do
      let score = positive_occurrences.(v) + negative_occurrences.(v) in
      if values.(v) < 0 && score > !best then (
        selected := v;
        best := score)
    done;
    !selected
  in
  let statistics () = {
    decisions = !decisions;
    conflicts = !conflicts;
    learned = !learned;
    backjumps = !backjumps;
    work = fuel - !remaining;
  } in
  let rec loop
      (proof_database : {d : proof_result list |
        database_valid formula d}) :
      {r : report |
        match r.answer with
        | Unsat entry ->
          derivation_valid formula entry.proof
          && same_clause (conclusion formula entry.proof) entry.clause
          && entry.clause === []
        | Sat assignment -> eval_formula assignment formula
        | Unknown -> true} =
    match propagate () with
    | Conflict conflict ->
      incr conflicts;
      let initial =
          match fetch_result formula proof_database
            (source_of_id conflict) with
          | Some entry -> entry
          | None -> raise Analysis_failure
      in
      if !decision_level = 0 then (
        let empty = analyze 0 proof_database initial in
        match empty.clause with
        | [] -> {answer = Unsat empty; statistics = statistics ()}
        | _ :: _ -> raise Analysis_failure)
      else (
        let learned_entry =
          analyze 1 proof_database initial
        in
        let clause = Array.of_list
          (List.map encode learned_entry.clause) in
        let asserting = ref (-1) in
        let target_level = ref 0 in
        Array.iter (fun literal ->
          let v = variable literal in
          if value literal <> 0 then raise Analysis_failure;
          if levels.(v) = !decision_level then (
            if !asserting >= 0 && variable !asserting <> v then
              raise Analysis_failure;
            asserting := literal)
          else target_level := max !target_level levels.(v)) clause;
        if !asserting < 0 || !target_level >= !decision_level then
          raise Analysis_failure;
        if !learned >= 4096 ||
           !learned_literals + Array.length clause > 65536 then
          raise Exhausted;
        incr learned;
        learned_literals := !learned_literals + Array.length clause;
        let id = add_clause clause in
        if !target_level < !decision_level - 1 then incr backjumps;
        backtrack !target_level;
        if not (enqueue !asserting id) then
          raise Analysis_failure;
        loop (database_cons formula learned_entry proof_database))
    | Stable partial ->
      (match complete_partial partial with
       | Some assignment ->
         ghost_ (scan_formula_complete partial formula assignment);
         {answer = Sat assignment; statistics = statistics ()}
       | None ->
         let next = pick_variable () in
         if next < 0 then raise Analysis_failure;
         spend ();
         incr decision_level;
         trail_limits.(!decision_level) <- !trail_size;
         incr decisions;
         let literal =
           if positive_occurrences.(next) >= negative_occurrences.(next)
           then next lsl 1 else (next lsl 1) lor 1
         in
         if not (enqueue literal (-1)) then raise Analysis_failure;
         loop proof_database)
  in
  try loop (database_empty formula) with
  | Exhausted -> {answer = Unknown; statistics = statistics ()}
  | Analysis_failure ->
    {answer = Unknown; statistics = statistics ()}

let solve fuel n formula :
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
    let report = run fuel n formula in
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

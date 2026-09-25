open Vox_sat_spec

  let[@def] shift literal =
    match literal with
    | Positive index -> Positive (index - 1)
    | Negative index -> Negative (index - 1)

  let[@def] rec reduce_clause value clause =
    match clause with
    | [] -> Some []
    | literal :: rest ->
      (match literal with
       | Positive 0 -> if value then None else reduce_clause value rest
       | Negative 0 -> if value then reduce_clause value rest else None
       | _ ->
         match reduce_clause value rest with
         | None -> None
         | Some reduced -> Some (shift literal :: reduced))

  let[@def] rec reduce_formula value formula =
    match formula with
    | [] -> []
    | clause :: rest ->
      (match reduce_clause value clause with
       | None -> reduce_formula value rest
       | Some reduced -> reduced :: reduce_formula value rest)

  let rec (reduce_clause_correct @ total) :
      (value : bool) -> (clause : literal list) ->
      (assignment : bool list) ->
      {u : unit |
        match reduce_clause value clause with
        | None -> eval_clause (value :: assignment) clause
        | Some reduced ->
          eval_clause (value :: assignment) clause =
          eval_clause assignment reduced} =
    fun value clause assignment ->
    reduce_clause_def value clause;
    eval_clause_def (value :: assignment) clause;
    match clause with
    | [] ->
      eval_clause_def assignment [];
      ()
    | literal :: rest ->
      reduce_clause_correct value rest assignment;
      eval_literal_def (value :: assignment) literal;
      (match literal with
       | Positive index | Negative index ->
         lookup_def (value :: assignment) index);
      (match reduce_clause value rest with
       | None -> ()
       | Some reduced ->
         eval_clause_def assignment reduced;
         eval_clause_def assignment (shift literal :: reduced);
         shift_def literal;
         eval_literal_def assignment (shift literal));
      ()

  let rec (reduce_formula_correct @ total) :
      (value : bool) -> (formula : formula) ->
      (assignment : bool list) ->
      {u : unit |
        eval_formula (value :: assignment) formula =
        eval_formula assignment (reduce_formula value formula)} =
    fun value formula assignment ->
    reduce_formula_def value formula;
    eval_formula_def (value :: assignment) formula;
    match formula with
    | [] ->
      eval_formula_def assignment [];
      ()
    | clause :: rest ->
      reduce_clause_correct value clause assignment;
      reduce_formula_correct value rest assignment;
      (match reduce_clause value clause with
       | None -> ()
       | Some reduced ->
         eval_formula_def assignment
           (reduced :: reduce_formula value rest);
          ());
      ()

  let[@def] rec has_empty_clause formula =
    match formula with
    | [] -> false
    | clause :: rest ->
      (match clause with [] -> true | _ :: _ -> has_empty_clause rest)

  let rec (empty_conflict @ total) :
      (assignment : bool list) -> (formula : formula) ->
      {u : unit |
        if has_empty_clause formula then
          not (eval_formula assignment formula)
        else true} =
    fun assignment formula ->
    has_empty_clause_def formula;
    eval_formula_def assignment formula;
    match formula with
    | [] -> ()
    | clause :: rest ->
      (match clause with
       | [] -> eval_clause_def assignment []; ()
       | _ :: _ -> empty_conflict assignment rest)

  let[@def] rec unit_zero formula =
    match formula with
    | [] -> None
    | clause :: rest ->
      (match clause with
       | [Positive 0] -> Some true
       | [Negative 0] -> Some false
       | _ -> unit_zero rest)

  let rec (unit_zero_forces @ total) :
      (formula : formula) -> (value : bool) ->
      (assignment : bool list) ->
      {u : unit |
        match unit_zero formula with
        | None -> true
        | Some forced ->
          if eval_formula (value :: assignment) formula then
            value = forced
          else true} =
    fun formula value assignment ->
    unit_zero_def formula;
    eval_formula_def (value :: assignment) formula;
    match formula with
    | [] -> ()
    | clause :: rest ->
      (match clause with
       | [Positive 0] ->
         eval_clause_def (value :: assignment) clause;
         eval_clause_def (value :: assignment) [];
         eval_literal_def (value :: assignment) (Positive 0);
         lookup_def (value :: assignment) 0;
         ()
       | [Negative 0] ->
         eval_clause_def (value :: assignment) clause;
         eval_clause_def (value :: assignment) [];
         eval_literal_def (value :: assignment) (Negative 0);
         lookup_def (value :: assignment) 0;
         ()
       | _ -> unit_zero_forces rest value assignment)

  let[@def] same_literal left right =
    match left, right with
    | Positive x, Positive y | Negative x, Negative y -> x = y
    | Positive _, Negative _ | Negative _, Positive _ -> false

  let[@def] rec has_unit literal formula =
    match formula with
    | [] -> false
    | clause :: rest ->
      (match clause with
       | [candidate] -> same_literal candidate literal || has_unit literal rest
       | _ -> has_unit literal rest)

  let rec (has_unit_implies @ total) :
      (literal : literal) -> (formula : formula) ->
      (assignment : bool list) ->
      {u : unit |
        if has_unit literal formula
        && eval_formula assignment formula then
          eval_literal assignment literal
        else true} =
    fun literal formula assignment ->
    has_unit_def literal formula;
    eval_formula_def assignment formula;
    match formula with
    | [] -> ()
    | clause :: rest ->
      (match clause with
       | [candidate] ->
         if same_literal candidate literal then (
           same_literal_def candidate literal;
           eval_clause_def assignment clause;
           eval_clause_def assignment [];
           ())
         else has_unit_implies literal rest assignment
       | _ -> has_unit_implies literal rest assignment)

  let[@def] rec conflicting_units formula =
    match formula with
    | [] -> false
    | clause :: rest ->
      (match clause with
       | [Positive index] ->
         has_unit (Negative index) rest || conflicting_units rest
       | [Negative index] ->
         has_unit (Positive index) rest || conflicting_units rest
       | _ -> conflicting_units rest)

  let rec (conflicting_units_unsat @ total) :
      (formula : formula) -> (assignment : bool list) ->
      {u : unit |
        if conflicting_units formula then
          not (eval_formula assignment formula)
        else true} =
    fun formula assignment ->
    conflicting_units_def formula;
    eval_formula_def assignment formula;
    match formula with
    | [] -> ()
    | clause :: rest ->
      (match clause with
       | [Positive index] ->
         has_unit_implies (Negative index) rest assignment;
         if not (has_unit (Negative index) rest) then
           conflicting_units_unsat rest assignment;
         eval_clause_def assignment clause;
         eval_clause_def assignment [];
         eval_literal_def assignment (Positive index);
         eval_literal_def assignment (Negative index);
         ()
       | [Negative index] ->
         has_unit_implies (Positive index) rest assignment;
         if not (has_unit (Positive index) rest) then
           conflicting_units_unsat rest assignment;
         eval_clause_def assignment clause;
         eval_clause_def assignment [];
         eval_literal_def assignment (Positive index);
         eval_literal_def assignment (Negative index);
         ()
       | _ -> conflicting_units_unsat rest assignment)

  let[@def] rec refutes n formula =
    if has_empty_clause formula || conflicting_units formula then true
    else if n <= 0 then not (eval_formula [] formula)
    else match unit_zero formula with
      | Some value -> refutes (n - 1) (reduce_formula value formula)
      | None ->
        refutes (n - 1) (reduce_formula true formula)
        && refutes (n - 1) (reduce_formula false formula)
  [@@decreases n]

  let rec (refutes_sound @ total) :
      (n : int) -> (formula : formula) -> (assignment : bool list) ->
      {u : unit |
        if 0 <= n && well_sized n assignment && refutes n formula then
          not (eval_formula assignment formula)
        else true} =
    fun n formula assignment ->
    refutes_def n formula;
    well_sized_def n assignment;
    if has_empty_clause formula then
      empty_conflict assignment formula
    else if conflicting_units formula then
      conflicting_units_unsat formula assignment
    else if n <= 0 then (
      match assignment with
      | [] -> eval_formula_def [] formula; ()
      | _ :: _ -> ())
    else
      match assignment with
      | [] -> ()
      | value :: rest ->
        well_sized_def n assignment;
        reduce_formula_correct value formula rest;
        (match unit_zero formula with
         | Some forced ->
           unit_zero_forces formula value rest;
           (match value, forced with
            | true, true | false, false ->
              refutes_sound (n - 1) (reduce_formula forced formula) rest
            | true, false | false, true -> ())
         | None ->
           if value then
             refutes_sound (n - 1) (reduce_formula true formula) rest
           else
             refutes_sound (n - 1) (reduce_formula false formula) rest)
  [@@decreases n]

  let[@def] rec resize_assignment n assignment =
    if n <= 0 then []
    else match assignment with
      | [] -> false :: resize_assignment (n - 1) []
      | value :: rest -> value :: resize_assignment (n - 1) rest
  [@@decreases n]

  let rec (resize_assignment_size @ total) :
      (n : {n : int | 0 <= n}) -> (assignment : bool list) ->
      {u : unit | well_sized n (resize_assignment n assignment)} =
    fun n assignment ->
    resize_assignment_def n assignment;
    well_sized_def n (resize_assignment n assignment);
    if n = 0 then ()
    else match assignment with
      | [] -> resize_assignment_size (n - 1) []
      | _ :: rest -> resize_assignment_size (n - 1) rest
  [@@decreases n]

  let rec (resize_assignment_lookup @ total) :
      (n : {n : int | 0 <= n}) -> (assignment : bool list) ->
      (index : {i : int | 0 <= i && i < n}) ->
      {u : unit | lookup (resize_assignment n assignment) index ===
        lookup assignment index} =
    fun n assignment index ->
    resize_assignment_def n assignment;
    lookup_def (resize_assignment n assignment) index;
    lookup_def assignment index;
    match assignment with
    | [] ->
      if index > 0 then (
        resize_assignment_lookup (n - 1) [] (index - 1);
        lookup_def [] (index - 1));
      ()
    | _ :: rest ->
      if index > 0 then
        resize_assignment_lookup (n - 1) rest (index - 1);
      ()
  [@@decreases n]

  let rec (resize_assignment_clause @ total) :
      (n : {n : int | 0 <= n}) -> (assignment : bool list) ->
      (clause : literal list) ->
      {u : unit | if valid_clause n clause then
        eval_clause (resize_assignment n assignment) clause ===
        eval_clause assignment clause else true} =
    fun n assignment clause ->
    valid_clause_def n clause;
    eval_clause_def (resize_assignment n assignment) clause;
    eval_clause_def assignment clause;
    match clause with
    | [] -> ()
    | literal :: rest ->
      valid_literal_def n literal;
      if valid_clause n clause then (
        resize_assignment_clause n assignment rest;
        eval_literal_def (resize_assignment n assignment) literal;
        eval_literal_def assignment literal;
        match literal with
        | Positive index | Negative index ->
          resize_assignment_lookup n assignment index);
      ()

  let rec (resize_assignment_formula @ total) :
      (n : {n : int | 0 <= n}) -> (assignment : bool list) ->
      (formula : formula) ->
      {u : unit | if valid_formula n formula then
        eval_formula (resize_assignment n assignment) formula ===
        eval_formula assignment formula else true} =
    fun n assignment formula ->
    valid_formula_def n formula;
    eval_formula_def (resize_assignment n assignment) formula;
    eval_formula_def assignment formula;
    match formula with
    | [] -> ()
    | clause :: rest ->
      resize_assignment_clause n assignment clause;
      resize_assignment_formula n assignment rest

  let (refutes_sound_any @ total) :
      (n : {n : int | 0 <= n}) ->
      (formula : {f : formula | valid_formula n f && refutes n f}) ->
      (assignment : bool list) ->
      {u : unit | not (eval_formula assignment formula)} =
    fun n formula assignment ->
    resize_assignment_size n assignment;
    refutes_sound n formula (resize_assignment n assignment);
    resize_assignment_formula n assignment formula

  let[@def] opposite literal =
    match literal with
    | Positive index -> Negative index
    | Negative index -> Positive index

  let (opposite_eval @ total) :
      (literal : literal) -> (assignment : bool list) ->
      {u : unit |
        eval_literal assignment (opposite literal) =
        not (eval_literal assignment literal)} =
    fun literal assignment ->
    opposite_def literal;
    eval_literal_def assignment literal;
    eval_literal_def assignment (opposite literal);
    ()

  let[@def] rec negated_units clause =
    match clause with
    | [] -> []
    | literal :: rest -> [opposite literal] :: negated_units rest

  let rec (negated_units_if_false @ total) :
      (clause : literal list) -> (assignment : bool list) ->
      {u : unit |
        if not (eval_clause assignment clause) then
          eval_formula assignment (negated_units clause)
        else true} =
    fun clause assignment ->
    negated_units_def clause;
    eval_clause_def assignment clause;
    match clause with
    | [] -> eval_formula_def assignment []; ()
    | literal :: rest ->
      opposite_eval literal assignment;
      negated_units_if_false rest assignment;
      eval_formula_def assignment (negated_units clause);
      eval_clause_def assignment [opposite literal];
      eval_clause_def assignment [];
      ()

  let[@def] rec prepend_formula left right =
    match left with
    | [] -> right
    | clause :: rest -> clause :: prepend_formula rest right

  let rec (prepend_formula_eval @ total) :
      (left : formula) -> (right : formula) ->
      (assignment : bool list) ->
      {u : unit |
        eval_formula assignment (prepend_formula left right) =
        (eval_formula assignment left && eval_formula assignment right)} =
    fun left right assignment ->
    prepend_formula_def left right;
    eval_formula_def assignment left;
    match left with
    | [] -> ()
    | clause :: rest ->
      prepend_formula_eval rest right assignment;
      eval_formula_def assignment (prepend_formula left right);
      eval_formula_def assignment rest;
      ()

  let[@def] rec first_unit formula =
    match formula with
    | [] -> None
    | clause :: rest ->
      (match clause with
       | [literal] -> Some literal
       | _ -> first_unit rest)

  let rec (first_unit_forces @ total) :
      (formula : formula) -> (assignment : bool list) ->
      {u : unit |
        match first_unit formula with
        | Some literal ->
          if eval_formula assignment formula then
            eval_literal assignment literal
          else true
        | None -> true} =
    fun formula assignment ->
    first_unit_def formula;
    eval_formula_def assignment formula;
    match formula with
    | [] -> ()
    | clause :: rest ->
      (match clause with
       | [literal] ->
         eval_clause_def assignment clause;
         eval_clause_def assignment [];
         ()
       | _ -> first_unit_forces rest assignment)

  let[@def] rec reduce_clause_at index value clause =
    match clause with
    | [] -> Some []
    | literal :: rest ->
      let action =
        match literal with
        | Positive v when v = index -> if value then 1 else 0
        | Negative v when v = index -> if value then 0 else 1
        | _ -> 2
      in
      if action = 1 then None
      else
        match reduce_clause_at index value rest with
        | None -> None
        | Some reduced ->
          if action = 0 then Some reduced
          else Some (literal :: reduced)

  let[@def] rec reduce_formula_at index value formula =
    match formula with
    | [] -> []
    | clause :: rest ->
      match reduce_clause_at index value clause with
      | None -> reduce_formula_at index value rest
      | Some reduced -> reduced :: reduce_formula_at index value rest

  let rec (reduce_clause_at_correct @ total) :
      (index : int) -> (value : bool) ->
      (clause : literal list) -> (assignment : bool list) ->
      {u : unit |
        if lookup assignment index = value then
          match reduce_clause_at index value clause with
          | None -> eval_clause assignment clause
          | Some reduced ->
            eval_clause assignment clause = eval_clause assignment reduced
        else true} =
    fun index value clause assignment ->
    reduce_clause_at_def index value clause;
    eval_clause_def assignment clause;
    match clause with
    | [] -> eval_clause_def assignment []; ()
    | literal :: rest ->
      reduce_clause_at_correct index value rest assignment;
      eval_literal_def assignment literal;
      (match literal with
       | Positive v | Negative v -> lookup_def assignment v);
      (match reduce_clause_at index value rest with
       | None -> ()
       | Some reduced ->
         eval_clause_def assignment reduced;
         eval_clause_def assignment (literal :: reduced));
      ()

  let rec (reduce_formula_at_correct @ total) :
      (index : int) -> (value : bool) ->
      (formula : formula) -> (assignment : bool list) ->
      {u : unit |
        if lookup assignment index = value then
          eval_formula assignment formula =
          eval_formula assignment (reduce_formula_at index value formula)
        else true} =
    fun index value formula assignment ->
    reduce_formula_at_def index value formula;
    eval_formula_def assignment formula;
    match formula with
    | [] -> eval_formula_def assignment []; ()
    | clause :: rest ->
      reduce_clause_at_correct index value clause assignment;
      reduce_formula_at_correct index value rest assignment;
      (match reduce_clause_at index value clause with
       | None -> ()
       | Some reduced ->
         eval_formula_def assignment
           (reduced :: reduce_formula_at index value rest));
      ()

  let[@def] rec unit_refutes fuel formula =
    if has_empty_clause formula then true
    else if fuel <= 0 then false
    else
      match first_unit formula with
      | None -> false
      | Some literal ->
        (match literal with
         | Positive index ->
           unit_refutes (fuel - 1) (reduce_formula_at index true formula)
         | Negative index ->
           unit_refutes (fuel - 1) (reduce_formula_at index false formula))
  [@@decreases fuel]

  let rec (unit_refutes_sound @ total) :
      (fuel : int) -> (formula : formula) ->
      (assignment : bool list) ->
      {u : unit |
        if unit_refutes fuel formula then
          not (eval_formula assignment formula)
        else true} =
    fun fuel formula assignment ->
    unit_refutes_def fuel formula;
    if has_empty_clause formula then
      empty_conflict assignment formula
    else if fuel <= 0 then ()
    else
      match first_unit formula with
      | None -> ()
      | Some literal ->
        first_unit_forces formula assignment;
        eval_literal_def assignment literal;
        (match literal with
         | Positive index ->
           reduce_formula_at_correct index true formula assignment;
           unit_refutes_sound (fuel - 1)
             (reduce_formula_at index true formula) assignment
         | Negative index ->
           reduce_formula_at_correct index false formula assignment;
           unit_refutes_sound (fuel - 1)
             (reduce_formula_at index false formula) assignment)
  [@@decreases fuel]

  let[@def] rec check_unsat_trace n database trace =
    match trace with
    | [] -> false
    | clause :: rest ->
      if valid_clause n clause
         && unit_refutes n
              (prepend_formula database (negated_units clause))
      then
        match clause with
        | [] -> true
        | _ :: _ -> check_unsat_trace n (clause :: database) rest
      else false

  let rec (unsat_trace_sound @ total) :
      (n : int) -> (database : formula) ->
      (trace : formula) -> (assignment : bool list) ->
      {u : unit |
        if 0 <= n && well_sized n assignment
           && eval_formula assignment database
           && check_unsat_trace n database trace
        then false else true} =
    fun n database trace assignment ->
    check_unsat_trace_def n database trace;
    match trace with
    | [] -> ()
    | clause :: rest ->
      if valid_clause n clause
         && unit_refutes n
              (prepend_formula database (negated_units clause))
      then (
        unit_refutes_sound n
          (prepend_formula database (negated_units clause)) assignment;
        prepend_formula_eval database (negated_units clause) assignment;
        negated_units_if_false clause assignment;
        match clause with
        | [] -> eval_clause_def assignment []; ()
        | _ :: _ ->
          eval_formula_def assignment (clause :: database);
          unsat_trace_sound n (clause :: database) rest assignment)
      else ()

  let[@def] rec remove_positive index clause =
    match clause with
    | [] -> []
    | Positive v :: rest when v = index -> remove_positive index rest
    | literal :: rest -> literal :: remove_positive index rest

  let[@def] rec remove_negative index clause =
    match clause with
    | [] -> []
    | Negative v :: rest when v = index -> remove_negative index rest
    | literal :: rest -> literal :: remove_negative index rest

  let[@def] rec append_clause left right =
    match left with
    | [] -> right
    | literal :: rest -> literal :: append_clause rest right

  let[@def] rec has_literal literal clause =
    match clause with
    | [] -> false
    | candidate :: rest ->
      same_literal literal candidate || has_literal literal rest

  let[@def] rec dedup_clause clause =
    match clause with
    | [] -> []
    | literal :: rest ->
      let reduced = dedup_clause rest in
      if has_literal literal reduced then reduced
      else literal :: reduced

  let[@def] resolve_clause index positive negative =
    dedup_clause (append_clause
      (remove_positive index positive)
      (remove_negative index negative))

  let rec (append_clause_eval @ total) :
      (left : literal list) -> (right : literal list) ->
      (assignment : bool list) ->
      {u : unit |
        eval_clause assignment (append_clause left right) =
        (eval_clause assignment left || eval_clause assignment right)} =
    fun left right assignment ->
    append_clause_def left right;
    eval_clause_def assignment left;
    match left with
    | [] -> ()
    | literal :: rest ->
      append_clause_eval rest right assignment;
      eval_clause_def assignment (append_clause left right);
      eval_clause_def assignment rest;
      ()

  let rec (has_literal_sound @ total) :
      (literal : literal) -> (clause : literal list) ->
      (assignment : bool list) ->
      {u : unit |
        if has_literal literal clause
           && eval_literal assignment literal
        then eval_clause assignment clause
        else true} =
    fun literal clause assignment ->
    has_literal_def literal clause;
    eval_clause_def assignment clause;
    match clause with
    | [] -> ()
    | candidate :: rest ->
      same_literal_def literal candidate;
      eval_literal_def assignment literal;
      eval_literal_def assignment candidate;
      (match literal with
       | Positive v | Negative v -> lookup_def assignment v);
      (match candidate with
       | Positive v | Negative v -> lookup_def assignment v);
      has_literal_sound literal rest assignment

  let rec (dedup_clause_correct @ total) :
      (clause : literal list) -> (assignment : bool list) ->
      {u : unit |
        eval_clause assignment (dedup_clause clause) =
        eval_clause assignment clause} =
    fun clause assignment ->
    dedup_clause_def clause;
    eval_clause_def assignment clause;
    match clause with
    | [] -> eval_clause_def assignment []; ()
    | literal :: rest ->
      dedup_clause_correct rest assignment;
      has_literal_sound literal (dedup_clause rest) assignment;
      eval_clause_def assignment (dedup_clause rest);
      eval_clause_def assignment (literal :: dedup_clause rest);
      ()

  let rec (remove_positive_sound @ total) :
      (index : int) -> (clause : literal list) ->
      (assignment : bool list) ->
      {u : unit |
        if eval_clause assignment clause
           && not (eval_clause assignment (remove_positive index clause))
        then lookup assignment index
        else true} =
    fun index clause assignment ->
    remove_positive_def index clause;
    eval_clause_def assignment clause;
    match clause with
    | [] -> ()
    | literal :: rest ->
      remove_positive_sound index rest assignment;
      eval_literal_def assignment literal;
      (match literal with
       | Positive v | Negative v -> lookup_def assignment v);
      (match literal with
       | Positive v when v = index -> ()
       | _ ->
         eval_clause_def assignment
           (literal :: remove_positive index rest));
      ()

  let rec (remove_negative_sound @ total) :
      (index : int) -> (clause : literal list) ->
      (assignment : bool list) ->
      {u : unit |
        if eval_clause assignment clause
           && not (eval_clause assignment (remove_negative index clause))
        then not (lookup assignment index)
        else true} =
    fun index clause assignment ->
    remove_negative_def index clause;
    eval_clause_def assignment clause;
    match clause with
    | [] -> ()
    | literal :: rest ->
      remove_negative_sound index rest assignment;
      eval_literal_def assignment literal;
      (match literal with
       | Positive v | Negative v -> lookup_def assignment v);
      (match literal with
       | Negative v when v = index -> ()
       | _ ->
         eval_clause_def assignment
           (literal :: remove_negative index rest));
      ()

  let (resolution_sound @ total) :
      (index : int) -> (positive : literal list) ->
      (negative : literal list) -> (assignment : bool list) ->
      {u : unit |
        if eval_clause assignment positive
           && eval_clause assignment negative
        then eval_clause assignment
          (resolve_clause index positive negative)
        else true} =
    fun index positive negative assignment ->
    resolve_clause_def index positive negative;
    dedup_clause_correct
      (append_clause
        (remove_positive index positive)
        (remove_negative index negative)) assignment;
    append_clause_eval
      (remove_positive index positive)
      (remove_negative index negative) assignment;
    remove_positive_sound index positive assignment;
    remove_negative_sound index negative assignment;
    ()

  let[@def] rec same_clause left right =
    match left, right with
    | [], [] -> true
    | literal_left :: rest_left, literal_right :: rest_right ->
      same_literal literal_left literal_right
      && same_clause rest_left rest_right
    | [], _ :: _ | _ :: _, [] -> false

  let rec (same_clause_reflexive @ total) :
      (clause : literal list) ->
      {u : unit | same_clause clause clause} =
    fun clause ->
    same_clause_def clause clause;
    match clause with
    | [] -> ()
    | literal :: rest ->
      same_literal_def literal literal;
      same_clause_reflexive rest;
      ()

  let (same_literal_eval @ total) :
      (left : literal) -> (right : literal) ->
      (assignment : bool list) ->
      {u : unit |
        if same_literal left right then
          eval_literal assignment left = eval_literal assignment right
        else true} =
    fun left right assignment ->
    same_literal_def left right;
    eval_literal_def assignment left;
    eval_literal_def assignment right;
    ()

  let rec (same_clause_eval @ total) :
      (left : literal list) -> (right : literal list) ->
      (assignment : bool list) ->
      {u : unit |
        if same_clause left right then
          eval_clause assignment left = eval_clause assignment right
        else true} =
    fun left right assignment ->
    same_clause_def left right;
    eval_clause_def assignment left;
    eval_clause_def assignment right;
    match left, right with
    | literal_left :: rest_left, literal_right :: rest_right ->
      same_literal_eval literal_left literal_right assignment;
      same_clause_eval rest_left rest_right assignment
    | [], [] | [], _ :: _ | _ :: _, [] -> ()

  let[@def] identical_formula left right = ghost_ (left === right)

  let[@def] rec clause_at formula index =
    match formula with
    | [] -> None
    | clause :: rest ->
      if index = 0 then Some clause
      else clause_at rest (index - 1)

  let rec (clause_at_sound @ total) :
      (formula : formula) -> (index : int) ->
      (assignment : bool list) ->
      {u : unit |
        match clause_at formula index with
        | Some clause ->
          if eval_formula assignment formula then
            eval_clause assignment clause
          else true
        | None -> true} =
    fun formula index assignment ->
    clause_at_def formula index;
    eval_formula_def assignment formula;
    match formula with
    | [] -> ()
    | clause :: rest ->
      if index = 0 then ()
      else clause_at_sound rest (index - 1) assignment

  type derivation : immutable_data mod total =
    | Input of int
    | Exhaustion of int
    | Resolution of int * literal list * literal list
        * derivation * derivation
  [@@inductive]

  let[@def] rec conclusion formula derivation =
    match derivation with
    | Input index ->
      (match clause_at formula index with
       | Some clause -> clause
       | None -> [])
    | Exhaustion _ -> []
    | Resolution (index, left_clause, right_clause, _, _) ->
      resolve_clause index left_clause right_clause

  let[@def] rec derivation_valid formula derivation =
    match derivation with
    | Input index ->
      (match clause_at formula index with Some _ -> true | None -> false)
    | Exhaustion n ->
      0 <= n && valid_formula n formula && refutes n formula
    | Resolution (_, left_clause, right_clause, left, right) ->
      derivation_valid formula left
      && derivation_valid formula right
      && same_clause (conclusion formula left) left_clause
      && same_clause (conclusion formula right) right_clause

  let rec (derivation_sound @ total) :
      (formula : formula) -> (derivation : derivation) ->
      (assignment : bool list) ->
      {u : unit |
        if derivation_valid formula derivation
           && eval_formula assignment formula
        then eval_clause assignment (conclusion formula derivation)
        else true} =
    fun formula derivation assignment ->
    derivation_valid_def formula derivation;
    conclusion_def formula derivation;
    match derivation with
    | Input index -> clause_at_sound formula index assignment
    | Exhaustion n ->
      if derivation_valid formula derivation then
        refutes_sound_any n formula assignment;
      ()
    | Resolution (index, left_clause, right_clause, left, right) ->
      derivation_sound formula left assignment;
      derivation_sound formula right assignment;
      same_clause_eval (conclusion formula left) left_clause assignment;
      same_clause_eval (conclusion formula right) right_clause assignment;
      resolution_sound index
        left_clause right_clause assignment

  let (input_proof @ total) :
      (formula : formula) ->
      (index : {i : int |
        match clause_at formula i with Some _ -> true | None -> false}) ->
      {d : derivation |
        derivation_valid formula d
        && same_clause (conclusion formula d)
          (match clause_at formula index with
           | Some clause -> clause | None -> [])} @ ghost =
    fun formula index -> ghost_ (
      let derivation = Input index in
      derivation_valid_def formula derivation;
      conclusion_def formula derivation;
      (match clause_at formula index with
       | Some clause -> same_clause_reflexive clause
       | None -> ());
      refine_ derivation)

  let (resolve_proof @ total) :
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
          (resolve_clause index left_clause right_clause)} @ ghost =
    fun formula index left_clause right_clause left right -> ghost_ (
      let derivation =
        Resolution (index, left_clause, right_clause, left, right)
      in
      derivation_valid_def formula derivation;
      conclusion_def formula derivation;
      same_clause_reflexive
        (resolve_clause index left_clause right_clause);
      refine_ derivation)

  let (empty_proof_at @ total) :
      (formula : formula) ->
      (derivation : {d : derivation |
        derivation_valid formula d
        && same_clause (conclusion formula d) []}) @ ghost ->
      (assignment : bool list) ->
      {u : unit | not (eval_formula assignment formula)} =
    fun formula derivation assignment ->
    ghost_ (derivation_sound formula derivation assignment);
    ghost_ (same_clause_eval
      (conclusion formula derivation) [] assignment);
    eval_clause_def assignment [];
    ()

  let[@def] rec aligned formula database proofs =
    match database, proofs with
    | [], [] -> true
    | clause :: rest, proof :: proof_rest ->
      derivation_valid formula proof
      && same_clause (conclusion formula proof) clause
      && aligned formula rest proof_rest
    | [], _ :: _ | _ :: _, [] -> false

  let (aligned_cons @ total) :
      (formula : formula) ->
      (clause : literal list) ->
      (proof : {d : derivation |
        derivation_valid formula d
        && same_clause (conclusion formula d) clause}) @ ghost ->
      (database : formula) ->
      (proofs : {p : derivation list | aligned formula database p})
        @ ghost ->
      {u : unit |
        aligned formula (clause :: database) (proof :: proofs)} @ ghost =
    fun formula clause proof database proofs -> ghost_ (
      aligned_def formula (clause :: database) (proof :: proofs);
      refine_ ())

  let[@def] rec learned_clause_at database index =
    match database with
    | [] -> None
    | clause :: rest ->
      if index = 0 then Some clause
      else learned_clause_at rest (index - 1)

  let[@def] rec learned_proof_at proofs index =
    match proofs with
    | [] -> None
    | proof :: rest ->
      if index = 0 then Some proof
      else learned_proof_at rest (index - 1)

  let rec (aligned_at @ total) :
      (formula : formula) -> (database : formula) ->
      (proofs : derivation list) -> (index : int) ->
      {u : unit |
        if aligned formula database proofs then
          match learned_clause_at database index,
                learned_proof_at proofs index with
          | Some clause, Some proof ->
            derivation_valid formula proof
            && same_clause (conclusion formula proof) clause
          | _ -> true
        else true} =
    fun formula database proofs index ->
    aligned_def formula database proofs;
    learned_clause_at_def database index;
    learned_proof_at_def proofs index;
    match database, proofs with
    | clause :: rest, proof :: proof_rest ->
      if index = 0 then ()
      else aligned_at formula rest proof_rest (index - 1)
    | [], [] | [], _ :: _ | _ :: _, [] -> ()

  type proof_result = {
    clause : literal list;
    proof : derivation @@ ghost;
  }

  let (exhaustive_result @ total) :
      (n : {n : int | 0 <= n}) ->
      (formula : {f : formula | valid_formula n f && refutes n f}) ->
      {r : proof_result |
        derivation_valid formula r.proof
        && same_clause (conclusion formula r.proof) r.clause
        && r.clause === []} =
    fun n formula ->
    let proof : {d : derivation | derivation_valid formula d
      && same_clause (conclusion formula d) []} @ ghost = ghost_ (
      let proof = Exhaustion n in
      derivation_valid_def formula proof;
      conclusion_def formula proof;
      same_clause_reflexive [];
      refine_ proof) in
    {clause = []; proof}

  let (original_result @ total) :
      (formula : formula) -> (index : int) ->
      {r : proof_result option |
        match r with
        | None -> clause_at formula index === None
        | Some entry ->
          derivation_valid formula entry.proof
          && same_clause (conclusion formula entry.proof) entry.clause} =
    fun formula index ->
    match clause_at formula index with
    | None -> None
    | Some clause ->
      let proof = input_proof formula index in
      Some {clause; proof}

  let (resolve_result @ total) :
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
          (resolve_clause index left.clause right.clause)} =
    fun formula index left right ->
    let clause = resolve_clause index left.clause right.clause in
    let proof = resolve_proof formula index
      left.clause right.clause left.proof right.proof in
    ghost_ (same_clause_reflexive clause);
    {clause; proof}

  let (empty_result_at @ total) :
      (formula : formula) ->
      (entry : {e : proof_result |
        derivation_valid formula e.proof
        && same_clause (conclusion formula e.proof) e.clause
        && e.clause === []}) ->
      (assignment : bool list) ->
      {u : unit | not (eval_formula assignment formula)} =
    fun formula entry assignment ->
    ghost_ (empty_proof_at formula entry.proof assignment);
    ()

  let[@def] rec database_valid formula entries =
    match entries with
    | [] -> ghost_ true
    | entry :: rest -> ghost_ (
      derivation_valid formula entry.proof
      && same_clause (conclusion formula entry.proof) entry.clause
      && database_valid formula rest)

  let (database_empty @ total) :
      (formula : formula) ->
      {entries : proof_result list | database_valid formula entries && entries
        === []} =
    fun formula ->
    ghost_ (database_valid_def formula []);
    []

  let (database_cons @ total) :
      (formula : formula) ->
      (entry : {e : proof_result |
        derivation_valid formula e.proof
        && same_clause (conclusion formula e.proof) e.clause}) ->
      (entries : {es : proof_result list | database_valid formula es}) ->
      {result : proof_result list | database_valid formula result && result
        === entry :: entries} =
    fun formula entry entries ->
    ghost_ (database_valid_def formula (entry :: entries));
    entry :: entries

  let[@def] rec database_clauses entries =
    match entries with
    | [] -> []
    | entry :: rest -> entry.clause :: database_clauses rest

  let rec (database_at @ total) :
      (formula : formula) ->
      (entries : {es : proof_result list | database_valid formula es}) ->
      (index : int) ->
      {r : proof_result option |
        match r with
        | None -> clause_at (database_clauses entries) index === None
        | Some entry ->
          derivation_valid formula entry.proof
          && same_clause (conclusion formula entry.proof) entry.clause} =
    fun formula entries index ->
    ghost_ (database_valid_def formula entries);
    ghost_ (database_clauses_def entries);
    ghost_ (clause_at_def (database_clauses entries) index);
    match entries with
    | [] -> None
    | entry :: rest ->
      if index = 0 then Some entry
      else database_at formula rest (index - 1)

  type clause_source =
    | Original_clause of int
    | Learned_clause of int
  [@@inductive]

  type resolution_instruction = {
    pivot : int;
    source : clause_source;
    current_positive : bool;
  }

  let[@def] source_clause formula database source =
    match source with
    | Original_clause index -> clause_at formula index
    | Learned_clause index -> clause_at (database_clauses database) index

  let (fetch_result @ total) :
      (formula : formula) ->
      (database : {d : proof_result list | database_valid formula d}) ->
      (source : clause_source) ->
      {r : proof_result option |
        match r with
        | None -> source_clause formula database source === None
        | Some entry ->
          derivation_valid formula entry.proof
          && same_clause (conclusion formula entry.proof) entry.clause} =
    fun formula database source ->
    ghost_ (source_clause_def formula database source);
    match source with
    | Original_clause index -> original_result formula index
    | Learned_clause index -> database_at formula database index

  let rec (execute_resolution_steps @ total) :
      (formula : formula) ->
      (database : {d : proof_result list | database_valid formula d}) ->
      (current : {e : proof_result |
        derivation_valid formula e.proof
        && same_clause (conclusion formula e.proof) e.clause}) ->
      (steps : resolution_instruction list) ->
      {r : proof_result option |
        match r with
        | None -> true
        | Some entry ->
          derivation_valid formula entry.proof
          && same_clause (conclusion formula entry.proof) entry.clause} =
    fun formula database current steps ->
    match steps with
    | [] -> Some current
    | step :: rest ->
      (match fetch_result formula database step.source with
       | None -> None
       | Some reason ->
         let resolved =
           if step.current_positive then
             resolve_result formula step.pivot current reason
           else resolve_result formula step.pivot reason current
         in
         execute_resolution_steps formula database resolved rest)

  let (execute_resolution @ total) :
      (formula : formula) ->
      (database : {d : proof_result list | database_valid formula d}) ->
      (start : clause_source) ->
      (steps : resolution_instruction list) ->
      {r : proof_result option |
        match r with
        | None -> true
        | Some entry ->
          derivation_valid formula entry.proof
          && same_clause (conclusion formula entry.proof) entry.clause} =
    fun formula database start steps ->
    match fetch_result formula database start with
    | None -> None
    | Some first -> execute_resolution_steps formula database first steps

  type answer = Sat of bool list | Unsat | Unknown [@@inductive]
  type report = {answer : answer; fuel_left : int}

  let[@def] rec all_false n =
    if n <= 0 then [] else false :: all_false (n - 1)
  [@@decreases n]

  let rec (all_false_size @ total) :
      (n : {n : int | 0 <= n}) ->
      {u : unit | well_sized n (all_false n)} =
    fun n ->
    all_false_def n;
    well_sized_def n (all_false n);
    if n = 0 then ()
    else (
      all_false_size (n - 1);
      ())
  [@@decreases n]

  let rec (search @ total) :
      (fuel : {fuel : int | 0 <= fuel}) ->
      (n : {n : int | 0 <= n}) -> (formula : formula) ->
      {r : report |
        0 <= r.fuel_left && r.fuel_left <= fuel
        && match r.answer with
           | Sat assignment ->
             well_sized n assignment && eval_formula assignment formula
           | Unsat -> refutes n formula
           | Unknown -> true} =
    fun fuel n formula ->
    if fuel = 0 then {answer = Unknown; fuel_left = 0}
    else if has_empty_clause formula || conflicting_units formula then (
      ghost_ (refutes_def n formula);
      {answer = Unsat; fuel_left = fuel - 1})
    else if n = 0 then (
      let nil : bool list = [] in
      ghost_ (refutes_def n formula);
      ghost_ (well_sized_def n nil);
      if eval_formula nil formula then
        let remaining = fuel - 1 in
        let _ : {u : unit |
          0 <= remaining && remaining <= fuel} = () in
        let _ : {u : unit | n = 0} = () in
        let _ : {u : unit | well_sized n nil} = () in
        let _ : {u : unit | eval_formula nil formula} = () in
        {answer = Sat nil; fuel_left = remaining}
      else
        {answer = Unsat; fuel_left = fuel - 1})
    else (match unit_zero formula with
    | Some forced ->
      let reduced = reduce_formula forced formula in
      let result = search (fuel - 1) (n - 1) reduced in
      (match result.answer with
       | Sat assignment ->
         ghost_ (
           reduce_formula_correct forced formula assignment;
           well_sized_def n (forced :: assignment));
         {answer = Sat (forced :: assignment);
          fuel_left = result.fuel_left}
       | Unknown -> {answer = Unknown; fuel_left = result.fuel_left}
       | Unsat ->
         ghost_ (refutes_def n formula);
         {answer = Unsat; fuel_left = result.fuel_left})
    | None -> match formula with
    | [] ->
      let assignment = all_false n in
      ghost_ (all_false_size n);
      ghost_ (eval_formula_def assignment []);
      {answer = Sat assignment; fuel_left = fuel - 1}
    | _ :: _ ->
      let true_formula = reduce_formula true formula in
      let true_result = search (fuel - 1) (n - 1) true_formula in
      match true_result.answer with
      | Sat assignment ->
        ghost_ (
          reduce_formula_correct true formula assignment;
          well_sized_def n (true :: assignment));
        {answer = Sat (true :: assignment);
         fuel_left = true_result.fuel_left}
      | Unknown -> {answer = Unknown; fuel_left = true_result.fuel_left}
      | Unsat ->
        let false_formula = reduce_formula false formula in
        let false_result =
          search true_result.fuel_left (n - 1) false_formula
        in
        (match false_result.answer with
         | Sat assignment ->
           ghost_ (
             reduce_formula_correct false formula assignment;
             well_sized_def n (false :: assignment));
           {answer = Sat (false :: assignment);
            fuel_left = false_result.fuel_left}
         | Unknown ->
           {answer = Unknown; fuel_left = false_result.fuel_left}
         | Unsat ->
           ghost_ (refutes_def n formula);
           {answer = Unsat; fuel_left = false_result.fuel_left}))
  [@@decreases n]

  let rec (decide_depth @ total) :
      (fuel : {fuel : int | 0 <= fuel}) ->
      (n : {n : int | 0 <= n}) -> (formula : formula) ->
      {r : answer | match r with
        | Sat assignment ->
          well_sized n assignment && eval_formula assignment formula
        | Unsat -> refutes n formula
        | Unknown -> fuel <= n} =
    fun fuel n formula ->
    if fuel = 0 then Unknown
    else if has_empty_clause formula || conflicting_units formula then (
      ghost_ (refutes_def n formula);
      Unsat)
    else if n = 0 then (
      let nil : bool list = [] in
      ghost_ (refutes_def n formula);
      ghost_ (well_sized_def n nil);
      if eval_formula nil formula then Sat nil else Unsat)
    else match unit_zero formula with
    | Some forced ->
      (match decide_depth (fuel - 1) (n - 1)
          (reduce_formula forced formula) with
       | Sat assignment ->
         ghost_ (
           reduce_formula_correct forced formula assignment;
           well_sized_def n (forced :: assignment));
         Sat (forced :: assignment)
       | Unknown -> Unknown
       | Unsat ->
         ghost_ (refutes_def n formula);
         Unsat)
    | None -> match formula with
    | [] ->
      let assignment = all_false n in
      ghost_ (all_false_size n);
      ghost_ (eval_formula_def assignment []);
      Sat assignment
    | _ :: _ ->
      match decide_depth (fuel - 1) (n - 1)
          (reduce_formula true formula) with
      | Sat assignment ->
        ghost_ (
          reduce_formula_correct true formula assignment;
          well_sized_def n (true :: assignment));
        Sat (true :: assignment)
      | Unknown -> Unknown
      | Unsat ->
        (match decide_depth (fuel - 1) (n - 1)
            (reduce_formula false formula) with
         | Sat assignment ->
           ghost_ (
             reduce_formula_correct false formula assignment;
             well_sized_def n (false :: assignment));
           Sat (false :: assignment)
         | Unknown -> Unknown
         | Unsat ->
           ghost_ (refutes_def n formula);
           Unsat)
  [@@decreases n]

  type input_error = Vox_sat_spec.input_error =
  | Invalid_formula
  | Unsupported_variable_count
  | Too_many_clauses
  | Too_many_literals

  let (solve @ total) :
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
             | Unknown -> true} =
    fun fuel n formula ->
    if n < 0 || n > 256 then Error Unsupported_variable_count
    else if not (clauses_fit 4096 formula) then Error Too_many_clauses
    else if not (literals_fit 65536 formula) then Error Too_many_literals
    else if not (valid_formula n formula) then Error Invalid_formula
    else
      Ok (search fuel n formula)

  let (unsat_at @ total) :
      (n : {n : int | 0 <= n}) -> (formula : formula) ->
      (r : {r : report |
        match r.answer with Unsat -> refutes n formula | _ -> true}) ->
      (assignment : bool list) ->
      {u : unit |
        if well_sized n assignment then
          match r.answer with
          | Unsat -> not (eval_formula assignment formula)
          | Sat _ | Unknown -> true
        else true} =
    fun n formula r assignment ->
    match r.answer with
    | Unsat -> refutes_sound n formula assignment
    | Sat _ | Unknown -> ()

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

  let[@def] rec partial_lookup partial index =
    match partial with
    | [] -> Some false
    | value :: rest ->
      if index = 0 then value
      else partial_lookup rest (index - 1)

  let[@def] partial_literal partial literal =
    match literal with
    | Positive index -> partial_lookup partial index
    | Negative index ->
      match partial_lookup partial index with
      | None -> None
      | Some value -> Some (not value)

  let[@def] rec complete_partial partial =
    match partial with
    | [] -> Some []
    | None :: _ -> None
    | Some value :: rest ->
      match complete_partial rest with
      | None -> None
      | Some assignment -> Some (value :: assignment)

  let[@def] rec scan_clause partial clause =
    match clause with
    | [] -> Clause_conflict
    | literal :: rest ->
      match partial_literal partial literal with
      | Some true -> Clause_satisfied
      | Some false -> scan_clause partial rest
      | None ->
        match scan_clause partial rest with
        | Clause_satisfied -> Clause_satisfied
        | Clause_conflict -> Clause_unit literal
        | Clause_unit _ | Clause_open -> Clause_open

  let[@def] rec scan_formula_from partial index formula =
    match formula with
    | [] -> Scan_stable
    | clause :: rest ->
      match scan_clause partial clause with
      | Clause_conflict -> Scan_conflict index
      | Clause_unit literal -> Scan_unit (index, literal)
      | Clause_satisfied | Clause_open ->
        scan_formula_from partial (index + 1) rest

  let[@def] scan_formula partial formula =
    scan_formula_from partial 0 formula

  let rec (partial_lookup_complete @ total) :
      (partial : bool option list) -> (index : int) ->
      (assignment : bool list) ->
      {u : unit |
        if complete_partial partial === Some assignment then
          partial_lookup partial index === Some (lookup assignment index)
        else true} =
    fun partial index assignment ->
    complete_partial_def partial;
    partial_lookup_def partial index;
    lookup_def assignment index;
    match partial, assignment with
    | [], [] -> ()
    | None :: _, _ -> ()
    | Some value :: rest, first :: tail ->
      if index <> 0 then partial_lookup_complete rest (index - 1) tail;
      ()
    | [], _ :: _ | _ :: _, [] -> ()

  let (partial_literal_complete @ total) :
      (partial : bool option list) -> (literal : literal) ->
      (assignment : bool list) ->
      {u : unit |
        if complete_partial partial === Some assignment then
          partial_literal partial literal ===
            Some (eval_literal assignment literal)
        else true} =
    fun partial literal assignment ->
    partial_literal_def partial literal;
    eval_literal_def assignment literal;
    match literal with
    | Positive index | Negative index ->
      partial_lookup_complete partial index assignment

  let rec (scan_clause_complete @ total) :
      (partial : bool option list) -> (clause : literal list) ->
      (assignment : bool list) ->
      {u : unit |
        if complete_partial partial === Some assignment then
          match scan_clause partial clause with
          | Clause_satisfied -> eval_clause assignment clause
          | Clause_conflict -> not (eval_clause assignment clause)
          | Clause_unit _ | Clause_open -> false
        else true} =
    fun partial clause assignment ->
    scan_clause_def partial clause;
    eval_clause_def assignment clause;
    match clause with
    | [] -> ()
    | literal :: rest ->
      partial_literal_complete partial literal assignment;
      scan_clause_complete partial rest assignment;
      eval_literal_def assignment literal;
      ()

  let rec (scan_formula_from_complete @ total) :
      (partial : bool option list) -> (index : int) ->
      (formula : formula) -> (assignment : bool list) ->
      {u : unit |
        if complete_partial partial === Some assignment
           && scan_formula_from partial index formula === Scan_stable
        then eval_formula assignment formula
        else true} =
    fun partial index formula assignment ->
    scan_formula_from_def partial index formula;
    eval_formula_def assignment formula;
    match formula with
    | [] -> ()
    | clause :: rest ->
      scan_clause_complete partial clause assignment;
      scan_formula_from_complete partial (index + 1) rest assignment

  let (scan_formula_complete @ total) :
      (partial : bool option list) -> (formula : formula) ->
      (assignment : bool list) ->
      {u : unit |
        if complete_partial partial === Some assignment
           && scan_formula partial formula === Scan_stable
        then eval_formula assignment formula
        else true} =
    fun partial formula assignment ->
    scan_formula_def partial formula;
    scan_formula_from_complete partial 0 formula assignment

  let rec (scan_formula_from_source @ total) :
      (partial : bool option list) -> (start : int) -> (formula : formula) ->
      {u : unit | match scan_formula_from partial start formula with
        | Scan_stable -> true
        | Scan_conflict index | Scan_unit (index, _) ->
          not (clause_at formula (index - start) === None)} =
    fun partial start formula ->
    scan_formula_from_def partial start formula;
    match formula with
    | [] -> ()
    | _ :: rest ->
      scan_formula_from_source partial (start + 1) rest;
      match scan_formula_from partial start formula with
      | Scan_stable -> ()
      | Scan_conflict index | Scan_unit (index, _) ->
        clause_at_def formula (index - start)

  let (scan_formula_source @ total) :
      (partial : bool option list) -> (formula : formula) ->
      {u : unit | match scan_formula partial formula with
        | Scan_stable -> true
        | Scan_conflict index | Scan_unit (index, _) ->
          not (clause_at formula index === None)} =
    fun partial formula ->
    scan_formula_def partial formula;
    scan_formula_from_source partial 0 formula

  let rec (scan_clause_unit_unassigned @ total) :
      (partial : bool option list) -> (clause : literal list) ->
      {u : unit | match scan_clause partial clause with
        | Clause_unit literal -> partial_literal partial literal === None
        | Clause_satisfied | Clause_open | Clause_conflict -> true} =
    fun partial clause ->
    scan_clause_def partial clause;
    match clause with
    | [] -> ()
    | _ :: rest -> scan_clause_unit_unassigned partial rest

  let rec (scan_formula_from_unit_unassigned @ total) :
      (partial : bool option list) -> (index : int) -> (formula : formula) ->
      {u : unit | match scan_formula_from partial index formula with
        | Scan_unit (_, literal) -> partial_literal partial literal === None
        | Scan_stable | Scan_conflict _ -> true} =
    fun partial index formula ->
    scan_formula_from_def partial index formula;
    match formula with
    | [] -> ()
    | clause :: rest ->
      scan_clause_unit_unassigned partial clause;
      scan_formula_from_unit_unassigned partial (index + 1) rest

  let (scan_formula_unit_unassigned @ total) :
      (partial : bool option list) -> (formula : formula) ->
      {u : unit | match scan_formula partial formula with
        | Scan_unit (_, literal) ->
          (match literal with Positive v | Negative v ->
            partial_lookup partial v === None)
        | Scan_stable | Scan_conflict _ -> true} =
    fun partial formula ->
    scan_formula_def partial formula;
    scan_formula_from_unit_unassigned partial 0 formula;
    match scan_formula partial formula with
    | Scan_unit (_, literal) -> partial_literal_def partial literal
    | Scan_stable | Scan_conflict _ -> ()

let rec (empty_rejects_extensions @ total) :
    (formula : formula) ->
    (entry : {e : proof_result |
      derivation_valid formula e.proof
      && same_clause (conclusion formula e.proof) e.clause
      && e.clause === []}) ->
    (prefix : bool list) -> (remaining : int) ->
    {u : unit | rejects_extensions prefix remaining formula} =
  fun formula entry prefix remaining ->
  rejects_extensions_def prefix remaining formula;
  if remaining <= 0 then empty_result_at formula entry prefix
  else (
    empty_rejects_extensions formula entry (append_assignment prefix [false])
      (remaining - 1);
    empty_rejects_extensions formula entry (append_assignment prefix [true])
      (remaining - 1))
[@@decreases remaining]

let (empty_unsatisfiable @ total) :
    (n : {n : int | 0 <= n}) ->
    (formula : {f : formula | valid_formula n f}) ->
    (entry : {e : proof_result |
      derivation_valid formula e.proof
      && same_clause (conclusion formula e.proof) e.clause
      && e.clause === []}) ->
    {u : unit | unsatisfiable n formula} =
  fun n formula entry ->
  empty_rejects_extensions formula entry [] n;
  unsatisfiable_def n formula

let rec (append_assignment_nil @ total) :
    (values : bool list) ->
    {u : unit | append_assignment values [] === values} =
  fun values ->
  append_assignment_def values [];
  match values with
  | [] -> ()
  | _ :: rest -> append_assignment_nil rest

let rec (append_assignment_cons @ total) :
    (prefix : bool list) -> (value : bool) -> (rest : bool list) ->
    {u : unit |
      append_assignment (append_assignment prefix [value]) rest ===
      append_assignment prefix (value :: rest)} =
  fun prefix value rest ->
  append_assignment_def prefix [value];
  append_assignment_def prefix (value :: rest);
  match prefix with
  | [] ->
    append_assignment_def [value] rest;
    append_assignment_def [] rest
  | first :: tail ->
    append_assignment_def (first :: append_assignment tail [value]) rest;
    append_assignment_cons tail value rest

let rec (rejects_extensions_at @ total) :
    (prefix : bool list) -> (remaining : int) -> (formula : formula) ->
    (assignment : bool list) ->
    {u : unit |
      if 0 <= remaining && well_sized remaining assignment
         && rejects_extensions prefix remaining formula
      then not (eval_formula (append_assignment prefix assignment) formula)
      else true} =
  fun prefix remaining formula assignment ->
  rejects_extensions_def prefix remaining formula;
  well_sized_def remaining assignment;
  if remaining <= 0 then (
    match assignment with
    | [] -> append_assignment_nil prefix
    | _ :: _ -> ())
  else match assignment with
  | [] -> ()
  | value :: rest ->
    rejects_extensions_at (append_assignment prefix [value])
      (remaining - 1) formula rest;
    append_assignment_cons prefix value rest
[@@decreases remaining]

let (semantic_unsat_at @ total) :
    (n : int) ->
    (formula : {f : formula | unsatisfiable n f}) ->
    (assignment : bool list) ->
    {u : unit | not (eval_formula assignment formula)} =
  fun n formula assignment ->
  unsatisfiable_def n formula;
  resize_assignment_size n assignment;
  rejects_extensions_at [] n formula (resize_assignment n assignment);
  append_assignment_def [] (resize_assignment n assignment);
  resize_assignment_formula n assignment formula

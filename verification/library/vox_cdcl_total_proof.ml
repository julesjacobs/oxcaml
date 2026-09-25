open Vox_sat_spec
open Vox_sat_proof

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

type input_error = Invalid_fuel | Invalid_input of Vox_sat_proof.input_error

type propagation : immutable_data mod total =
  | Conflict of clause_source * state
  | Stable of state * bool option list
[@@inductive]

let[@def] rec all_unassigned (bindings : binding option list) =
  match bindings with
  | [] -> true
  | None :: rest -> all_unassigned rest
  | Some _ :: _ -> false

let rec (empty_bindings @ total) : (n : {n : int | 0 <= n}) ->
    {bs : binding option list |
      Vox_sequence.length bs === Bigint.of_int n && all_unassigned bs} =
  fun n ->
  if n = 0 then (
    let result : binding option list = [] in
    ghost_ (Vox_sequence.length_def result);
    ghost_ (all_unassigned_def result);
    result)
  else
    let result = None :: empty_bindings (n - 1) in
    ghost_ (Vox_sequence.length_def result);
    ghost_ (all_unassigned_def result);
    result
[@@decreases n]

let[@def] rec all_bound (bindings : binding option list) =
  match bindings with
  | [] -> true
  | None :: _ -> false
  | Some _ :: rest -> all_bound rest

let[@def] rec binding_values (bindings : binding option list) =
  match bindings with
  | [] -> []
  | None :: rest -> None :: binding_values rest
  | Some binding :: rest -> Some binding.value :: binding_values rest

let[@def] rec unassigned (bindings : binding option list) =
  match bindings with
  | [] -> 0Z
  | None :: rest -> Bigint.add 1Z (unassigned rest)
  | Some _ :: rest -> unassigned rest

let rec (unassigned_bounds @ total) : (bindings : binding option list) ->
    {u : unit | Bigint.compare (unassigned bindings) 0Z >= 0
      && Bigint.compare (unassigned bindings)
        (Vox_sequence.length bindings) <= 0} =
  fun bindings ->
  unassigned_def bindings;
  Vox_sequence.length_def bindings;
  match bindings with
  | [] -> ()
  | _ :: rest -> unassigned_bounds rest

let rec (lookup_unassigned @ total) :
    (bindings : {bs : binding option list |
      Bigint.compare (Vox_sequence.length bs) 256Z <= 0}) ->
    (index : int) ->
    {u : unit | if partial_lookup (binding_values bindings) index === None then
      0 <= index
      && Bigint.compare (Bigint.of_int index)
        (Vox_sequence.length bindings) < 0
      && Vox_sequence.at bindings (Bigint.of_int index) === Some None
      else true} =
  fun bindings index ->
  binding_values_def bindings;
  partial_lookup_def (binding_values bindings) index;
  Vox_sequence.at_def bindings (Bigint.of_int index);
  Vox_sequence.length_def bindings;
  match bindings with
  | [] -> ()
  | _ :: rest ->
    unassigned_bounds rest;
    if index <> 0 then lookup_unassigned rest (index - 1);
    ()

let rec (fill_unassigned @ total) :
    (bindings : binding option list) -> (index : Bigint.t) ->
    (binding : binding) ->
    {u : unit | if Vox_sequence.at bindings index === Some None then
      unassigned (Vox_sequence.set bindings index (Some binding)) ===
        Bigint.sub (unassigned bindings) 1Z else true} =
  fun bindings index binding ->
  Vox_sequence.at_def bindings index;
  Vox_sequence.set_def bindings index (Some binding);
  unassigned_def bindings;
  unassigned_def (Vox_sequence.set bindings index (Some binding));
  match bindings with
  | [] -> ()
  | _ :: rest ->
    if not (Bigint.equal index 0Z) then
      fill_unassigned rest (Bigint.sub index 1Z) binding;
    ()

let rec (partial_of_bindings @ total) : (bindings : binding option list) ->
    {partial : bool option list |
      Vox_sequence.length partial === Vox_sequence.length bindings
      && partial === binding_values bindings
      && (if all_bound bindings then
        match complete_partial partial with Some _ -> true | None -> false
        else true)} =
  fun bindings ->
  ghost_ (Vox_sequence.length_def bindings);
  ghost_ (all_bound_def bindings);
  ghost_ (binding_values_def bindings);
  match bindings with
  | [] ->
    let result : bool option list = [] in
    ghost_ (Vox_sequence.length_def result);
    ghost_ (complete_partial_def result);
    result
  | first :: rest ->
    let value = match first with
      | None -> None | Some binding -> Some binding.value in
    let result = value :: partial_of_bindings rest in
    ghost_ (Vox_sequence.length_def result);
    ghost_ (complete_partial_def result);
    result

let rec (complete_partial_length @ total) :
    (partial : bool option list) -> (assignment : bool list) ->
    {u : unit | if complete_partial partial === Some assignment then
      Vox_sequence.length assignment === Vox_sequence.length partial
      else true} =
  fun partial assignment ->
  complete_partial_def partial;
  Vox_sequence.length_def partial;
  Vox_sequence.length_def assignment;
  match partial, assignment with
  | Some _ :: rest, _ :: tail -> complete_partial_length rest tail
  | None :: _, _ | [], _ | _, [] -> ()

let rec (assignment_length @ total) : (n : int) ->
    (assignment : bool list) ->
    {u : unit |
      Bigint.compare (Vox_sequence.length assignment) 0Z >= 0
      && (if Vox_sequence.length assignment === Bigint.of_int n then
        well_sized n assignment else true)} =
  fun n assignment ->
  Vox_sequence.length_def assignment;
  well_sized_def n assignment;
  match assignment with
  | [] -> ()
  | _ :: rest -> assignment_length (n - 1) rest

let[@def] at (bindings : binding option list @ immutable total)
    (index : int) =
  Vox_sequence.at bindings (Bigint.of_int index)

let (set_at @ total) (bindings : binding option list @ immutable total)
    (index : int) (value : binding option @ immutable total) :
    {result : binding option list |
      Vox_sequence.length result === Vox_sequence.length bindings
      && result === Vox_sequence.set bindings (Bigint.of_int index) value
      && (match value with
        | None -> true
        | Some _ ->
          if Vox_sequence.at bindings (Bigint.of_int index) === Some None then
            unassigned result === Bigint.sub (unassigned bindings) 1Z
          else true)} =
  ghost_ (Vox_sequence.set_length bindings (Bigint.of_int index) value);
  ghost_ (match value with
    | None -> ()
    | Some binding -> fill_unassigned bindings (Bigint.of_int index) binding);
  Vox_sequence.set bindings (Bigint.of_int index) value

let[@def] variable literal =
  match literal with Positive v | Negative v -> v
let (wanted @ total) = function Positive _ -> true | Negative _ -> false

let[@def] rec trail_has (index : Bigint.t) trail =
  match trail with
  | [] -> false
  | literal :: rest ->
    Bigint.equal index (Bigint.of_int (variable literal)) || trail_has index
      rest

let[@def] rec covered_below (bindings : binding option list) trail
    (bound : Bigint.t) =
  if Bigint.compare bound 0Z <= 0 then true
  else
    let index = Bigint.sub bound 1Z in
    (match Vox_sequence.at bindings index with
     | Some (Some _) -> trail_has index trail
     | Some None | None -> true)
    && covered_below bindings trail index
[@@decreases bound]

let[@def] trail_covers bindings trail =
  covered_below bindings trail (Vox_sequence.length bindings)

let rec (binding_at_bounds @ total) : (bindings : binding option list) ->
    (index : Bigint.t) ->
    {u : unit | match Vox_sequence.at bindings index with
      | None -> true
      | Some _ -> Bigint.compare index 0Z >= 0
        && Bigint.compare index (Vox_sequence.length bindings) < 0} =
  fun bindings index ->
  Vox_sequence.at_def bindings index;
  Vox_sequence.length_def bindings;
  match bindings with
  | [] -> ()
  | _ :: rest ->
    unassigned_bounds rest;
    if not (Bigint.equal index 0Z) then
      binding_at_bounds rest (Bigint.sub index 1Z);
    ()

let rec (all_unassigned_at @ total) : (bindings : binding option list) ->
    (index : Bigint.t) ->
    {u : unit | if all_unassigned bindings then
      match Vox_sequence.at bindings index with
      | Some (Some _) -> false | Some None | None -> true
      else true} =
  fun bindings index ->
  all_unassigned_def bindings;
  Vox_sequence.at_def bindings index;
  match bindings with
  | [] -> ()
  | _ :: rest ->
    if not (Bigint.equal index 0Z) then
      all_unassigned_at rest (Bigint.sub index 1Z);
    ()

let rec (unassigned_covered @ total) : (bindings : binding option list) ->
    (trail : literal list) -> (bound : Bigint.t) ->
    {u : unit | if all_unassigned bindings then
      covered_below bindings trail bound else true} =
  fun bindings trail bound ->
  covered_below_def bindings trail bound;
  if Bigint.compare bound 0Z > 0 then (
    all_unassigned_at bindings (Bigint.sub bound 1Z);
    unassigned_covered bindings trail (Bigint.sub bound 1Z));
  ()
[@@decreases bound]

let rec (binding_at_set @ total) : (bindings : binding option list) ->
    (index : Bigint.t) -> (value : binding option) -> (query : Bigint.t) ->
    {u : unit | Vox_sequence.at (Vox_sequence.set bindings index value) query
      === (if Bigint.equal index query then
        match Vox_sequence.at bindings query with
        | None -> None | Some _ -> Some value
        else Vox_sequence.at bindings query)} =
  fun bindings index value query ->
  Vox_sequence.set_def bindings index value;
  Vox_sequence.at_def bindings query;
  Vox_sequence.at_def (Vox_sequence.set bindings index value) query;
  match bindings with
  | [] -> ()
  | _ :: rest ->
    if not (Bigint.equal index 0Z) && not (Bigint.equal query 0Z) then
      binding_at_set rest (Bigint.sub index 1Z) value (Bigint.sub query 1Z);
    ()

let rec (covered_set @ total) : (bindings : binding option list) ->
    (trail : literal list) -> (bound : Bigint.t) ->
    (literal : literal) -> (binding : binding) ->
    {u : unit | if covered_below bindings trail bound then
      covered_below
        (Vox_sequence.set bindings (Bigint.of_int (variable literal))
          (Some binding)) (literal :: trail) bound else true} =
  fun bindings trail bound literal binding ->
  let index = Bigint.of_int (variable literal) in
  covered_below_def bindings trail bound;
  covered_below_def (Vox_sequence.set bindings index (Some binding))
    (literal :: trail) bound;
  if Bigint.compare bound 0Z > 0 then (
    let query = Bigint.sub bound 1Z in
    binding_at_set bindings index (Some binding) query;
    trail_has_def query (literal :: trail);
    covered_set bindings trail query literal binding);
  ()
[@@decreases bound]

let rec (covered_member @ total) : (bindings : binding option list) ->
    (trail : literal list) -> (bound : Bigint.t) -> (index : Bigint.t) ->
    {u : unit | if covered_below bindings trail bound
        && Bigint.compare index 0Z >= 0 && Bigint.compare index bound < 0 then
      match Vox_sequence.at bindings index with
      | Some (Some _) -> trail_has index trail
      | Some None | None -> true
      else true} =
  fun bindings trail bound index ->
  covered_below_def bindings trail bound;
  if Bigint.compare bound 0Z > 0 then
    covered_member bindings trail (Bigint.sub bound 1Z) index;
  ()
[@@decreases bound]

let[@def] source_of_reason learned_count reason =
  match reason with
  | Decision -> None
  | Original index -> Some (Original_clause index)
  | Learned ordinal -> Some (Learned_clause (learned_count - 1 - ordinal))

let[@def] reason_source_valid formula database (learned : int) reason =
  match reason with
  | Decision -> true
  | Original index ->
    (match clause_at formula index with None -> false | Some _ -> true)
  | Learned ordinal ->
    0 <= ordinal && ordinal < learned
    && (match clause_at (database_clauses database)
      (learned - 1 - ordinal) with None -> false | Some _ -> true)

let[@def] rec reason_sources_valid formula database learned
    (bindings : binding option list) =
  match bindings with
  | [] -> true
  | None :: rest -> reason_sources_valid formula database learned rest
  | Some binding :: rest ->
    reason_source_valid formula database learned binding.reason
    && reason_sources_valid formula database learned rest

let rec (sequence_length_nonnegative @ total) :
    (values : ('a : immutable_data) list) @ immutable ->
    {u : unit | Bigint.compare (Vox_sequence.length values) 0Z >= 0} =
  fun values ->
  Vox_sequence.length_def values;
  match values with [] -> () | _ :: rest -> sequence_length_nonnegative rest

let rec (database_clauses_length @ total) : (database : proof_result list) ->
    {u : unit | Vox_sequence.length (database_clauses database) ===
      Vox_sequence.length database} =
  fun database ->
  database_clauses_def database;
  Vox_sequence.length_def database;
  Vox_sequence.length_def (database_clauses database);
  match database with [] -> () | _ :: rest -> database_clauses_length rest

let rec (clause_at_bounds @ total) : (bound : {n : int | 0 <= n}) ->
    (clauses : {f : formula |
      Bigint.compare (Vox_sequence.length f) (Bigint.of_int bound) <= 0}) ->
    (index : int) ->
    {u : unit | match clause_at clauses index with
      | None -> true
      | Some _ -> 0 <= index && index < bound} =
  fun bound clauses index ->
  clause_at_def clauses index;
  Vox_sequence.length_def clauses;
  match clauses with
  | [] -> ()
  | _ :: rest ->
    sequence_length_nonnegative rest;
    if index <> 0 then clause_at_bounds (bound - 1) rest (index - 1);
    ()

let rec (unassigned_reason_sources @ total) : (formula : formula) ->
    (database : proof_result list) -> (learned : int) ->
    (bindings : binding option list) ->
    {u : unit | if all_unassigned bindings then
      reason_sources_valid formula database learned bindings else true} =
  fun formula database learned bindings ->
  all_unassigned_def bindings;
  reason_sources_valid_def formula database learned bindings;
  match bindings with
  | [] -> ()
  | _ :: rest -> unassigned_reason_sources formula database learned rest

let rec (reason_sources_at @ total) : (formula : formula) ->
    (database : proof_result list) -> (learned : int) ->
    (bindings : binding option list) -> (index : Bigint.t) ->
    {u : unit | if reason_sources_valid formula database learned bindings then
      match Vox_sequence.at bindings index with
      | Some (Some binding) ->
        reason_source_valid formula database learned binding.reason
      | Some None | None -> true
      else true} =
  fun formula database learned bindings index ->
  reason_sources_valid_def formula database learned bindings;
  Vox_sequence.at_def bindings index;
  match bindings with
  | [] -> ()
  | _ :: rest ->
    if not (Bigint.equal index 0Z) then
      reason_sources_at formula database learned rest (Bigint.sub index 1Z);
    ()

let rec (reason_sources_set @ total) : (formula : formula) ->
    (database : proof_result list) -> (learned : int) ->
    (bindings : binding option list) -> (index : Bigint.t) ->
    (binding : binding) ->
    {u : unit | if reason_sources_valid formula database learned bindings
        && reason_source_valid formula database learned binding.reason then
      reason_sources_valid formula database learned
        (Vox_sequence.set bindings index (Some binding)) else true} =
  fun formula database learned bindings index binding ->
  reason_sources_valid_def formula database learned bindings;
  Vox_sequence.set_def bindings index (Some binding);
  reason_sources_valid_def formula database learned
    (Vox_sequence.set bindings index (Some binding));
  match bindings with
  | [] -> ()
  | _ :: rest ->
    if not (Bigint.equal index 0Z) then
      reason_sources_set formula database learned rest
        (Bigint.sub index 1Z) binding;
    ()

let (reason_source_prepend @ total) : (formula : formula) ->
    (database : proof_result list) -> (learned : int) -> (limit : int) ->
    (entry : proof_result) -> (reason : reason) ->
    {u : unit | if 0 <= learned && learned < limit
        && reason_source_valid formula database learned reason then
      reason_source_valid formula (entry :: database) (learned + 1) reason
      else true} =
  fun formula database learned limit entry reason ->
  reason_source_valid_def formula database learned reason;
  reason_source_valid_def formula (entry :: database) (learned + 1) reason;
  database_clauses_def (entry :: database);
  match reason with
  | Decision | Original _ -> ()
  | Learned ordinal ->
    clause_at_def (database_clauses (entry :: database))
      (learned + 1 - 1 - ordinal)

let rec (reason_sources_prepend @ total) : (formula : formula) ->
    (database : proof_result list) -> (learned : int) -> (limit : int) ->
    (entry : proof_result) -> (bindings : binding option list) ->
    {u : unit | if 0 <= learned && learned < limit
        && reason_sources_valid formula database learned bindings then
      reason_sources_valid formula (entry :: database) (learned + 1) bindings
      else true} =
  fun formula database learned limit entry bindings ->
  reason_sources_valid_def formula database learned bindings;
  reason_sources_valid_def formula (entry :: database) (learned + 1) bindings;
  match bindings with
  | [] -> ()
  | first :: rest ->
    (match first with
     | None -> ()
     | Some binding ->
       reason_source_prepend formula database learned limit entry
         binding.reason);
    reason_sources_prepend formula database learned limit entry rest

let (reason_source_exists @ total) : (formula : formula) ->
    (database : proof_result list) -> (learned : int) -> (reason : reason) ->
    {u : unit | if reason_source_valid formula database learned reason then
      match source_of_reason learned reason with
      | None -> true
      | Some source -> not (source_clause formula database source === None)
      else true} =
  fun formula database learned reason ->
  reason_source_valid_def formula database learned reason;
  source_of_reason_def learned reason;
  match reason with
  | Decision -> ()
  | Original index ->
    source_clause_def formula database (Original_clause index)
  | Learned ordinal ->
    source_clause_def formula database (Learned_clause (learned - 1 - ordinal))

let[@def] rec levels_bounded (upper : int) (bindings : binding option list) =
  match bindings with
  | [] -> true
  | None :: rest -> levels_bounded upper rest
  | Some binding :: rest ->
    0 <= binding.level && binding.level <= upper && levels_bounded upper rest

let rec (unassigned_levels @ total) : (bindings : binding option list) ->
    (upper : int) ->
    {u : unit | if all_unassigned bindings then levels_bounded upper bindings
      else true} =
  fun bindings upper ->
  all_unassigned_def bindings;
  levels_bounded_def upper bindings;
  match bindings with [] -> () | _ :: rest -> unassigned_levels rest upper

let rec (levels_at @ total) : (bindings : binding option list) ->
    (upper : int) -> (index : Bigint.t) ->
    {u : unit | if levels_bounded upper bindings then
      match Vox_sequence.at bindings index with
      | Some (Some binding) -> 0 <= binding.level && binding.level <= upper
      | Some None | None -> true
      else true} =
  fun bindings upper index ->
  levels_bounded_def upper bindings;
  Vox_sequence.at_def bindings index;
  match bindings with
  | [] -> ()
  | _ :: rest ->
    if not (Bigint.equal index 0Z) then
      levels_at rest upper (Bigint.sub index 1Z);
    ()

let rec (levels_set @ total) : (bindings : binding option list) ->
    (upper : int) -> (index : Bigint.t) -> (binding : binding) ->
    {u : unit | if levels_bounded upper bindings
        && 0 <= binding.level && binding.level <= upper then
      levels_bounded upper (Vox_sequence.set bindings index (Some binding))
      else true} =
  fun bindings upper index binding ->
  levels_bounded_def upper bindings;
  Vox_sequence.set_def bindings index (Some binding);
  levels_bounded_def upper (Vox_sequence.set bindings index (Some binding));
  match bindings with
  | [] -> ()
  | _ :: rest ->
    if not (Bigint.equal index 0Z) then
      levels_set rest upper (Bigint.sub index 1Z) binding;
    ()

let rec (levels_weaken @ total) : (bindings : binding option list) ->
    (upper : int) -> (next : int) ->
    {u : unit | if levels_bounded upper bindings && upper <= next then
      levels_bounded next bindings else true} =
  fun bindings upper next ->
  levels_bounded_def upper bindings;
  levels_bounded_def next bindings;
  match bindings with [] -> () | _ :: rest -> levels_weaken rest upper next

let (enqueue @ total) (formula : formula @ ghost)
    (database : proof_result list @ ghost) (state : state) literal reason :
    {result : state option | match result with
      | None -> not (at state.bindings (variable literal) === Some None)
      | Some next -> next.learned = state.learned && next.level = state.level
        && (if 0 <= state.level && levels_bounded state.level state.bindings
            then levels_bounded next.level next.bindings else true)
        && Vox_sequence.length next.bindings ===
          Vox_sequence.length state.bindings
        && (if at state.bindings (variable literal) === Some None then
          unassigned next.bindings ===
            Bigint.sub (unassigned state.bindings) 1Z else true)
        && (if trail_covers state.bindings state.trail then
          trail_covers next.bindings next.trail else true)
        && (if reason_sources_valid formula database state.learned
          state.bindings
            && reason_source_valid formula database state.learned reason then
          reason_sources_valid formula database state.learned next.bindings
          else true)} =
  let v = variable literal in
  ghost_ (at_def state.bindings v);
  match at state.bindings v with
  | None -> None
  | Some (Some existing) ->
    if existing.value = wanted literal then Some state else None
  | Some None ->
    let binding = {value = wanted literal; level = state.level; reason} in
    let bindings = set_at state.bindings v (Some binding) in
    ghost_ (
      levels_set state.bindings state.level (Bigint.of_int v) binding;
      reason_sources_set formula database state.learned state.bindings
        (Bigint.of_int v) binding;
      trail_covers_def state.bindings state.trail;
      covered_set state.bindings state.trail
        (Vox_sequence.length state.bindings) literal binding;
      trail_covers_def bindings (literal :: state.trail));
    Some {state with bindings; trail = literal :: state.trail}

let[@def] rec retained_bindings target (bindings : binding option list) =
  match bindings with
  | [] -> []
  | first :: rest ->
    (match first with
     | Some binding when binding.level <= target -> Some binding
     | Some _ | None -> None) :: retained_bindings target rest

let rec (retain_bindings @ total) : (target : int) ->
    (bindings : binding option list) ->
    {result : binding option list |
      Vox_sequence.length result === Vox_sequence.length bindings
      && result === retained_bindings target bindings} =
  fun target bindings ->
  ghost_ (retained_bindings_def target bindings);
  ghost_ (Vox_sequence.length_def bindings);
  match bindings with
  | [] -> []
  | first :: rest ->
    let value = match first with
      | Some binding when binding.level <= target -> Some binding
      | Some _ | None -> None in
    let result = value :: retain_bindings target rest in
    ghost_ (Vox_sequence.length_def result);
    result

let[@def] rec retain_trail target
    (bindings : binding option list @ immutable total) trail =
  match trail with
  | [] -> []
  | literal :: rest ->
    let keep = match at bindings (variable literal) with
      | Some (Some binding) -> binding.level <= target
      | Some None | None -> false in
    if keep then literal :: retain_trail target bindings rest
    else retain_trail target bindings rest

let rec (retained_binding_at @ total) : (target : int) ->
    (bindings : binding option list) -> (index : Bigint.t) ->
    {u : unit | Vox_sequence.at (retained_bindings target bindings) index ===
      (match Vox_sequence.at bindings index with
       | None -> None
       | Some None -> Some None
       | Some (Some binding) ->
         if binding.level <= target then Some (Some binding) else Some None)} =
  fun target bindings index ->
  retained_bindings_def target bindings;
  Vox_sequence.at_def bindings index;
  Vox_sequence.at_def (retained_bindings target bindings) index;
  match bindings with
  | [] -> ()
  | _ :: rest ->
    if not (Bigint.equal index 0Z) then
      retained_binding_at target rest (Bigint.sub index 1Z);
    ()

let rec (retained_trail_member @ total) : (target : int) ->
    (bindings : binding option list) -> (trail : literal list) ->
    (index : Bigint.t) ->
    {u : unit | if trail_has index trail then
      match Vox_sequence.at bindings index with
      | Some (Some binding) ->
        if binding.level <= target then
          trail_has index (retain_trail target bindings trail) else true
      | Some None | None -> true
      else true} =
  fun target bindings trail index ->
  trail_has_def index trail;
  retain_trail_def target bindings trail;
  trail_has_def index (retain_trail target bindings trail);
  match trail with
  | [] -> ()
  | literal :: rest ->
    at_def bindings (variable literal);
    retained_trail_member target bindings rest index

let rec (covered_retained @ total) : (target : int) ->
    (bindings : binding option list) -> (trail : literal list) ->
    (bound : Bigint.t) ->
    {u : unit | if covered_below bindings trail bound then
      covered_below (retained_bindings target bindings)
        (retain_trail target bindings trail) bound else true} =
  fun target bindings trail bound ->
  covered_below_def bindings trail bound;
  covered_below_def (retained_bindings target bindings)
    (retain_trail target bindings trail) bound;
  if Bigint.compare bound 0Z > 0 then (
    let index = Bigint.sub bound 1Z in
    retained_binding_at target bindings index;
    retained_trail_member target bindings trail index;
    covered_retained target bindings trail index);
  ()
[@@decreases bound]

let rec (retained_reason_sources @ total) : (formula : formula) ->
    (database : proof_result list) -> (learned : int) -> (target : int) ->
    (bindings : binding option list) ->
    {u : unit | if reason_sources_valid formula database learned bindings then
      reason_sources_valid formula database learned
        (retained_bindings target bindings) else true} =
  fun formula database learned target bindings ->
  reason_sources_valid_def formula database learned bindings;
  retained_bindings_def target bindings;
  reason_sources_valid_def formula database learned
    (retained_bindings target bindings);
  match bindings with
  | [] -> ()
  | _ :: rest -> retained_reason_sources formula database learned target rest

let rec (retained_levels @ total) : (bindings : binding option list) ->
    (upper : int) -> (target : int) ->
    {u : unit | if levels_bounded upper bindings then
      levels_bounded target (retained_bindings target bindings) else true} =
  fun bindings upper target ->
  levels_bounded_def upper bindings;
  retained_bindings_def target bindings;
  levels_bounded_def target (retained_bindings target bindings);
  match bindings with [] -> () | _ :: rest -> retained_levels rest upper target

let (backtrack @ total) (formula : formula @ ghost)
    (database : proof_result list @ ghost) (state : state) target :
    {next : state | next.learned = state.learned && next.level = target
      && next.bindings === retained_bindings target state.bindings
      && (if levels_bounded state.level state.bindings then
        levels_bounded target next.bindings else true)
      && Vox_sequence.length next.bindings ===
      Vox_sequence.length state.bindings
      && (if trail_covers state.bindings state.trail then
        trail_covers next.bindings next.trail else true)
      && (if reason_sources_valid formula database state.learned state.bindings
          then reason_sources_valid formula database state.learned next.bindings
          else true)} =
  let bindings = retain_bindings target state.bindings in
  let trail = retain_trail target state.bindings state.trail in
  ghost_ (
    retained_levels state.bindings state.level target;
    retained_reason_sources formula database state.learned target
      state.bindings;
    trail_covers_def state.bindings state.trail;
    covered_retained target state.bindings state.trail
      (Vox_sequence.length state.bindings);
    trail_covers_def bindings trail);
  {state with bindings; trail; level = target}

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

let rec (choose_variable @ total) :
    (bindings : {bs : binding option list |
      Bigint.compare (Vox_sequence.length bs) 256Z <= 0}) ->
    (scores : int list) ->
    {choice : (int * int) option | match choice with
      | None -> all_bound bindings
      | Some (v, _) -> 0 <= v
        && Bigint.compare (Bigint.of_int v)
          (Vox_sequence.length bindings) < 0
        && Vox_sequence.at bindings (Bigint.of_int v) === Some None} =
  fun bindings scores ->
  ghost_ (all_bound_def bindings);
  ghost_ (Vox_sequence.length_def bindings);
  match bindings with
  | [] -> None
  | binding :: rest ->
    let score, remaining = match scores with
      | [] -> 0, []
      | first :: rest -> first, rest in
    let choice = choose_variable rest remaining in
    let selected = match binding, choice with
      | Some _, None -> None
      | None, None -> Some (0, score)
      | None, Some (_, best) when score >= best -> Some (0, score)
      | _, Some (v, best) -> Some (v + 1, best) in
    ghost_ (match selected with
      | None -> ()
      | Some (v, _) -> Vox_sequence.at_def bindings (Bigint.of_int v));
    selected

let[@def] rec has_int (target : int) (values : int list) =
  match values with
  | [] -> false
  | first :: rest -> first = target || has_int target rest

let[@def] rec all_current variables (bindings : binding option list)
    (level : int) =
  match variables with
  | [] -> true
  | v :: rest ->
    match at bindings v with
    | Some (Some binding) ->
      binding.level = level && all_current rest bindings level
    | Some None | None -> false

let rec (all_current_member @ total) : (variables : int list) ->
    (bindings : binding option list) -> (level : int) -> (v : int) ->
    {u : unit | if all_current variables bindings level
        && has_int v variables then
      match at bindings v with
      | Some (Some binding) -> binding.level = level
      | Some None | None -> false
      else true} =
  fun variables bindings level v ->
  all_current_def variables bindings level;
  has_int_def v variables;
  match variables with
  | [] -> ()
  | first :: rest ->
    if first <> v then all_current_member rest bindings level v;
    ()

let rec (current_variables @ total) : (clause : literal list) ->
    (bindings : binding option list) -> (level : int) -> (seen : int list) ->
    {variables : int list | if all_current seen bindings level then
      all_current variables bindings level else true} =
  fun clause bindings level seen ->
  match clause with
  | [] -> seen
  | literal :: rest ->
    let v = variable literal in
    match at bindings v with
    | Some (Some binding) when binding.level = level ->
      ghost_ (all_current_def (v :: seen) bindings level);
      current_variables rest bindings level
        (if has_int v seen then seen else v :: seen)
    | Some (Some _) | Some None | None ->
      current_variables rest bindings level seen

let[@def] rec trail_avoids trail variables =
  match trail with
  | [] -> true
  | literal :: rest ->
    not (has_int (variable literal) variables) && trail_avoids rest variables

let rec (trail_hit_not_avoided @ total) : (trail : literal list) ->
    (variables : int list) -> (v : int) ->
    {u : unit | if trail_has (Bigint.of_int v) trail && has_int v variables then
      not (trail_avoids trail variables) else true} =
  fun trail variables v ->
  trail_has_def (Bigint.of_int v) trail;
  trail_avoids_def trail variables;
  match trail with
  | [] -> ()
  | _ :: rest -> trail_hit_not_avoided rest variables v

let rec (find_latest @ total) : (trail : literal list) ->
    (variables : int list) ->
    {found : int option | match found with
      | None -> trail_avoids trail variables
      | Some v -> has_int v variables} =
  fun trail variables ->
  ghost_ (trail_avoids_def trail variables);
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
    (state : {s : state | trail_covers s.bindings s.trail
      && reason_sources_valid formula database s.learned s.bindings}) ->
    (stop : int) -> (fuel : int) ->
    (current : {e : proof_result |
      derivation_valid formula e.proof
      && same_clause (conclusion formula e.proof) e.clause}) ->
    {r : proof_result option |
      match r with
      | None -> true
      | Some entry ->
        derivation_valid formula entry.proof
        && same_clause (conclusion formula entry.proof) entry.clause
        && (match scan_formula (partial_of_bindings state.bindings)
            [entry.clause] with
          | Scan_conflict _ -> true
          | Scan_stable | Scan_unit _ -> false)
        && (match current_variables entry.clause
            state.bindings state.level [] with
          | [] -> true
          | [_] -> 1 <= stop
          | _ :: _ :: _ -> false)} =
  fun formula database state stop fuel current ->
  if fuel <= 0 then None
  else
    let partial = partial_of_bindings state.bindings in
    match scan_formula partial [current.clause] with
    | Scan_conflict _ ->
      ghost_ (all_current_def [] state.bindings state.level);
      let variables =
        current_variables current.clause state.bindings state.level [] in
      (match variables with
       | [] -> Some current
       | [_] when stop >= 1 -> Some current
       | first :: _ ->
         ghost_ (
           has_int_def first variables;
           all_current_member variables state.bindings state.level first;
           at_def state.bindings first;
           binding_at_bounds state.bindings (Bigint.of_int first);
           trail_covers_def state.bindings state.trail;
           covered_member state.bindings state.trail
             (Vox_sequence.length state.bindings) (Bigint.of_int first);
           trail_hit_not_avoided state.trail variables first);
         match find_latest state.trail variables with
         | None ->
           let _ : {u : unit | false} = () in
           None
         | Some v ->
           ghost_ (all_current_member variables state.bindings state.level v);
           match at state.bindings v with
           | Some (Some binding) ->
             ghost_ (
               at_def state.bindings v;
               reason_sources_at formula database state.learned
                 state.bindings (Bigint.of_int v);
               reason_source_exists formula database state.learned
                 binding.reason);
             (match source_of_reason state.learned binding.reason with
              | None -> None
              | Some source ->
                match fetch_result formula database source with
                | None ->
                  let _ : {u : unit | false} = () in
                  None
                | Some reason ->
                  let resolved =
                    if has_positive v current.clause then
                      resolve_result formula v current reason
                    else resolve_result formula v reason current in
                  analyze formula database state stop (fuel - 1) resolved)
           | Some None | None ->
             let _ : {u : unit | false} = () in
             None)
    | Scan_stable | Scan_unit _ -> None
[@@decreases fuel]

let[@def] at_level (bindings : binding option list) (level : int) literal =
  match at bindings (variable literal) with
  | Some (Some binding) -> binding.level = level
  | Some None | None -> false

let rec (asserting_clause @ total) : (clause : literal list) ->
    (bindings : binding option list) -> (level : int) ->
    (asserting : {a : literal option | match a with
      | None -> true | Some literal -> at_level bindings level literal}) ->
    (target : int) ->
    {result : literal option * int | match result with
      | literal, next -> target <= next
        && (if levels_bounded level bindings && target < level then
          next < level else true)
        && (match literal with None -> true
          | Some literal -> at_level bindings level literal)} =
  fun clause bindings level asserting target ->
  match clause with
  | [] -> asserting, target
  | literal :: rest ->
    let v = variable literal in
    match at bindings v with
    | Some (Some binding) ->
      ghost_ (
        at_def bindings v;
        levels_at bindings level (Bigint.of_int v);
        at_level_def bindings level literal);
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

let (scan_unit_binding @ total) :
    (bindings : {bs : binding option list |
      Bigint.compare (Vox_sequence.length bs) 256Z <= 0}) ->
    (formula : formula) ->
    {u : unit | match scan_formula (binding_values bindings) formula with
      | Scan_unit (_, literal) ->
        at bindings (variable literal) === Some None
      | Scan_stable | Scan_conflict _ -> true} =
  fun bindings formula ->
  scan_formula_unit_unassigned (binding_values bindings) formula;
  match scan_formula (binding_values bindings) formula with
  | Scan_stable | Scan_conflict _ -> ()
  | Scan_unit (_, literal) ->
    variable_def literal;
    match literal with
    | Positive v | Negative v ->
      lookup_unassigned bindings v;
      at_def bindings v

let rec (propagate @ total) :
    (formula : formula) ->
    (database : {d : proof_result list | database_valid formula d}) ->
    (state : {s : state |
      Bigint.compare (Vox_sequence.length s.bindings) 256Z <= 0
      && trail_covers s.bindings s.trail
      && 0 <= s.level && levels_bounded s.level s.bindings
      && 0 <= s.learned
      && Vox_sequence.length database === Bigint.of_int s.learned
      && reason_sources_valid formula database s.learned s.bindings}) ->
    (remaining : {count : Bigint.t |
      Bigint.compare count 0Z >= 0
      && count === unassigned state.bindings}) @ ghost ->
    {r : propagation |
      match r with
      | Stable (next, partial) ->
        scan_formula partial formula === Scan_stable
        && trail_covers next.bindings next.trail
        && next.learned = state.learned && next.level = state.level
        && levels_bounded next.level next.bindings
        && reason_sources_valid formula database next.learned next.bindings
        && Vox_sequence.length next.bindings ===
          Vox_sequence.length state.bindings
        && Vox_sequence.length partial ===
          Vox_sequence.length state.bindings
        && (if all_bound next.bindings then
          match complete_partial partial with Some _ -> true | None -> false
          else true)
      | Conflict (source, next) -> Vox_sequence.length next.bindings ===
          Vox_sequence.length state.bindings
        && trail_covers next.bindings next.trail
        && next.learned = state.learned && next.level = state.level
        && levels_bounded next.level next.bindings
        && reason_sources_valid formula database next.learned next.bindings
        && not (source_clause formula database source === None)} =
  fun formula database state remaining ->
  ghost_ (unassigned_bounds state.bindings);
  let partial = partial_of_bindings state.bindings in
  ghost_ (scan_formula_source partial formula);
  match scan_formula partial formula with
  | Scan_conflict index ->
    ghost_ (source_clause_def formula database (Original_clause index));
    Conflict (Original_clause index, state)
  | Scan_unit (index, literal) ->
    ghost_ (scan_unit_binding state.bindings formula);
    ghost_ (source_clause_def formula database (Original_clause index));
    ghost_ (reason_source_valid_def formula database state.learned
      (Original index));
    (match enqueue (ghost_ formula) (ghost_ database) state literal (Original
      index) with
     | None ->
       let _ : {u : unit | false} = () in
       Conflict (Original_clause index, state)
     | Some state ->
       ghost_ (unassigned_bounds state.bindings);
       propagate formula database state (ghost_ (Bigint.sub remaining 1Z)))
  | Scan_stable ->
    let clauses = database_clauses database in
    ghost_ (scan_formula_source partial clauses);
    match scan_formula partial clauses with
    | Scan_conflict index ->
      ghost_ (source_clause_def formula database (Learned_clause index));
      Conflict (Learned_clause index, state)
    | Scan_unit (index, literal) ->
      ghost_ (scan_unit_binding state.bindings clauses);
      ghost_ (source_clause_def formula database (Learned_clause index));
      ghost_ (
        database_clauses_length database;
        clause_at_bounds state.learned clauses index;
        reason_source_valid_def formula database state.learned
          (Learned (state.learned - 1 - index)));
      (match enqueue (ghost_ formula) (ghost_ database) state literal (Learned
        (state.learned - 1 - index)) with
       | None ->
         let _ : {u : unit | false} = () in
         Conflict (Learned_clause index, state)
       | Some state ->
         ghost_ (unassigned_bounds state.bindings);
         propagate formula database state (ghost_ (Bigint.sub remaining 1Z)))
    | Scan_stable -> Stable (state, partial)
[@@decreases remaining]

let (statistics @ total) (state : state) = {
  decisions = state.decisions;
  conflicts = state.conflicts;
  learned = state.learned;
  backjumps = state.backjumps;
  steps = state.steps;
}

let rec (search @ total) :
    (limit : {l : int | 0 <= l}) @ ghost ->
    (formula : formula) ->
    (scores : int list) ->
    (database : {d : proof_result list | database_valid formula d}) ->
    (state : {s : state |
      Bigint.compare (Vox_sequence.length s.bindings) 256Z <= 0
      && trail_covers s.bindings s.trail
      && 0 <= s.level && levels_bounded s.level s.bindings
      && 0 <= s.learned
      && Vox_sequence.length database === Bigint.of_int s.learned
      && reason_sources_valid formula database s.learned s.bindings}) ->
    (fuel : {f : int | 0 <= f
      && Bigint.compare
        (Bigint.add (Bigint.of_int state.learned) (Bigint.of_int f))
        (Bigint.of_int limit) <= 0
      && Bigint.compare
        (Bigint.add (Bigint.of_int state.level) (Bigint.of_int f))
        (Bigint.of_int limit) <= 0}) ->
    {r : report |
      match r.answer with
      | Sat assignment -> eval_formula assignment formula
        && Vox_sequence.length assignment ===
          Vox_sequence.length state.bindings
      | Unsat entry ->
        derivation_valid formula entry.proof
        && same_clause (conclusion formula entry.proof) entry.clause
        && entry.clause === []
      | Unknown -> true} =
  fun limit formula scores database state fuel ->
  if fuel <= 0 then {answer = Unknown; statistics = statistics state}
  else
    let state = {state with steps = state.steps + 1} in
    ghost_ (unassigned_bounds state.bindings);
    match propagate formula database state
      (ghost_ (unassigned state.bindings)) with
    | Stable (state, partial) ->
      (match complete_partial partial with
       | Some assignment ->
         ghost_ (scan_formula_complete partial formula assignment);
         ghost_ (complete_partial_length partial assignment);
         {answer = Sat assignment; statistics = statistics state}
       | None ->
         (match choose_variable state.bindings scores with
          | None ->
            let _ : {u : unit | false} = () in
            {answer = Unknown; statistics = statistics state}
          | Some (v, _) ->
            ghost_ (at_def state.bindings v);
            ghost_ (variable_def (Positive v));
            ghost_ (levels_weaken state.bindings state.level (state.level + 1));
            let state = {state with
              level = state.level + 1;
              decisions = state.decisions + 1} in
            ghost_ (reason_source_valid_def formula database state.learned
              Decision);
            (match enqueue (ghost_ formula) (ghost_ database) state (Positive
              v) Decision with
             | None ->
               let _ : {u : unit | false} = () in
               {answer = Unknown; statistics = statistics state}
             | Some state -> search (ghost_ limit) formula scores database
               state (fuel - 1))))
    | Conflict (source, state) ->
      let state = {state with conflicts = state.conflicts + 1} in
      (match fetch_result formula database source with
       | None ->
         let _ : {u : unit | false} = () in
         {answer = Unknown; statistics = statistics state}
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
                 if target >= state.level then (
                   let _ : {u : unit | false} = () in
                   {answer = Unknown; statistics = statistics state})
                 else
                   let old_level = state.level in
                   ghost_ (
                     at_level_def state.bindings state.level literal;
                     at_def state.bindings (variable literal);
                     retained_binding_at target state.bindings
                       (Bigint.of_int (variable literal)));
                   let state = backtrack (ghost_ formula) (ghost_ database)
                     state target in
                   ghost_ (at_def state.bindings (variable literal));
                   ghost_ (reason_sources_prepend formula database
                     state.learned limit learned state.bindings);
                   let state = {state with
                     learned = state.learned + 1;
                     backjumps = state.backjumps +
                       (if target < old_level - 1 then 1 else 0)} in
                   let database = database_cons formula learned database in
                   let reason = Learned (state.learned - 1) in
                   ghost_ (
                     Vox_sequence.length_def database;
                     database_clauses_def database;
                     clause_at_def (database_clauses database) 0;
                     reason_source_valid_def formula database state.learned
                       reason);
                   (match enqueue (ghost_ formula) (ghost_ database) state
                     literal reason with
                    | None ->
                      let _ : {u : unit | false} = () in
                      {answer = Unknown; statistics = statistics state}
                    | Some state ->
                      search (ghost_ limit) formula scores database state
                        (fuel - 1)))))
[@@decreases fuel]

let (solve @ total) :
    (fuel : int) -> (n : int) -> (formula : formula) ->
    {r : (report, input_error) result |
      match r with
      | Error Invalid_fuel -> fuel < 0
      | Error (Invalid_input Unsupported_variable_count) ->
        0 <= fuel && (n < 0 || n > 256)
      | Error (Invalid_input Too_many_clauses) ->
        0 <= fuel && 0 <= n && n <= 256
        && not (clauses_fit 4096 formula)
      | Error (Invalid_input Too_many_literals) ->
        0 <= fuel && 0 <= n && n <= 256 && clauses_fit 4096 formula
        && not (literals_fit 65536 formula)
      | Error (Invalid_input Invalid_formula) ->
        0 <= fuel && 0 <= n && n <= 256 && clauses_fit 4096 formula
        && literals_fit 65536 formula && not (valid_formula n formula)
      | Ok report ->
        0 <= fuel && 0 <= n && n <= 256 && clauses_fit 4096 formula
        && literals_fit 65536 formula && valid_formula n formula
        && match report.answer with
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
    ghost_ (
      unassigned_levels initial.bindings 0;
      unassigned_covered initial.bindings initial.trail
        (Vox_sequence.length initial.bindings);
      trail_covers_def initial.bindings initial.trail);
    let scores = occurrence_scores formula n 0 in
    let database = database_empty formula in
    ghost_ (
      Vox_sequence.length_def database;
      unassigned_reason_sources formula database 0 initial.bindings);
    let report = search (ghost_ fuel) formula scores database initial fuel in
    match report.answer with
    | Sat assignment ->
      ghost_ (assignment_length n assignment);
      ghost_ (check_def n formula assignment);
      Ok report
    | Unsat _ | Unknown -> Ok report

let (solve_with_fallback @ total) :
    (fuel : int) -> (depth_fuel : int) -> (n : int) -> (formula : formula) ->
    {r : (report, input_error) result |
      match r with
      | Error Invalid_fuel -> fuel < 0 || depth_fuel < 0
      | Error (Invalid_input Unsupported_variable_count) ->
        0 <= fuel && 0 <= depth_fuel && (n < 0 || n > 256)
      | Error (Invalid_input Too_many_clauses) ->
        0 <= fuel && 0 <= depth_fuel && 0 <= n && n <= 256
        && not (clauses_fit 4096 formula)
      | Error (Invalid_input Too_many_literals) ->
        0 <= fuel && 0 <= depth_fuel && 0 <= n && n <= 256
        && clauses_fit 4096 formula
        && not (literals_fit 65536 formula)
      | Error (Invalid_input Invalid_formula) ->
        0 <= fuel && 0 <= depth_fuel && 0 <= n && n <= 256
        && clauses_fit 4096 formula
        && literals_fit 65536 formula && not (valid_formula n formula)
      | Ok report ->
        0 <= fuel && 0 <= depth_fuel && 0 <= n && n <= 256
        && clauses_fit 4096 formula
        && literals_fit 65536 formula && valid_formula n formula
        && match report.answer with
        | Sat assignment -> check n formula assignment
        | Unsat entry ->
          derivation_valid formula entry.proof
          && same_clause (conclusion formula entry.proof) entry.clause
          && entry.clause === []
        | Unknown -> depth_fuel <= n} =
  fun fuel depth_fuel n formula ->
  if depth_fuel < 0 then Error Invalid_fuel
  else match solve fuel n formula with
  | Error error -> Error error
  | Ok report ->
    match report.answer with
    | Sat _ | Unsat _ -> Ok report
    | Unknown ->
      match Vox_sat_proof.decide_depth depth_fuel n formula with
      | Vox_sat_proof.Sat assignment ->
        ghost_ (check_def n formula assignment);
        Ok {report with answer = Sat assignment}
      | Vox_sat_proof.Unsat ->
        let proof = exhaustive_result n formula in
        Ok {report with answer = Unsat proof}
      | Vox_sat_proof.Unknown -> Ok report

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

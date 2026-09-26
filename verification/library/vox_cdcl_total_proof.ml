open Vox_sat_spec
open Vox_sat_proof

type reason : immutable_data mod total =
  | Decision
  | Original of int
  | Learned of stored_result
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
  | Conflict of proof_result * state
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
let[@def] wanted literal =
  match literal with Positive _ -> true | Negative _ -> false

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

let[@def] implied_reason (reason : reason) =
  match reason with
  | Decision -> None
  | Original _ | Learned _ -> Some reason

let[@def] reason_source_clause (formula : formula)
    (database : proof_result list) (reason : reason) =
  match reason with
  | Decision -> None
  | Original index -> clause_at formula index
  | Learned stored -> Some (load_result stored).clause

let[@def] reason_source_valid (formula : formula) (database : proof_result list)
    (learned : int) reason =
  ghost_ (match reason with
  | Decision -> true
  | Original index ->
    (match clause_at formula index with None -> false | Some _ -> true)
  | Learned stored ->
    let entry = load_result stored in
    derivation_valid formula entry.proof
    && same_clause (conclusion formula entry.proof) entry.clause)

let (fetch_reason @ total) : (formula : formula) ->
    (database : proof_result list) @ ghost -> (learned : int) @ ghost ->
    (reason : {r : reason | reason_source_valid formula database learned r}) ->
    {r : proof_result option | match r with
      | None -> reason_source_clause formula database reason === None
      | Some entry -> derivation_valid formula entry.proof
        && same_clause (conclusion formula entry.proof) entry.clause
        && reason_source_clause formula database reason === Some entry.clause} =
  fun formula database learned reason ->
  ghost_ (reason_source_valid_def formula database learned reason;
    reason_source_clause_def formula database reason);
  match reason with
  | Decision -> None
  | Original index -> original_result formula index
  | Learned stored -> Some (load_result stored)

let (reason_source_clause_valid @ total) : (n : int) -> (formula : formula) ->
    (database : proof_result list) -> (learned : int) -> (reason : reason) ->
    {u : unit | if valid_formula n formula
        && reason_source_valid formula database learned reason then
      match reason_source_clause formula database reason with
      | None -> true | Some clause -> valid_clause n clause else true} =
  fun n formula database learned reason ->
  ghost_ (
  reason_source_valid_def formula database learned reason;
  reason_source_clause_def formula database reason;
  match reason with
  | Decision -> ()
  | Original index -> clause_at_valid n formula index
  | Learned stored ->
    let entry = load_result stored in
    if valid_formula n formula
        && reason_source_valid formula database learned reason then
      result_clause_valid n formula entry;
    ());
  ()

let[@def] rec reason_sources_valid formula database learned
    (bindings : binding option list) =
  ghost_ (match bindings with
  | [] -> true
  | None :: rest -> reason_sources_valid formula database learned rest
  | Some binding :: rest ->
    reason_source_valid formula database learned binding.reason
    && reason_sources_valid formula database learned rest)

let[@def] rec reason_clause (bindings : binding option list)
    pivot level value clause =
  match clause with
  | [] -> true
  | literal :: rest ->
    (if Bigint.equal (Bigint.of_int (variable literal)) pivot then
       wanted literal = value
     else match at bindings (variable literal) with
       | Some (Some binding) ->
         binding.value <> wanted literal && binding.level <= level
       | Some None | None -> false)
    && reason_clause bindings pivot level value rest

let[@def] rec reason_contains pivot value clause =
  match clause with
  | [] -> false
  | literal :: rest ->
    (Bigint.equal (Bigint.of_int (variable literal)) pivot
      && wanted literal = value) || reason_contains pivot value rest

let rec (reason_contains_literal @ total) : (literal : literal) ->
    (clause : literal list) ->
    {u : unit | has_literal literal clause =
      reason_contains (Bigint.of_int (variable literal)) (wanted literal)
        clause} =
  fun literal clause ->
  has_literal_def literal clause;
  reason_contains_def (Bigint.of_int (variable literal)) (wanted literal)
    clause;
  match clause with
  | [] -> ()
  | candidate :: rest ->
    same_literal_def literal candidate;
    variable_def literal;
    variable_def candidate;
    wanted_def literal;
    wanted_def candidate;
    reason_contains_literal literal rest

let[@def] binding_reason formula database learned bindings pivot
    (binding : binding) =
  match implied_reason binding.reason with
  | None -> true
  | Some source -> match reason_source_clause formula database source with
    | None -> false
    | Some clause -> reason_clause bindings pivot binding.level binding.value
        clause && reason_contains pivot binding.value clause

let[@def] enqueue_reason formula database learned bindings literal level
  reason =
  match implied_reason reason with
  | None -> true
  | Some source -> match reason_source_clause formula database source with
    | None -> false
    | Some clause -> reason_clause bindings
        (Bigint.of_int (variable literal)) level (wanted literal) clause
      && reason_contains (Bigint.of_int (variable literal)) (wanted literal)
        clause

let[@def] rec reason_semantics_from formula database learned bindings index
    (remaining : binding option list) =
  match remaining with
  | [] -> true
  | first :: rest ->
    (match first with
     | None -> true
     | Some binding -> binding_reason formula database learned bindings index
         binding)
    && reason_semantics_from formula database learned bindings
      (Bigint.add index 1Z) rest

let[@def] reason_semantics formula database learned bindings =
  reason_semantics_from formula database learned bindings 0Z bindings

let rec (reason_clause_set @ total) : (bindings : binding option list) ->
    (index : Bigint.t) -> (entry : binding) -> (pivot : Bigint.t) ->
    (level : int) -> (value : bool) -> (clause : literal list) ->
    {u : unit | if reason_clause bindings pivot level value clause
        && Vox_sequence.at bindings index === Some None then
      reason_clause (Vox_sequence.set bindings index (Some entry))
        pivot level value clause else true} =
  fun bindings index entry pivot level value clause ->
  let next = Vox_sequence.set bindings index (Some entry) in
  reason_clause_def bindings pivot level value clause;
  reason_clause_def next pivot level value clause;
  match clause with
  | [] -> ()
  | literal :: rest ->
    let query = variable literal in
    at_def bindings query;
    at_def next query;
    binding_at_set bindings index (Some entry) (Bigint.of_int query);
    reason_clause_set bindings index entry pivot level value rest

let (binding_reason_set @ total) : (formula : formula) ->
    (database : proof_result list) -> (learned : int) ->
    (bindings : binding option list) -> (index : Bigint.t) ->
    (entry : binding) -> (pivot : Bigint.t) -> (binding : binding) ->
    {u : unit | if binding_reason formula database learned bindings pivot
        binding && Vox_sequence.at bindings index === Some None then
      binding_reason formula database learned
        (Vox_sequence.set bindings index (Some entry)) pivot binding
      else true} =
  fun formula database learned bindings index entry pivot binding ->
  binding_reason_def formula database learned bindings pivot binding;
  binding_reason_def formula database learned
    (Vox_sequence.set bindings index (Some entry)) pivot binding;
  match implied_reason binding.reason with
  | None -> ()
  | Some source -> match reason_source_clause formula database source with
    | None -> ()
    | Some clause -> reason_clause_set bindings index entry pivot
        binding.level binding.value clause

let rec (reason_environment_set @ total) : (formula : formula) ->
    (database : proof_result list) -> (learned : int) ->
    (bindings : binding option list) -> (index : Bigint.t) ->
    (entry : binding) -> (offset : Bigint.t) ->
    (remaining : binding option list) ->
    {u : unit | if reason_semantics_from formula database learned bindings
        offset remaining && Vox_sequence.at bindings index === Some None then
      reason_semantics_from formula database learned
        (Vox_sequence.set bindings index (Some entry)) offset remaining
      else true} =
  fun formula database learned bindings index entry offset remaining ->
  reason_semantics_from_def formula database learned bindings offset remaining;
  reason_semantics_from_def formula database learned
    (Vox_sequence.set bindings index (Some entry)) offset remaining;
  match remaining with
  | [] -> ()
  | first :: rest ->
    (match first with
     | None -> ()
     | Some binding -> binding_reason_set formula database learned bindings
         index entry offset binding);
    reason_environment_set formula database learned bindings index entry
      (Bigint.add offset 1Z) rest

let rec (reason_slots_set @ total) : (formula : formula) ->
    (database : proof_result list) -> (learned : int) ->
    (bindings : binding option list) -> (offset : Bigint.t) ->
    (remaining : binding option list) -> (index : Bigint.t) ->
    (entry : binding) ->
    {u : unit | if reason_semantics_from formula database learned bindings
        offset remaining && binding_reason formula database learned bindings
        (Bigint.add offset index) entry then
      reason_semantics_from formula database learned bindings offset
        (Vox_sequence.set remaining index (Some entry)) else true} =
  fun formula database learned bindings offset remaining index entry ->
  reason_semantics_from_def formula database learned bindings offset remaining;
  Vox_sequence.set_def remaining index (Some entry);
  reason_semantics_from_def formula database learned bindings offset
    (Vox_sequence.set remaining index (Some entry));
  match remaining with
  | [] -> ()
  | _ :: rest ->
    if not (Bigint.equal index 0Z) then
      reason_slots_set formula database learned bindings (Bigint.add offset 1Z)
        rest (Bigint.sub index 1Z) entry;
    ()

let (reason_semantics_set @ total) : (formula : formula) ->
    (database : proof_result list) -> (learned : int) ->
    (bindings : binding option list) -> (index : Bigint.t) ->
    (entry : binding) ->
    {u : unit | if reason_semantics formula database learned bindings
        && Vox_sequence.at bindings index === Some None
        && binding_reason formula database learned bindings index entry then
      reason_semantics formula database learned
        (Vox_sequence.set bindings index (Some entry)) else true} =
  fun formula database learned bindings index entry ->
  let next = Vox_sequence.set bindings index (Some entry) in
  reason_semantics_def formula database learned bindings;
  reason_semantics_def formula database learned next;
  binding_reason_set formula database learned bindings index entry index entry;
  reason_environment_set formula database learned bindings index entry 0Z
    bindings;
  reason_slots_set formula database learned next 0Z bindings index entry

let rec (sequence_length_nonnegative @ total) :
    (values : ('a : immutable_data) list) @ immutable ->
    {u : unit | Bigint.compare (Vox_sequence.length values) 0Z >= 0} =
  fun values ->
  Vox_sequence.length_def values;
  match values with [] -> () | _ :: rest -> sequence_length_nonnegative rest

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
    (database : proof_result list) -> (learned : int) ->
    (entry : proof_result) -> (reason : reason) ->
    {u : unit | if reason_source_valid formula database learned reason then
      reason_source_valid formula (entry :: database) (learned + 1) reason
      else true} =
  fun formula database learned entry reason ->
  reason_source_valid_def formula database learned reason;
  reason_source_valid_def formula (entry :: database) (learned + 1) reason;
  ()

let rec (reason_sources_prepend @ total) : (formula : formula) ->
    (database : proof_result list) -> (learned : int) ->
    (entry : proof_result) -> (bindings : binding option list) ->
    {u : unit | if reason_sources_valid formula database learned bindings then
      reason_sources_valid formula (entry :: database) (learned + 1) bindings
      else true} =
  fun formula database learned entry bindings ->
  reason_sources_valid_def formula database learned bindings;
  reason_sources_valid_def formula (entry :: database) (learned + 1) bindings;
  match bindings with
  | [] -> ()
  | first :: rest ->
    (match first with
     | None -> ()
     | Some binding ->
       reason_source_prepend formula database learned entry
         binding.reason);
    reason_sources_prepend formula database learned entry rest

let (binding_reason_prepend @ total) : (formula : formula) ->
    (database : proof_result list) -> (learned : int) ->
    (entry : proof_result) -> (bindings : binding option list) ->
    (index : Bigint.t) -> (binding : binding) ->
    {u : unit | if reason_source_valid formula database learned binding.reason
        && binding_reason formula database learned bindings index binding then
      binding_reason formula (entry :: database) (learned + 1) bindings index
        binding else true} =
  fun formula database learned entry bindings index binding ->
  reason_source_valid_def formula database learned binding.reason;
  binding_reason_def formula database learned bindings index binding;
  binding_reason_def formula (entry :: database) (learned + 1) bindings index
    binding;
  implied_reason_def binding.reason;
  match implied_reason binding.reason with
  | None -> ()
  | Some source ->
    reason_source_clause_def formula database source;
    reason_source_clause_def formula (entry :: database) source

let rec (reason_slots_prepend @ total) : (formula : formula) ->
    (database : proof_result list) -> (learned : int) ->
    (entry : proof_result) -> (bindings : binding option list) ->
    (offset : Bigint.t) -> (remaining : binding option list) ->
    {u : unit | if reason_sources_valid formula database learned remaining
        && reason_semantics_from formula database learned bindings offset
          remaining then
      reason_semantics_from formula (entry :: database) (learned + 1) bindings
        offset remaining else true} =
  fun formula database learned entry bindings offset remaining ->
  reason_sources_valid_def formula database learned remaining;
  reason_semantics_from_def formula database learned bindings offset remaining;
  reason_semantics_from_def formula (entry :: database) (learned + 1) bindings
    offset remaining;
  match remaining with
  | [] -> ()
  | first :: rest ->
    (match first with
     | None -> ()
     | Some binding -> binding_reason_prepend formula database learned
         entry bindings offset binding);
    reason_slots_prepend formula database learned entry bindings
      (Bigint.add offset 1Z) rest

let (reason_semantics_prepend @ total) : (formula : formula) ->
    (database : proof_result list) -> (learned : int) ->
    (entry : proof_result) -> (bindings : binding option list) ->
    {u : unit | if reason_sources_valid formula database learned bindings
        && reason_semantics formula database learned bindings then
      reason_semantics formula (entry :: database) (learned + 1) bindings
      else true} =
  fun formula database learned entry bindings ->
  reason_semantics_def formula database learned bindings;
  reason_semantics_def formula (entry :: database) (learned + 1) bindings;
  reason_slots_prepend formula database learned entry bindings 0Z bindings

let rec (unassigned_reason_semantics_from @ total) : (formula : formula) ->
    (database : proof_result list) -> (learned : int) ->
    (bindings : binding option list) -> (offset : Bigint.t) ->
    (remaining : binding option list) ->
    {u : unit | if all_unassigned remaining then
      reason_semantics_from formula database learned bindings offset remaining
      else true} =
  fun formula database learned bindings offset remaining ->
  all_unassigned_def remaining;
  reason_semantics_from_def formula database learned bindings offset remaining;
  match remaining with
  | [] -> ()
  | _ :: rest -> unassigned_reason_semantics_from formula database learned
      bindings (Bigint.add offset 1Z) rest

let (unassigned_reason_semantics @ total) : (formula : formula) ->
    (database : proof_result list) -> (learned : int) ->
    (bindings : binding option list) ->
    {u : unit | if all_unassigned bindings then
      reason_semantics formula database learned bindings else true} =
  fun formula database learned bindings ->
  reason_semantics_def formula database learned bindings;
  unassigned_reason_semantics_from formula database learned bindings 0Z bindings

let (reason_source_exists @ total) : (formula : formula) ->
    (database : proof_result list) -> (learned : int) -> (reason : reason) ->
    {u : unit | if reason_source_valid formula database learned reason then
      match implied_reason reason with
      | None -> true
      | Some source -> reason_source_valid formula database learned source
        && not (reason_source_clause formula database source === None)
      else true} =
  fun formula database learned reason ->
  reason_source_valid_def formula database learned reason;
  implied_reason_def reason;
  reason_source_clause_def formula database reason

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

let[@def] rec trail_consistent (bindings : binding option list) trail =
  match trail with
  | [] -> true
  | literal :: rest ->
    (match at bindings (variable literal) with
     | Some (Some binding) -> binding.value = wanted literal
     | Some None | None -> false)
    && trail_consistent bindings rest

let[@def] rec trail_unique trail =
  match trail with
  | [] -> true
  | literal :: rest ->
    not (trail_has (Bigint.of_int (variable literal)) rest)
    && trail_unique rest

let rec (consistent_absent @ total) : (bindings : binding option list) ->
    (trail : literal list) -> (v : int) ->
    {u : unit | if trail_consistent bindings trail
        && at bindings v === Some None then
      not (trail_has (Bigint.of_int v) trail) else true} =
  fun bindings trail v ->
  trail_consistent_def bindings trail;
  trail_has_def (Bigint.of_int v) trail;
  match trail with
  | [] -> ()
  | _ :: rest -> consistent_absent bindings rest v

let rec (consistent_set @ total) : (bindings : binding option list) ->
    (trail : literal list) -> (v : int) -> (binding : binding) ->
    {u : unit | if trail_consistent bindings trail
        && at bindings v === Some None then
      trail_consistent
        (Vox_sequence.set bindings (Bigint.of_int v) (Some binding)) trail
      else true} =
  fun bindings trail v binding ->
  let next = Vox_sequence.set bindings (Bigint.of_int v) (Some binding) in
  trail_consistent_def bindings trail;
  trail_consistent_def next trail;
  match trail with
  | [] -> ()
  | literal :: rest ->
    let query = variable literal in
    at_def bindings v;
    at_def bindings query;
    at_def next query;
    binding_at_set bindings (Bigint.of_int v) (Some binding)
      (Bigint.of_int query);
    consistent_set bindings rest v binding

let[@def] rec trail_levels (bindings : binding option list) trail level =
  match trail with
  | [] -> level = 0
  | literal :: rest ->
    match at bindings (variable literal) with
    | Some (Some binding) -> binding.level = level
      && (match binding.reason with
        | Decision -> 0 < level && trail_levels bindings rest (level - 1)
        | Original _ | Learned _ -> trail_levels bindings rest level)
    | Some None | None -> false

let rec (trail_levels_bound @ total) : (bindings : binding option list) ->
    (trail : literal list) -> (level : int) ->
    {u : unit | if trail_levels bindings trail level then
      0 <= level && Bigint.compare (Bigint.of_int level)
        (Vox_sequence.length trail) <= 0 else true} =
  fun bindings trail level ->
  trail_levels_def bindings trail level;
  Vox_sequence.length_def trail;
  match trail with
  | [] -> ()
  | literal :: rest ->
    (match at bindings (variable literal) with
     | None | Some None -> ()
     | Some (Some binding) ->
       match binding.reason with
       | Decision -> if 0 < level then trail_levels_bound bindings rest (level -
         1)
       | Original _ | Learned _ -> trail_levels_bound bindings rest level);
    ()

let rec (trail_levels_set @ total) : (bindings : binding option list) ->
    (trail : literal list) -> (level : int) -> (v : int) -> (binding : binding)
      ->
    {u : unit | if trail_levels bindings trail level
        && at bindings v === Some None then
      trail_levels (Vox_sequence.set bindings (Bigint.of_int v) (Some binding))
        trail level else true} =
  fun bindings trail level v binding ->
  let next = Vox_sequence.set bindings (Bigint.of_int v) (Some binding) in
  trail_levels_def bindings trail level;
  trail_levels_def next trail level;
  match trail with
  | [] -> ()
  | literal :: rest ->
    let query = variable literal in
    at_def bindings v;
    at_def bindings query;
    at_def next query;
    binding_at_set bindings (Bigint.of_int v) (Some binding)
      (Bigint.of_int query);
    (match at bindings query with
     | None | Some None -> ()
     | Some (Some previous) ->
       match previous.reason with
       | Decision -> trail_levels_set bindings rest (level - 1) v binding
       | Original _ | Learned _ -> trail_levels_set bindings rest level v
         binding);
    ()

let[@def] preceding_level (binding : binding) =
  match binding.reason with Decision -> binding.level - 1
  | Original _ | Learned _ -> binding.level

let[@def] rec trail_ordered (bindings : binding option list) trail upper =
  match trail with
  | [] -> true
  | literal :: rest ->
    match at bindings (variable literal) with
    | Some (Some binding) ->
      0 <= binding.level && binding.level <= upper
      && (match binding.reason with Decision -> 0 < binding.level
          | Original _ | Learned _ -> true)
      && trail_ordered bindings rest (preceding_level binding)
    | Some None | None -> false

let (ordered_weaken @ total) : (bindings : binding option list) ->
    (trail : literal list) -> (lower : int) -> (upper : int) ->
    {u : unit | if lower <= upper && trail_ordered bindings trail lower then
      trail_ordered bindings trail upper else true} =
  fun bindings trail lower upper ->
  trail_ordered_def bindings trail lower;
  trail_ordered_def bindings trail upper;
  ()

let rec (ordered_set @ total) : (bindings : binding option list) ->
    (trail : literal list) -> (upper : int) -> (v : int) ->
    (binding : binding) ->
    {u : unit | if trail_ordered bindings trail upper
        && at bindings v === Some None then
      trail_ordered
        (Vox_sequence.set bindings (Bigint.of_int v) (Some binding))
        trail upper else true} =
  fun bindings trail upper v binding ->
  let next = Vox_sequence.set bindings (Bigint.of_int v) (Some binding) in
  trail_ordered_def bindings trail upper;
  trail_ordered_def next trail upper;
  match trail with
  | [] -> ()
  | literal :: rest ->
    let query = variable literal in
    at_def bindings v;
    at_def bindings query;
    at_def next query;
    binding_at_set bindings (Bigint.of_int v) (Some binding)
      (Bigint.of_int query);
    match at bindings query with
    | Some (Some previous) ->
      ordered_set bindings rest (preceding_level previous) v binding
    | Some None | None -> ()

let rec (lookup_value @ total) : (bindings : binding option list) ->
    (index : int) ->
    {u : unit |
      (if 0 <= index then partial_lookup (binding_values bindings) index ===
        (match at bindings index with
         | None -> Some false | Some None -> None
         | Some (Some binding) -> Some binding.value) else true)
      && (if 0 <= index && Bigint.compare (Bigint.of_int index)
          (Vox_sequence.length bindings) < 0 then
        match at bindings index with Some _ -> true | None -> false
        else true)} =
  fun bindings index ->
  binding_values_def bindings;
  partial_lookup_def (binding_values bindings) index;
  at_def bindings index;
  Vox_sequence.at_def bindings (Bigint.of_int index);
  Vox_sequence.length_def bindings;
  match bindings with
  | [] -> ()
  | _ :: rest ->
    if index > 0 then (
      lookup_value rest (index - 1);
      at_def rest (index - 1));
    ()

let rec (unit_clause_reason @ total) : (n : int) ->
    (bindings : binding option list) -> (level : int) ->
    (forced : literal) -> (clause : literal list) ->
    {u : unit | if Vox_sequence.length bindings === Bigint.of_int n
        && levels_bounded level bindings && valid_clause n clause
        && at bindings (variable forced) === Some None
        && false_except (binding_values bindings) forced clause then
      reason_clause bindings (Bigint.of_int (variable forced)) level
        (wanted forced) clause else true} =
  fun n bindings level forced clause ->
  false_except_def (binding_values bindings) forced clause;
  valid_clause_def n clause;
  reason_clause_def bindings (Bigint.of_int (variable forced)) level
    (wanted forced) clause;
  match clause with
  | [] -> ()
  | literal :: rest ->
    valid_literal_def n literal;
    variable_def literal;
    variable_def forced;
    wanted_def literal;
    wanted_def forced;
    same_literal_def literal forced;
    partial_literal_def (binding_values bindings) literal;
    lookup_value bindings (variable literal);
    at_def bindings (variable literal);
    levels_at bindings level (Bigint.of_int (variable literal));
    unit_clause_reason n bindings level forced rest

let (unit_enqueue_reason @ total) : (n : int) -> (formula : formula) ->
    (database : proof_result list) -> (learned : int) ->
    (bindings : binding option list) -> (level : int) -> (forced : literal) ->
    (reason : reason) ->
    {u : unit | if valid_formula n formula && database_valid formula database
        && reason_source_valid formula database learned reason
        && Vox_sequence.length bindings === Bigint.of_int n
        && levels_bounded level bindings
        && at bindings (variable forced) === Some None
        && (match implied_reason reason with
          | None -> false
          | Some source -> match reason_source_clause formula database source
            with
            | None -> false
            | Some clause -> has_literal forced clause
              && false_except (binding_values bindings) forced
              clause)
      then enqueue_reason formula database learned bindings forced level reason
      else true} =
  fun n formula database learned bindings level forced reason ->
  implied_reason_def reason;
  enqueue_reason_def formula database learned bindings forced level reason;
  match implied_reason reason with
  | None -> ()
  | Some source ->
    reason_source_clause_valid n formula database learned source;
    match reason_source_clause formula database source with
    | None -> ()
    | Some clause ->
      unit_clause_reason n bindings level forced clause;
      reason_contains_literal forced clause

let rec (reason_slots_at @ total) : (formula : formula) ->
    (database : proof_result list) -> (learned : int) ->
    (bindings : binding option list) -> (offset : Bigint.t) ->
    (remaining : binding option list) -> (index : Bigint.t) ->
    {u : unit | if reason_semantics_from formula database learned bindings
        offset remaining then match Vox_sequence.at remaining index with
      | Some (Some binding) -> binding_reason formula database learned bindings
          (Bigint.add offset index) binding
      | Some None | None -> true
      else true} =
  fun formula database learned bindings offset remaining index ->
  reason_semantics_from_def formula database learned bindings offset remaining;
  Vox_sequence.at_def remaining index;
  match remaining with
  | [] -> ()
  | _ :: rest ->
    if not (Bigint.equal index 0Z) then
      reason_slots_at formula database learned bindings (Bigint.add offset 1Z)
        rest (Bigint.sub index 1Z);
    ()

let (reason_semantics_at @ total) : (formula : formula) ->
    (database : proof_result list) -> (learned : int) ->
    (bindings : binding option list) -> (index : Bigint.t) ->
    {u : unit | if reason_semantics formula database learned bindings then
      match Vox_sequence.at bindings index with
      | Some (Some binding) -> binding_reason formula database learned bindings
          index binding
      | Some None | None -> true
      else true} =
  fun formula database learned bindings index ->
  reason_semantics_def formula database learned bindings;
  reason_slots_at formula database learned bindings 0Z bindings index

let[@def] rec trail_after index trail =
  match trail with
  | [] -> []
  | literal :: rest ->
    if Bigint.equal (Bigint.of_int (variable literal)) index then rest
    else trail_after index rest

let[@def] rec earlier_clause trail pivot clause =
  match clause with
  | [] -> true
  | literal :: rest ->
    (Bigint.equal (Bigint.of_int (variable literal)) pivot
      || trail_has (Bigint.of_int (variable literal)) (trail_after pivot trail))
    && earlier_clause trail pivot rest

let[@def] binding_order formula database learned trail pivot
    (binding : binding) =
  match implied_reason binding.reason with
  | None -> true
  | Some source -> match reason_source_clause formula database source with
    | None -> false
    | Some clause -> earlier_clause trail pivot clause

let[@def] rec reason_order_from formula database learned
    (bindings : binding option list) whole remaining =
  match remaining with
  | [] -> true
  | literal :: rest ->
    (match at bindings (variable literal) with
     | Some (Some binding) -> binding_order formula database learned whole
         (Bigint.of_int (variable literal)) binding
     | Some None | None -> false)
    && reason_order_from formula database learned bindings whole rest

let[@def] reason_order formula database learned bindings trail =
  reason_order_from formula database learned bindings trail trail

let rec (reason_order_member @ total) : (formula : formula) ->
    (database : proof_result list) -> (learned : int) ->
    (bindings : binding option list) -> (whole : literal list) ->
    (remaining : literal list) -> (v : int) ->
    {u : unit | if reason_order_from formula database learned bindings whole
        remaining && trail_has (Bigint.of_int v) remaining then
      match at bindings v with
      | Some (Some binding) -> binding_order formula database learned whole
          (Bigint.of_int v) binding
      | Some None | None -> false
      else true} =
  fun formula database learned bindings whole remaining v ->
  reason_order_from_def formula database learned bindings whole remaining;
  trail_has_def (Bigint.of_int v) remaining;
  match remaining with
  | [] -> ()
  | literal :: rest ->
    if variable literal <> v then
      reason_order_member formula database learned bindings whole rest v;
    ()

let[@def] rec trail_rank index trail =
  match trail with
  | [] -> 0Z
  | literal :: rest ->
    if Bigint.equal (Bigint.of_int (variable literal)) index then
      Bigint.add (Vox_sequence.length rest) 1Z
    else trail_rank index rest

let rec (trail_rank_bounds @ total) : (trail : literal list) ->
    (index : Bigint.t) ->
    {u : unit | Bigint.compare 0Z (trail_rank index trail) <= 0
      && Bigint.compare (trail_rank index trail) (Vox_sequence.length trail)
        <= 0
      && (if trail_has index trail then
        Bigint.compare 0Z (trail_rank index trail) < 0
        else trail_rank index trail === 0Z)} =
  fun trail index ->
  trail_rank_def index trail;
  trail_has_def index trail;
  Vox_sequence.length_def trail;
  match trail with
  | [] -> ()
  | _ :: rest -> trail_rank_bounds rest index

let rec (trail_after_subset @ total) : (trail : literal list) ->
    (pivot : Bigint.t) -> (query : Bigint.t) ->
    {u : unit | if trail_has query (trail_after pivot trail) then
      trail_has query trail else true} =
  fun trail pivot query ->
  trail_after_def pivot trail;
  trail_has_def query trail;
  match trail with
  | [] -> ()
  | _ :: rest -> trail_after_subset rest pivot query

let rec (trail_antecedent_rank @ total) : (trail : literal list) ->
    (pivot : Bigint.t) -> (query : Bigint.t) ->
    {u : unit | if trail_unique trail
        && trail_has query (trail_after pivot trail) then
      Bigint.compare (trail_rank query trail) (trail_rank pivot trail) < 0
      else true} =
  fun trail pivot query ->
  trail_unique_def trail;
  trail_after_def pivot trail;
  trail_rank_def pivot trail;
  trail_rank_def query trail;
  match trail with
  | [] -> trail_has_def query []
  | literal :: rest ->
    trail_rank_bounds rest query;
    trail_rank_bounds rest pivot;
    if Bigint.equal (Bigint.of_int (variable literal)) pivot then ()
    else (
      trail_after_subset rest pivot query;
      trail_antecedent_rank rest pivot query)

let[@def] rec clause_rank_except trail pivot clause =
  match clause with
  | [] -> 0Z
  | literal :: rest ->
    let tail = clause_rank_except trail pivot rest in
    let index = Bigint.of_int (variable literal) in
    let rank = if Bigint.equal index pivot then 0Z
      else trail_rank index trail in
    if Bigint.compare rank tail <= 0 then tail else rank

let rec (earlier_clause_rank @ total) : (trail : literal list) ->
    (pivot : Bigint.t) -> (clause : literal list) ->
    {u : unit | if trail_unique trail && trail_has pivot trail
        && earlier_clause trail pivot clause then
      Bigint.compare (clause_rank_except trail pivot clause)
        (trail_rank pivot trail) < 0 else true} =
  fun trail pivot clause ->
  clause_rank_except_def trail pivot clause;
  earlier_clause_def trail pivot clause;
  trail_rank_bounds trail pivot;
  match clause with
  | [] -> ()
  | literal :: rest ->
    trail_antecedent_rank trail pivot (Bigint.of_int (variable literal));
    earlier_clause_rank trail pivot rest

let rec (clause_rank_member @ total) : (trail : literal list) ->
    (pivot : Bigint.t) -> (clause : literal list) -> (query : literal) ->
    {u : unit | if has_literal query clause
        && not (Bigint.equal (Bigint.of_int (variable query)) pivot) then
      Bigint.compare (trail_rank (Bigint.of_int (variable query)) trail)
        (clause_rank_except trail pivot clause) <= 0 else true} =
  fun trail pivot clause query ->
  clause_rank_except_def trail pivot clause;
  has_literal_def query clause;
  match clause with
  | [] -> ()
  | literal :: rest ->
    same_literal_def query literal;
    variable_def query;
    variable_def literal;
    clause_rank_member trail pivot rest query

let[@def] rec clause_rank trail clause =
  match clause with
  | [] -> 0Z
  | literal :: rest ->
    let tail = clause_rank trail rest in
    let rank = trail_rank (Bigint.of_int (variable literal)) trail in
    if Bigint.compare rank tail <= 0 then tail else rank

let rec (clause_rank_nonnegative @ total) : (trail : literal list) ->
    (clause : literal list) ->
    {u : unit | Bigint.compare 0Z (clause_rank trail clause) <= 0} =
  fun trail clause ->
  clause_rank_def trail clause;
  match clause with
  | [] -> ()
  | _ :: rest -> clause_rank_nonnegative trail rest

let rec (clause_rank_contains @ total) : (trail : literal list) ->
    (clause : literal list) -> (query : literal) ->
    {u : unit | if has_literal query clause then
      Bigint.compare (trail_rank (Bigint.of_int (variable query)) trail)
        (clause_rank trail clause) <= 0 else true} =
  fun trail clause query ->
  clause_rank_def trail clause;
  has_literal_def query clause;
  match clause with
  | [] -> ()
  | literal :: rest ->
    same_literal_def query literal;
    variable_def query;
    variable_def literal;
    clause_rank_contains trail rest query

let[@def] rec clause_subset part whole =
  match part with
  | [] -> true
  | literal :: rest -> has_literal literal whole && clause_subset rest whole

let rec (clause_subset_cons @ total) : (part : literal list) ->
    (whole : literal list) -> (literal : literal) ->
    {u : unit | if clause_subset part whole then
      clause_subset part (literal :: whole) else true} =
  fun part whole literal ->
  clause_subset_def part whole;
  clause_subset_def part (literal :: whole);
  match part with
  | [] -> ()
  | first :: rest ->
    has_literal_def first (literal :: whole);
    clause_subset_cons rest whole literal

let rec (clause_subset_self @ total) : (clause : literal list) ->
    {u : unit | clause_subset clause clause} =
  fun clause ->
  clause_subset_def clause clause;
  match clause with
  | [] -> ()
  | literal :: rest ->
    same_literal_def literal literal;
    has_literal_def literal clause;
    clause_subset_self rest;
    clause_subset_cons rest rest literal

let rec (resolve_rank_part @ total) : (trail : literal list) ->
    (pivot : int) -> (bound : Bigint.t) ->
    (positive : literal list) -> (negative : literal list) ->
    (part : literal list) ->
    {u : unit | if Bigint.compare 0Z bound < 0
        && Bigint.compare (clause_rank_except trail (Bigint.of_int pivot)
          positive) bound < 0
        && Bigint.compare (clause_rank_except trail (Bigint.of_int pivot)
          negative) bound < 0
        && not (has_literal (Negative pivot) positive)
        && not (has_literal (Positive pivot) negative)
        && clause_subset part (resolve_clause pivot positive negative) then
      Bigint.compare (clause_rank trail part) bound < 0 else true} =
  fun trail pivot bound positive negative part ->
  clause_subset_def part (resolve_clause pivot positive negative);
  clause_rank_def trail part;
  match part with
  | [] -> ()
  | literal :: rest ->
    resolve_membership pivot positive negative literal;
    same_literal_def literal (Positive pivot);
    same_literal_def literal (Negative pivot);
    variable_def literal;
    clause_rank_member trail (Bigint.of_int pivot) positive literal;
    clause_rank_member trail (Bigint.of_int pivot) negative literal;
    resolve_rank_part trail pivot bound positive negative rest

let (resolve_rank @ total) : (trail : literal list) ->
    (pivot : int) -> (positive : literal list) -> (negative : literal list) ->
    {u : unit | if trail_has (Bigint.of_int pivot) trail
        && Bigint.compare (clause_rank_except trail (Bigint.of_int pivot)
          positive) (trail_rank (Bigint.of_int pivot) trail) < 0
        && Bigint.compare (clause_rank_except trail (Bigint.of_int pivot)
          negative) (trail_rank (Bigint.of_int pivot) trail) < 0
        && not (has_literal (Negative pivot) positive)
        && not (has_literal (Positive pivot) negative) then
      Bigint.compare (clause_rank trail (resolve_clause pivot positive
        negative))
        (trail_rank (Bigint.of_int pivot) trail) < 0 else true} =
  fun trail pivot positive negative ->
  trail_rank_bounds trail (Bigint.of_int pivot);
  clause_subset_self (resolve_clause pivot positive negative);
  resolve_rank_part trail pivot (trail_rank (Bigint.of_int pivot) trail)
    positive negative (resolve_clause pivot positive negative)

let rec (earlier_clause_cons @ total) : (literal : literal) ->
    (trail : literal list) -> (pivot : Bigint.t) -> (clause : literal list) ->
    {u : unit | if earlier_clause trail pivot clause then
      earlier_clause (literal :: trail) pivot clause else true} =
  fun literal trail pivot clause ->
  earlier_clause_def trail pivot clause;
  earlier_clause_def (literal :: trail) pivot clause;
  trail_after_def pivot (literal :: trail);
  match clause with
  | [] -> ()
  | first :: rest ->
    trail_after_subset trail pivot (Bigint.of_int (variable first));
    earlier_clause_cons literal trail pivot rest

let (binding_order_cons @ total) : (formula : formula) ->
    (database : proof_result list) -> (learned : int) -> (literal : literal) ->
    (trail : literal list) -> (pivot : Bigint.t) -> (binding : binding) ->
    {u : unit | if binding_order formula database learned trail pivot binding
      then
      binding_order formula database learned (literal :: trail) pivot binding
      else true} =
  fun formula database learned literal trail pivot binding ->
  binding_order_def formula database learned trail pivot binding;
  binding_order_def formula database learned (literal :: trail) pivot binding;
  match implied_reason binding.reason with
  | None -> ()
  | Some source -> match reason_source_clause formula database source with
    | None -> ()
    | Some clause -> earlier_clause_cons literal trail pivot clause

let rec (reason_order_cons @ total) : (formula : formula) ->
    (database : proof_result list) -> (learned : int) ->
    (bindings : binding option list) -> (literal : literal) ->
    (whole : literal list) -> (remaining : literal list) ->
    {u : unit | if reason_order_from formula database learned bindings whole
        remaining then reason_order_from formula database learned bindings
        (literal :: whole) remaining else true} =
  fun formula database learned bindings literal whole remaining ->
  reason_order_from_def formula database learned bindings whole remaining;
  reason_order_from_def formula database learned bindings (literal :: whole)
    remaining;
  match remaining with
  | [] -> ()
  | first :: rest ->
    (match at bindings (variable first) with
     | Some (Some binding) -> binding_order_cons formula database learned
       literal
         whole (Bigint.of_int (variable first)) binding
     | Some None | None -> ());
    reason_order_cons formula database learned bindings literal whole rest

let rec (reason_order_set @ total) : (formula : formula) ->
    (database : proof_result list) -> (learned : int) ->
    (bindings : binding option list) -> (index : int) -> (entry : binding) ->
    (whole : literal list) -> (remaining : literal list) ->
    {u : unit | if reason_order_from formula database learned bindings whole
        remaining && trail_consistent bindings remaining
        && at bindings index === Some None then
      reason_order_from formula database learned
        (Vox_sequence.set bindings (Bigint.of_int index) (Some entry)) whole
        remaining else true} =
  fun formula database learned bindings index entry whole remaining ->
  let next = Vox_sequence.set bindings (Bigint.of_int index) (Some entry) in
  reason_order_from_def formula database learned bindings whole remaining;
  reason_order_from_def formula database learned next whole remaining;
  trail_consistent_def bindings remaining;
  match remaining with
  | [] -> ()
  | literal :: rest ->
    let v = variable literal in
    at_def bindings index;
    at_def bindings v;
    at_def next v;
    binding_at_set bindings (Bigint.of_int index) (Some entry) (Bigint.of_int
      v);
    reason_order_set formula database learned bindings index entry whole rest

let rec (reason_clause_earlier @ total) : (bindings : binding option list) ->
    (trail : literal list) -> (forced : literal) -> (level : int) ->
    (value : bool) -> (clause : literal list) ->
    {u : unit | if trail_covers bindings trail && reason_clause bindings
        (Bigint.of_int (variable forced)) level value clause then
      earlier_clause (forced :: trail) (Bigint.of_int (variable forced)) clause
      else true} =
  fun bindings trail forced level value clause ->
  let pivot = Bigint.of_int (variable forced) in
  reason_clause_def bindings pivot level value clause;
  earlier_clause_def (forced :: trail) pivot clause;
  trail_after_def pivot (forced :: trail);
  match clause with
  | [] -> ()
  | literal :: rest ->
    let query = Bigint.of_int (variable literal) in
    at_def bindings (variable literal);
    binding_at_bounds bindings query;
    trail_covers_def bindings trail;
    covered_member bindings trail (Vox_sequence.length bindings) query;
    reason_clause_earlier bindings trail forced level value rest

let (enqueue_binding_order @ total) : (formula : formula) ->
    (database : proof_result list) -> (learned : int) ->
    (bindings : binding option list) -> (trail : literal list) ->
    (literal : literal) -> (binding : binding) ->
    {u : unit | if trail_covers bindings trail && binding_reason formula
      database
        learned bindings (Bigint.of_int (variable literal)) binding then
      binding_order formula database learned (literal :: trail)
        (Bigint.of_int (variable literal)) binding else true} =
  fun formula database learned bindings trail literal binding ->
  let pivot = Bigint.of_int (variable literal) in
  binding_reason_def formula database learned bindings pivot binding;
  binding_order_def formula database learned (literal :: trail) pivot binding;
  match implied_reason binding.reason with
  | None -> ()
  | Some source -> match reason_source_clause formula database source with
    | None -> ()
    | Some clause -> reason_clause_earlier bindings trail literal binding.level
        binding.value clause

let (reason_order_enqueue @ total) : (formula : formula) ->
    (database : proof_result list) -> (learned : int) ->
    (bindings : binding option list) -> (trail : literal list) ->
    (literal : literal) -> (binding : binding) ->
    {u : unit | if reason_order formula database learned bindings trail
        && trail_consistent bindings trail
        && at bindings (variable literal) === Some None
        && binding_order formula database learned (literal :: trail)
          (Bigint.of_int (variable literal)) binding then
      reason_order formula database learned
        (Vox_sequence.set bindings (Bigint.of_int (variable literal))
          (Some binding)) (literal :: trail) else true} =
  fun formula database learned bindings trail literal binding ->
  let index = Bigint.of_int (variable literal) in
  let next = Vox_sequence.set bindings index (Some binding) in
  reason_order_def formula database learned bindings trail;
  reason_order_def formula database learned next (literal :: trail);
  reason_order_from_def formula database learned next (literal :: trail)
    (literal :: trail);
  reason_order_cons formula database learned bindings literal trail trail;
  reason_order_set formula database learned bindings (variable literal) binding
    (literal :: trail) trail;
  at_def bindings (variable literal);
  at_def next (variable literal);
  binding_at_set bindings index (Some binding) index

let (binding_order_prepend @ total) : (formula : formula) ->
    (database : proof_result list) -> (learned : int) ->
    (entry : proof_result) -> (trail : literal list) ->
    (index : Bigint.t) -> (binding : binding) ->
    {u : unit | if reason_source_valid formula database learned binding.reason
        && binding_order formula database learned trail index binding then
      binding_order formula (entry :: database) (learned + 1) trail index
        binding else true} =
  fun formula database learned entry trail index binding ->
  reason_source_valid_def formula database learned binding.reason;
  binding_order_def formula database learned trail index binding;
  binding_order_def formula (entry :: database) (learned + 1) trail index
    binding;
  implied_reason_def binding.reason;
  match implied_reason binding.reason with
  | None -> ()
  | Some source ->
    reason_source_clause_def formula database source;
    reason_source_clause_def formula (entry :: database) source

let rec (reason_order_prepend_from @ total) : (formula : formula) ->
    (database : proof_result list) -> (learned : int) ->
    (entry : proof_result) -> (bindings : binding option list) ->
    (whole : literal list) -> (remaining : literal list) ->
    {u : unit | if reason_sources_valid formula database learned bindings
        && reason_order_from formula database learned bindings whole remaining
      then reason_order_from formula (entry :: database) (learned + 1) bindings
        whole remaining else true} =
  fun formula database learned entry bindings whole remaining ->
  reason_order_from_def formula database learned bindings whole remaining;
  reason_order_from_def formula (entry :: database) (learned + 1) bindings whole
    remaining;
  match remaining with
  | [] -> ()
  | literal :: rest ->
    let v = variable literal in
    at_def bindings v;
    reason_sources_at formula database learned bindings (Bigint.of_int v);
    (match at bindings v with
     | Some (Some binding) -> binding_order_prepend formula database learned
         entry whole (Bigint.of_int v) binding
     | Some None | None -> ());
    reason_order_prepend_from formula database learned entry bindings
      whole
      rest

let (reason_order_prepend @ total) : (formula : formula) ->
    (database : proof_result list) -> (learned : int) ->
    (entry : proof_result) -> (bindings : binding option list) ->
    (trail : literal list) ->
    {u : unit | if reason_sources_valid formula database learned bindings
        && reason_order formula database learned bindings trail then
      reason_order formula (entry :: database) (learned + 1) bindings trail
      else true} =
  fun formula database learned entry bindings trail ->
  reason_order_def formula database learned bindings trail;
  reason_order_def formula (entry :: database) (learned + 1) bindings trail;
  reason_order_prepend_from formula database learned entry bindings trail
    trail

let[@def] rec retained_bindings target (bindings : binding option list) =
  match bindings with
  | [] -> []
  | first :: rest ->
    (match first with
     | Some binding when binding.level <= target -> Some binding
     | Some _ | None -> None) :: retained_bindings target rest

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

let[@def] prefix_consistent formula database bindings level =
  if level <= 0 then true
  else
    let partial = binding_values (retained_bindings (level - 1) bindings) in
    no_conflict partial formula && no_conflict partial (database_clauses
      database)

let rec (retained_clear_set @ total) : (target : int) ->
    (bindings : binding option list) -> (index : Bigint.t) ->
    (binding : binding) ->
    {u : unit | if target < binding.level
        && Vox_sequence.at bindings index === Some None then
      retained_bindings target (Vox_sequence.set bindings index (Some binding))
        === retained_bindings target bindings else true} =
  fun target bindings index binding ->
  retained_bindings_def target bindings;
  Vox_sequence.at_def bindings index;
  Vox_sequence.set_def bindings index (Some binding);
  match bindings with
  | [] -> ()
  | first :: rest ->
    if Bigint.equal index 0Z then
      retained_bindings_def target (Some binding :: rest)
    else (
      retained_clear_set target rest (Bigint.sub index 1Z) binding;
      retained_bindings_def target
        (first :: Vox_sequence.set rest (Bigint.sub index 1Z) (Some binding)))

let rec (retained_compose @ total) : (low : int) -> (high : int) ->
    (bindings : binding option list) ->
    {u : unit | if low <= high then
      retained_bindings low (retained_bindings high bindings)
        === retained_bindings low bindings else true} =
  fun low high bindings ->
  retained_bindings_def high bindings;
  retained_bindings_def low bindings;
  retained_bindings_def low (retained_bindings high bindings);
  match bindings with
  | [] -> ()
  | _ :: rest -> retained_compose low high rest

let rec (retained_bounded @ total) : (level : int) ->
    (bindings : binding option list) ->
    {u : unit | if levels_bounded level bindings then
      retained_bindings level bindings === bindings else true} =
  fun level bindings ->
  levels_bounded_def level bindings;
  retained_bindings_def level bindings;
  match bindings with
  | [] -> ()
  | _ :: rest -> retained_bounded level rest

let rec (retained_lookup_order @ total) : (low : int) -> (high : int) ->
    (bindings : binding option list) -> (index : int) ->
    {u : unit | if low <= high then
      match partial_lookup (binding_values (retained_bindings low bindings))
          index with
      | None -> true
      | Some value -> partial_lookup
          (binding_values (retained_bindings high bindings)) index === Some
            value
      else true} =
  fun low high bindings index ->
  retained_bindings_def low bindings;
  retained_bindings_def high bindings;
  binding_values_def (retained_bindings low bindings);
  binding_values_def (retained_bindings high bindings);
  partial_lookup_def (binding_values (retained_bindings low bindings)) index;
  partial_lookup_def (binding_values (retained_bindings high bindings)) index;
  match bindings with
  | [] -> ()
  | _ :: rest ->
    if index <> 0 then retained_lookup_order low high rest (index - 1);
    ()

let rec (retained_false_order @ total) : (low : int) -> (high : int) ->
    (bindings : binding option list) -> (clause : literal list) ->
    {u : unit | if low <= high && false_clause
        (binding_values (retained_bindings low bindings)) clause then
      false_clause (binding_values (retained_bindings high bindings)) clause
      else true} =
  fun low high bindings clause ->
  let lower = binding_values (retained_bindings low bindings) in
  let upper = binding_values (retained_bindings high bindings) in
  false_clause_def lower clause;
  false_clause_def upper clause;
  match clause with
  | [] -> ()
  | literal :: rest ->
    variable_def literal;
    partial_literal_def lower literal;
    partial_literal_def upper literal;
    retained_lookup_order low high bindings (variable literal);
    retained_false_order low high bindings rest

let rec (retained_no_conflict @ total) : (low : int) -> (high : int) ->
    (bindings : binding option list) -> (formula : formula) ->
    {u : unit | if low <= high && no_conflict
        (binding_values (retained_bindings high bindings)) formula then
      no_conflict (binding_values (retained_bindings low bindings)) formula
      else true} =
  fun low high bindings formula ->
  no_conflict_def (binding_values (retained_bindings low bindings)) formula;
  no_conflict_def (binding_values (retained_bindings high bindings)) formula;
  match formula with
  | [] -> ()
  | clause :: rest ->
    retained_false_order low high bindings clause;
    retained_no_conflict low high bindings rest

let (prefix_backtrack @ total) : (formula : formula) ->
    (database : proof_result list) -> (bindings : binding option list) ->
    (level : int) -> (target : int) ->
    {u : unit | if 0 <= target && target <= level
        && prefix_consistent formula database bindings level then
      prefix_consistent formula database (retained_bindings target bindings)
        target else true} =
  fun formula database bindings level target ->
  prefix_consistent_def formula database bindings level;
  prefix_consistent_def formula database (retained_bindings target bindings)
    target;
  if 0 < target then (
    retained_compose (target - 1) target bindings;
    retained_no_conflict (target - 1) (level - 1) bindings formula;
    retained_no_conflict (target - 1) (level - 1) bindings
      (database_clauses database));
  ()

let (prefix_decision @ total) : (formula : formula) ->
    (database : proof_result list) -> (bindings : binding option list) ->
    (level : int) ->
    {u : unit | if levels_bounded level bindings
        && scan_formula (binding_values bindings) formula === Scan_stable
        && scan_formula (binding_values bindings) (database_clauses database)
          === Scan_stable then
      prefix_consistent formula database bindings (level + 1) else true} =
  fun formula database bindings level ->
  prefix_consistent_def formula database bindings (level + 1);
  retained_bounded level bindings;
  scan_stable_no_conflict (binding_values bindings) formula;
  scan_stable_no_conflict (binding_values bindings) (database_clauses database)

let[@def] rec prefix_stable formula database bindings level =
  if level <= 0 then true
  else
    let partial = binding_values (retained_bindings (level - 1) bindings) in
    formula_stable partial formula
    && formula_stable partial (database_clauses database)
    && prefix_stable formula database bindings (level - 1)
[@@decreases level]

let rec (prefix_stable_set @ total) : (formula : formula) ->
    (database : proof_result list) -> (bindings : binding option list) ->
    (level : int) -> (index : Bigint.t) -> (binding : binding) ->
    {u : unit | if level <= binding.level
        && Vox_sequence.at bindings index === Some None
        && prefix_stable formula database bindings level then
      prefix_stable formula database
        (Vox_sequence.set bindings index (Some binding)) level else true} =
  fun formula database bindings level index binding ->
  prefix_stable_def formula database bindings level;
  prefix_stable_def formula database
    (Vox_sequence.set bindings index (Some binding)) level;
  if 0 < level then (
    retained_clear_set (level - 1) bindings index binding;
    prefix_stable_set formula database bindings (level - 1) index binding);
  ()
[@@decreases level]

let rec (prefix_stable_weaken @ total) : (formula : formula) ->
    (database : proof_result list) -> (bindings : binding option list) ->
    (level : int) -> (target : int) ->
    {u : unit | if 0 <= target && target <= level
        && prefix_stable formula database bindings level then
      prefix_stable formula database bindings target else true} =
  fun formula database bindings level target ->
  prefix_stable_def formula database bindings level;
  if 0 <= target && target < level then
    prefix_stable_weaken formula database bindings (level - 1) target;
  ()
[@@decreases level]

let rec (prefix_stable_retained @ total) : (formula : formula) ->
    (database : proof_result list) -> (bindings : binding option list) ->
    (target : int) -> (level : int) ->
    {u : unit | if level <= target
        && prefix_stable formula database bindings level then
      prefix_stable formula database (retained_bindings target bindings) level
      else true} =
  fun formula database bindings target level ->
  prefix_stable_def formula database bindings level;
  prefix_stable_def formula database (retained_bindings target bindings) level;
  if 0 < level then (
    retained_compose (level - 1) target bindings;
    prefix_stable_retained formula database bindings target (level - 1));
  ()
[@@decreases level]

let (prefix_stable_backtrack @ total) : (formula : formula) ->
    (database : proof_result list) -> (bindings : binding option list) ->
    (level : int) -> (target : int) ->
    {u : unit | if 0 <= target && target <= level
        && prefix_stable formula database bindings level then
      prefix_stable formula database (retained_bindings target bindings) target
      else true} =
  fun formula database bindings level target ->
  prefix_stable_weaken formula database bindings level target;
  prefix_stable_retained formula database bindings target target

let (prefix_stable_decision @ total) : (formula : formula) ->
    (database : proof_result list) -> (bindings : binding option list) ->
    (level : int) ->
    {u : unit | if levels_bounded level bindings
        && prefix_stable formula database bindings level
        && scan_formula (binding_values bindings) formula === Scan_stable
        && scan_formula (binding_values bindings) (database_clauses database)
          === Scan_stable then
      prefix_stable formula database bindings (level + 1) else true} =
  fun formula database bindings level ->
  prefix_stable_def formula database bindings (level + 1);
  retained_bounded level bindings;
  scan_stable_formula (binding_values bindings) formula;
  scan_stable_formula (binding_values bindings) (database_clauses database)

let rec (prefix_stable_at @ total) : (formula : formula) ->
    (database : proof_result list) -> (bindings : binding option list) ->
    (level : int) -> (target : int) ->
    {u : unit | if 0 <= target && target < level
        && prefix_stable formula database bindings level then
      formula_stable (binding_values (retained_bindings target bindings))
        formula
      && formula_stable (binding_values (retained_bindings target bindings))
        (database_clauses database) else true} =
  fun formula database bindings level target ->
  prefix_stable_def formula database bindings level;
  if 0 < level && 0 <= target && target < level - 1 then
    prefix_stable_at formula database bindings (level - 1) target;
  ()
[@@decreases level]

let (enqueue @ total) (formula : formula @ ghost)
    (database : proof_result list @ ghost) (state : state @ immutable total)
      literal reason :
    {result : state option | match result with
      | None -> not (at state.bindings (variable literal) === Some None)
      | Some next -> next.learned = state.learned && next.level = state.level
        && next.steps = state.steps
        && (if prefix_stable formula database state.bindings state.level then
          prefix_stable formula database next.bindings next.level else true)
        && (if prefix_consistent formula database state.bindings state.level
          then
          prefix_consistent formula database next.bindings next.level else true)
        && (if 0 <= state.level && levels_bounded state.level state.bindings
            then levels_bounded next.level next.bindings else true)
        && Vox_sequence.length next.bindings ===
          Vox_sequence.length state.bindings
        && (if at state.bindings (variable literal) === Some None then
          unassigned next.bindings ===
            Bigint.sub (unassigned state.bindings) 1Z else true)
        && (if at state.bindings (variable literal) === Some None
            && 0 <= state.level
            && (match reason with Decision -> 0 < state.level
                | Original _ | Learned _ -> true)
            && trail_levels state.bindings state.trail
              (match reason with Decision -> state.level - 1
               | Original _ | Learned _ -> state.level) then
          trail_levels next.bindings next.trail next.level else true)
        && (if 0 <= state.level
            && (match reason with Decision -> 0 < state.level
                | Original _ | Learned _ -> true)
            && trail_ordered state.bindings state.trail
              (match reason with Decision -> state.level - 1
               | Original _ | Learned _ -> state.level) then
          trail_ordered next.bindings next.trail next.level else true)
        && (if trail_consistent state.bindings state.trail then
          trail_consistent next.bindings next.trail else true)
        && (if trail_consistent state.bindings state.trail
            && trail_unique state.trail then trail_unique next.trail
          else true)
        && (if trail_covers state.bindings state.trail then
          trail_covers next.bindings next.trail else true)
        && (if reason_order formula database state.learned state.bindings
          state.trail
            && trail_consistent state.bindings state.trail
            && trail_covers state.bindings state.trail
            && enqueue_reason formula database state.learned state.bindings
              literal state.level reason then
          reason_order formula database next.learned next.bindings next.trail
          else true)
        && (if reason_semantics formula database state.learned state.bindings
            && enqueue_reason formula database state.learned state.bindings
              literal state.level reason then
          reason_semantics formula database next.learned next.bindings
          else true)
        && (if reason_sources_valid formula database state.learned
          state.bindings
            && reason_source_valid formula database state.learned reason then
          reason_sources_valid formula database state.learned next.bindings
          else true)} =
  let v = variable literal in
  ghost_ (at_def state.bindings v);
  ghost_ (ordered_weaken state.bindings state.trail (state.level - 1)
    state.level);
  match at state.bindings v with
  | None -> None
  | Some (Some existing) ->
    if existing.value = wanted literal then Some state else None
  | Some None ->
    let binding = {value = wanted literal; level = state.level; reason} in
    let bindings = set_at state.bindings v (Some binding) in
    ghost_ (
      prefix_stable_set formula database state.bindings state.level
        (Bigint.of_int v) binding;
      prefix_consistent_def formula database state.bindings state.level;
      prefix_consistent_def formula database bindings state.level;
      if 0 < state.level then
        retained_clear_set (state.level - 1) state.bindings (Bigint.of_int v)
          binding;
      enqueue_reason_def formula database state.learned state.bindings
        literal state.level reason;
      binding_reason_def formula database state.learned state.bindings
        (Bigint.of_int v) binding;
      enqueue_binding_order formula database state.learned state.bindings
        state.trail literal binding;
      reason_order_enqueue formula database state.learned state.bindings
        state.trail literal binding;
      reason_semantics_set formula database state.learned state.bindings
        (Bigint.of_int v) binding;
      preceding_level_def binding;
      trail_levels_set state.bindings state.trail (preceding_level binding)
        v binding;
      trail_levels_def bindings (literal :: state.trail) state.level;
      ordered_set state.bindings state.trail (preceding_level binding)
        v binding;
      trail_ordered_def bindings (literal :: state.trail) state.level;
      consistent_absent state.bindings state.trail v;
      consistent_set state.bindings state.trail v binding;
      trail_unique_def (literal :: state.trail);
      binding_at_set state.bindings (Bigint.of_int v) (Some binding)
        (Bigint.of_int v);
      at_def bindings v;
      trail_consistent_def bindings (literal :: state.trail);
      levels_set state.bindings state.level (Bigint.of_int v) binding;
      reason_sources_set formula database state.learned state.bindings
        (Bigint.of_int v) binding;
      trail_covers_def state.bindings state.trail;
      covered_set state.bindings state.trail
        (Vox_sequence.length state.bindings) literal binding;
      trail_covers_def bindings (literal :: state.trail));
    Some {state with bindings; trail = literal :: state.trail}

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

let rec (retained_trail_levels @ total) : (target : int) ->
    (bindings : binding option list) -> (trail : literal list) -> (level : int)
      ->
    {u : unit | if 0 <= target && 0 <= level
        && trail_levels bindings trail level then
      trail_levels (retained_bindings target bindings)
        (retain_trail target bindings trail)
        (if target < level then target else level) else true} =
  fun target bindings trail level ->
  let retained = retained_bindings target bindings in
  trail_levels_def bindings trail level;
  retain_trail_def target bindings trail;
  match trail with
  | [] -> trail_levels_def retained [] (if target < level then target else
    level)
  | literal :: rest ->
    let query = variable literal in
    at_def bindings query;
    retained_binding_at target bindings (Bigint.of_int query);
    at_def retained query;
    (match at bindings query with
     | None | Some None -> ()
     | Some (Some binding) ->
       (match binding.reason with
        | Decision ->
          if 0 < level then
            retained_trail_levels target bindings rest (level - 1)
        | Original _ | Learned _ -> retained_trail_levels target bindings rest
          level);
       trail_levels_def retained
         (literal :: retain_trail target bindings rest)
         (if target < level then target else level));
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

let rec (retained_trail_subset @ total) : (target : int) ->
    (bindings : binding option list) -> (trail : literal list) ->
    (index : Bigint.t) ->
    {u : unit | if trail_has index (retain_trail target bindings trail) then
      trail_has index trail else true} =
  fun target bindings trail index ->
  retain_trail_def target bindings trail;
  trail_has_def index trail;
  match trail with
  | [] -> ()
  | literal :: rest ->
    trail_has_def index (literal :: retain_trail target bindings rest);
    retained_trail_subset target bindings rest index

let rec (retained_trail_unique @ total) : (target : int) ->
    (bindings : binding option list) -> (trail : literal list) ->
    {u : unit | if trail_unique trail then
      trail_unique (retain_trail target bindings trail) else true} =
  fun target bindings trail ->
  retain_trail_def target bindings trail;
  trail_unique_def trail;
  match trail with
  | [] -> ()
  | literal :: rest ->
    trail_unique_def (literal :: retain_trail target bindings rest);
    retained_trail_subset target bindings rest
      (Bigint.of_int (variable literal));
    retained_trail_unique target bindings rest

let rec (retained_trail_consistent @ total) : (target : int) ->
    (bindings : binding option list) -> (trail : literal list) ->
    {u : unit | if trail_consistent bindings trail then
      trail_consistent (retained_bindings target bindings)
        (retain_trail target bindings trail) else true} =
  fun target bindings trail ->
  retain_trail_def target bindings trail;
  trail_consistent_def bindings trail;
  match trail with
  | [] -> trail_consistent_def (retained_bindings target bindings) []
  | literal :: rest ->
    let v = variable literal in
    at_def bindings v;
    at_def (retained_bindings target bindings) v;
    retained_binding_at target bindings (Bigint.of_int v);
    trail_consistent_def (retained_bindings target bindings)
      (literal :: retain_trail target bindings rest);
    retained_trail_consistent target bindings rest

let rec (retained_trail_ordered @ total) : (target : int) ->
    (bindings : binding option list) -> (trail : literal list) ->
    (upper : int) ->
    {u : unit | if trail_ordered bindings trail upper then
      trail_ordered (retained_bindings target bindings)
        (retain_trail target bindings trail)
        (if target < upper then target else upper) else true} =
  fun target bindings trail upper ->
  let next = retained_bindings target bindings in
  let bound = if target < upper then target else upper in
  retain_trail_def target bindings trail;
  trail_ordered_def bindings trail upper;
  match trail with
  | [] -> trail_ordered_def next [] bound
  | literal :: rest ->
    let v = variable literal in
    at_def bindings v;
    at_def next v;
    retained_binding_at target bindings (Bigint.of_int v);
    trail_ordered_def next (literal :: retain_trail target bindings rest)
      bound;
    match at bindings v with
    | Some (Some binding) ->
      preceding_level_def binding;
      retained_trail_ordered target bindings rest (preceding_level binding);
      ordered_weaken next (retain_trail target bindings rest)
        (if target < preceding_level binding then target
         else preceding_level binding) bound
    | Some None | None -> ()

let rec (reason_clause_retained @ total) :
    (bindings : binding option list) -> (target : int) -> (pivot : Bigint.t) ->
    (level : int) -> (value : bool) -> (clause : literal list) ->
    {u : unit | if reason_clause bindings pivot level value clause
        && level <= target then
      reason_clause (retained_bindings target bindings) pivot level value clause
      else true} =
  fun bindings target pivot level value clause ->
  let next = retained_bindings target bindings in
  reason_clause_def bindings pivot level value clause;
  reason_clause_def next pivot level value clause;
  match clause with
  | [] -> ()
  | literal :: rest ->
    let v = variable literal in
    at_def bindings v;
    at_def next v;
    retained_binding_at target bindings (Bigint.of_int v);
    reason_clause_retained bindings target pivot level value rest

let (binding_reason_retained @ total) : (formula : formula) ->
    (database : proof_result list) -> (learned : int) ->
    (bindings : binding option list) -> (target : int) -> (pivot : Bigint.t) ->
    (binding : binding) ->
    {u : unit | if binding_reason formula database learned bindings pivot
        binding && binding.level <= target then
      binding_reason formula database learned (retained_bindings target
        bindings)
        pivot binding else true} =
  fun formula database learned bindings target pivot binding ->
  binding_reason_def formula database learned bindings pivot binding;
  binding_reason_def formula database learned (retained_bindings target
    bindings)
    pivot binding;
  match implied_reason binding.reason with
  | None -> ()
  | Some source -> match reason_source_clause formula database source with
    | None -> ()
    | Some clause -> reason_clause_retained bindings target pivot
        binding.level binding.value clause

let rec (reason_slots_retained @ total) : (formula : formula) ->
    (database : proof_result list) -> (learned : int) ->
    (bindings : binding option list) -> (target : int) -> (offset : Bigint.t) ->
    (remaining : binding option list) ->
    {u : unit | if reason_semantics_from formula database learned bindings
        offset remaining then
      reason_semantics_from formula database learned
        (retained_bindings target bindings) offset
        (retained_bindings target remaining) else true} =
  fun formula database learned bindings target offset remaining ->
  reason_semantics_from_def formula database learned bindings offset remaining;
  retained_bindings_def target remaining;
  reason_semantics_from_def formula database learned
    (retained_bindings target bindings) offset
    (retained_bindings target remaining);
  match remaining with
  | [] -> ()
  | first :: rest ->
    (match first with
     | None -> ()
     | Some binding -> binding_reason_retained formula database learned
         bindings target offset binding);
    reason_slots_retained formula database learned bindings target
      (Bigint.add offset 1Z) rest

let (reason_semantics_retained @ total) : (formula : formula) ->
    (database : proof_result list) -> (learned : int) ->
    (bindings : binding option list) -> (target : int) ->
    {u : unit | if reason_semantics formula database learned bindings then
      reason_semantics formula database learned
        (retained_bindings target bindings) else true} =
  fun formula database learned bindings target ->
  reason_semantics_def formula database learned bindings;
  reason_semantics_def formula database learned (retained_bindings target
    bindings);
  reason_slots_retained formula database learned bindings target 0Z bindings

let rec (trail_after_retained @ total) : (target : int) ->
    (bindings : binding option list) -> (trail : literal list) ->
    (pivot : Bigint.t) ->
    {u : unit | if (match Vox_sequence.at bindings pivot with
        | Some (Some binding) -> binding.level <= target
        | Some None | None -> false) then
      trail_after pivot (retain_trail target bindings trail) ===
        retain_trail target bindings (trail_after pivot trail) else true} =
  fun target bindings trail pivot ->
  trail_after_def pivot trail;
  retain_trail_def target bindings trail;
  trail_after_def pivot (retain_trail target bindings trail);
  match trail with
  | [] -> ()
  | literal :: rest ->
    at_def bindings (variable literal);
    trail_after_def pivot (literal :: retain_trail target bindings rest);
    trail_after_retained target bindings rest pivot

let rec (earlier_clause_retained @ total) : (target : int) ->
    (bindings : binding option list) -> (trail : literal list) ->
    (pivot : Bigint.t) -> (level : int) -> (value : bool) ->
    (clause : literal list) ->
    {u : unit | if earlier_clause trail pivot clause
        && reason_clause bindings pivot level value clause && level <= target
        && (match Vox_sequence.at bindings pivot with
          | Some (Some binding) -> binding.level <= target
          | Some None | None -> false) then
      earlier_clause (retain_trail target bindings trail) pivot clause
      else true} =
  fun target bindings trail pivot level value clause ->
  earlier_clause_def trail pivot clause;
  earlier_clause_def (retain_trail target bindings trail) pivot clause;
  reason_clause_def bindings pivot level value clause;
  trail_after_retained target bindings trail pivot;
  match clause with
  | [] -> ()
  | literal :: rest ->
    let index = Bigint.of_int (variable literal) in
    at_def bindings (variable literal);
    retained_trail_member target bindings (trail_after pivot trail) index;
    earlier_clause_retained target bindings trail pivot level value rest

let (binding_order_retained @ total) : (formula : formula) ->
    (database : proof_result list) -> (learned : int) -> (target : int) ->
    (bindings : binding option list) -> (trail : literal list) ->
    (pivot : Bigint.t) -> (binding : binding) ->
    {u : unit | if binding_order formula database learned trail pivot binding
        && binding_reason formula database learned bindings pivot binding
        && Vox_sequence.at bindings pivot === Some (Some binding)
        && binding.level <= target then
      binding_order formula database learned (retain_trail target bindings
        trail)
        pivot binding else true} =
  fun formula database learned target bindings trail pivot binding ->
  binding_order_def formula database learned trail pivot binding;
  binding_order_def formula database learned (retain_trail target bindings
    trail)
    pivot binding;
  binding_reason_def formula database learned bindings pivot binding;
  match implied_reason binding.reason with
  | None -> ()
  | Some source -> match reason_source_clause formula database source with
    | None -> ()
    | Some clause -> earlier_clause_retained target bindings trail pivot
        binding.level binding.value clause

let rec (reason_order_retained_from @ total) : (formula : formula) ->
    (database : proof_result list) -> (learned : int) -> (target : int) ->
    (bindings : binding option list) -> (whole : literal list) ->
    (remaining : literal list) ->
    {u : unit | if reason_order_from formula database learned bindings whole
        remaining && reason_semantics formula database learned bindings then
      reason_order_from formula database learned (retained_bindings target
        bindings)
        (retain_trail target bindings whole)
        (retain_trail target bindings remaining) else true} =
  fun formula database learned target bindings whole remaining ->
  let next = retained_bindings target bindings in
  reason_order_from_def formula database learned bindings whole remaining;
  reason_order_from_def formula database learned next
    (retain_trail target bindings whole) (retain_trail target bindings
      remaining);
  retain_trail_def target bindings remaining;
  match remaining with
  | [] -> ()
  | literal :: rest ->
    let v = variable literal in
    at_def bindings v;
    at_def next v;
    retained_binding_at target bindings (Bigint.of_int v);
    reason_order_from_def formula database learned next
      (retain_trail target bindings whole)
      (literal :: retain_trail target bindings rest);
    reason_semantics_at formula database learned bindings (Bigint.of_int v);
    (match at bindings v with
     | Some (Some binding) -> binding_order_retained formula database learned
         target bindings whole (Bigint.of_int v) binding
     | Some None | None -> ());
    reason_order_retained_from formula database learned target bindings whole
      rest

let (reason_order_retained @ total) : (formula : formula) ->
    (database : proof_result list) -> (learned : int) -> (target : int) ->
    (bindings : binding option list) -> (trail : literal list) ->
    {u : unit | if reason_order formula database learned bindings trail
        && reason_semantics formula database learned bindings then
      reason_order formula database learned (retained_bindings target bindings)
        (retain_trail target bindings trail) else true} =
  fun formula database learned target bindings trail ->
  reason_order_def formula database learned bindings trail;
  reason_order_def formula database learned (retained_bindings target bindings)
    (retain_trail target bindings trail);
  reason_order_retained_from formula database learned target bindings trail
    trail

let (backtrack @ total) (formula : formula @ ghost)
    (database : proof_result list @ ghost) (state : state @ immutable total)
      target :
    {next : state | next.learned = state.learned && next.level = target
      && next.steps = state.steps
      && (if 0 <= target && target <= state.level
          && trail_levels state.bindings state.trail state.level then
        trail_levels next.bindings next.trail target else true)
      && (if 0 <= target && target <= state.level
          && prefix_stable formula database state.bindings state.level then
        prefix_stable formula database next.bindings target else true)
      && next.bindings === retained_bindings target state.bindings
      && (if 0 <= target && target <= state.level
          && prefix_consistent formula database state.bindings state.level then
        prefix_consistent formula database next.bindings target else true)
      && (if levels_bounded state.level state.bindings then
        levels_bounded target next.bindings else true)
      && Vox_sequence.length next.bindings ===
      Vox_sequence.length state.bindings
      && (if trail_ordered state.bindings state.trail state.level then
        trail_ordered next.bindings next.trail target else true)
      && (if trail_consistent state.bindings state.trail then
          trail_consistent next.bindings next.trail else true)
        && (if trail_consistent state.bindings state.trail
            && trail_unique state.trail then trail_unique next.trail
          else true)
        && (if trail_covers state.bindings state.trail then
        trail_covers next.bindings next.trail else true)
      && (if reason_order formula database state.learned state.bindings
        state.trail
          && reason_semantics formula database state.learned state.bindings then
        reason_order formula database next.learned next.bindings next.trail
        else true)
      && (if reason_semantics formula database state.learned state.bindings then
        reason_semantics formula database next.learned next.bindings else true)
      && (if reason_sources_valid formula database state.learned state.bindings
          then reason_sources_valid formula database state.learned next.bindings
          else true)} =
  let bindings = retain_bindings target state.bindings in
  let trail = retain_trail target state.bindings state.trail in
  ghost_ (
    trail_levels_bound state.bindings state.trail state.level;
    retained_trail_levels target state.bindings state.trail state.level;
    prefix_backtrack formula database state.bindings state.level target;
    prefix_stable_backtrack formula database state.bindings state.level target;
    reason_order_retained formula database state.learned target state.bindings
      state.trail;
    reason_semantics_retained formula database state.learned state.bindings
      target;
    retained_trail_ordered target state.bindings state.trail state.level;
    ordered_weaken bindings trail
      (if target < state.level then target else state.level) target;
    retained_trail_unique target state.bindings state.trail;
    retained_trail_consistent target state.bindings state.trail;
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

let[@def] rec unique_variables variables =
  match variables with
  | [] -> true
  | first :: rest -> not (has_int first rest) && unique_variables rest

let[@def] rec variables_covered trail variables =
  match variables with
  | [] -> true
  | first :: rest -> trail_has (Bigint.of_int first) trail
    && variables_covered trail rest

let rec (current_variables_covered @ total) :
    (bindings : binding option list) -> (trail : literal list) ->
    (level : int) -> (variables : int list) ->
    {u : unit | if trail_covers bindings trail
        && all_current variables bindings level then
      variables_covered trail variables else true} =
  fun bindings trail level variables ->
  all_current_def variables bindings level;
  variables_covered_def trail variables;
  match variables with
  | [] -> ()
  | first :: rest ->
    at_def bindings first;
    binding_at_bounds bindings (Bigint.of_int first);
    trail_covers_def bindings trail;
    covered_member bindings trail (Vox_sequence.length bindings)
      (Bigint.of_int first);
    current_variables_covered bindings trail level rest

let rec (covered_variable @ total) : (trail : literal list) ->
    (variables : int list) -> (v : int) ->
    {u : unit | if variables_covered trail variables && has_int v variables
      then trail_has (Bigint.of_int v) trail else true} =
  fun trail variables v ->
  variables_covered_def trail variables;
  has_int_def v variables;
  match variables with
  | [] -> ()
  | first :: rest -> if first <> v then covered_variable trail rest v; ()

let rec (covered_skip @ total) : (literal : literal) ->
    (trail : literal list) -> (variables : int list) ->
    {u : unit | if variables_covered (literal :: trail) variables
        && not (has_int (variable literal) variables) then
      variables_covered trail variables else true} =
  fun literal trail variables ->
  variables_covered_def (literal :: trail) variables;
  variables_covered_def trail variables;
  has_int_def (variable literal) variables;
  match variables with
  | [] -> ()
  | first :: rest ->
    trail_has_def (Bigint.of_int first) (literal :: trail);
    covered_skip literal trail rest

let rec (ordered_member @ total) : (bindings : binding option list) ->
    (trail : literal list) -> (upper : int) -> (v : int) ->
    {u : unit | if trail_ordered bindings trail upper
        && trail_has (Bigint.of_int v) trail then
      match at bindings v with
      | Some (Some binding) -> binding.level <= upper
        && (match binding.reason with Decision -> 0 < binding.level
            | Original _ | Learned _ -> true)
      | Some None | None -> false
      else true} =
  fun bindings trail upper v ->
  trail_ordered_def bindings trail upper;
  trail_has_def (Bigint.of_int v) trail;
  match trail with
  | [] -> ()
  | literal :: rest ->
    match at bindings (variable literal) with
    | Some (Some binding) ->
      preceding_level_def binding;
      if v <> variable literal then
        ordered_member bindings rest (preceding_level binding) v;
      ()
    | Some None | None -> ()

let (other_below_decision @ total) : (bindings : binding option list) ->
    (literal : literal) -> (rest : literal list) -> (level : int) ->
    (other : int) ->
    {u : unit | if trail_ordered bindings (literal :: rest) level
        && trail_has (Bigint.of_int other) (literal :: rest)
        && other <> variable literal then
      match at bindings (variable literal), at bindings other with
      | Some (Some chosen), Some (Some binding) ->
        if chosen.reason === Decision && chosen.level = level then
          binding.level < level else true
      | _ -> true
      else true} =
  fun bindings literal rest level other ->
  trail_ordered_def bindings (literal :: rest) level;
  trail_has_def (Bigint.of_int other) (literal :: rest);
  match at bindings (variable literal) with
  | Some (Some chosen) ->
    preceding_level_def chosen;
    ordered_member bindings rest (preceding_level chosen) other
  | Some None | None -> ()

let (latest_not_decision @ total) : (bindings : binding option list) ->
    (literal : literal) -> (rest : literal list) -> (level : int) ->
    (variables : int list) ->
    {u : unit | if trail_ordered bindings (literal :: rest) level
        && all_current variables bindings level
        && variables_covered (literal :: rest) variables
        && unique_variables variables && has_int (variable literal) variables
        && (level = 0 || match variables with
            | _ :: _ :: _ -> true | [] | [_] -> false) then
      match at bindings (variable literal) with
      | Some (Some chosen) -> not (chosen.reason === Decision)
      | Some None | None -> false
      else true} =
  fun bindings literal rest level variables ->
  let v = variable literal in
  all_current_member variables bindings level v;
  covered_variable (literal :: rest) variables v;
  ordered_member bindings (literal :: rest) level v;
  unique_variables_def variables;
  match variables with
  | first :: second :: tail ->
    has_int_def first variables;
    has_int_def first (second :: tail);
    has_int_def second variables;
    has_int_def second (second :: tail);
    let other = if first = v then second else first in
    all_current_member variables bindings level other;
    covered_variable (literal :: rest) variables other;
    other_below_decision bindings literal rest level other
  | [] | [_] -> ()

let[@def] rec includes_variables whole part =
  match part with
  | [] -> true
  | v :: rest -> has_int v whole && includes_variables whole rest

let rec (includes_self @ total) : (variables : int list) ->
    {u : unit | includes_variables variables variables} =
  fun variables ->
  let rec (extend @ total) : (whole : int list) -> (part : int list) ->
      (v : int) -> {u : unit | if includes_variables whole part then
        includes_variables (v :: whole) part else true} =
    fun whole part v ->
    includes_variables_def whole part;
    includes_variables_def (v :: whole) part;
    match part with
    | [] -> ()
    | first :: rest ->
      has_int_def first (v :: whole);
      extend whole rest v
  in
  includes_variables_def variables variables;
  match variables with
  | [] -> ()
  | v :: rest ->
    has_int_def v variables;
    includes_self rest;
    extend rest rest v

let rec (includes_member @ total) : (whole : int list) ->
    (part : int list) -> (v : int) ->
    {u : unit | if includes_variables whole part && has_int v part then
      has_int v whole else true} =
  fun whole part v ->
  includes_variables_def whole part;
  has_int_def v part;
  match part with
  | [] -> ()
  | _ :: rest -> includes_member whole rest v

let[@def] rec current_covered variables (bindings : binding option list)
    level clause =
  match clause with
  | [] -> true
  | literal :: rest ->
    (match at bindings (variable literal) with
     | Some (Some binding) ->
       if binding.level = level then has_int (variable literal) variables
       else true
     | Some None | None -> true)
    && current_covered variables bindings level rest

let[@def] rec variable_sources variables clause seen =
  match variables with
  | [] -> true
  | v :: rest ->
    (has_int v seen || has_literal (Positive v) clause
      || has_literal (Negative v) clause)
    && variable_sources rest clause seen

let rec (variable_sources_self @ total) : (variables : int list) ->
    (seen : int list) -> (clause : literal list) ->
    {u : unit | if includes_variables seen variables then
      variable_sources variables clause seen else true} =
  fun variables seen clause ->
  includes_variables_def seen variables;
  variable_sources_def variables clause seen;
  match variables with
  | [] -> ()
  | _ :: rest -> variable_sources_self rest seen clause

let rec (variable_sources_cons @ total) : (variables : int list) ->
    (literal : literal) -> (clause : literal list) -> (seen : int list) ->
    {u : unit | if variable_sources variables clause seen
        || variable_sources variables clause (variable literal :: seen) then
      variable_sources variables (literal :: clause) seen else true} =
  fun variables literal clause seen ->
  variable_sources_def variables clause seen;
  variable_sources_def variables clause (variable literal :: seen);
  variable_sources_def variables (literal :: clause) seen;
  match variables with
  | [] -> ()
  | v :: rest ->
    has_int_def v (variable literal :: seen);
    has_literal_def (Positive v) (literal :: clause);
    has_literal_def (Negative v) (literal :: clause);
    same_literal_def (Positive v) literal;
    same_literal_def (Negative v) literal;
    variable_def literal;
    variable_sources_cons rest literal clause seen

let rec (variable_source_member @ total) : (variables : int list) ->
    (clause : literal list) -> (seen : int list) -> (v : int) ->
    {u : unit | if variable_sources variables clause seen
        && has_int v variables then
      has_int v seen || has_literal (Positive v) clause
        || has_literal (Negative v) clause else true} =
  fun variables clause seen v ->
  variable_sources_def variables clause seen;
  has_int_def v variables;
  match variables with
  | [] -> ()
  | _ :: rest -> variable_source_member rest clause seen v

let rec (current_variables @ total) : (clause : literal list) ->
    (bindings : binding option list) -> (level : int) -> (seen : int list) ->
    {variables : int list |
      variable_sources variables clause seen
      && includes_variables variables seen
      && current_covered variables bindings level clause
      && (if all_current seen bindings level then
        all_current variables bindings level else true)
      && (if unique_variables seen then unique_variables variables else true)
      && (if variables === [] then seen === []
        && (match clause with
          | [] -> true
          | literal :: _ -> match at bindings (variable literal) with
            | Some (Some binding) -> binding.level <> level
            | Some None | None -> true)
        else true)} =
  fun clause bindings level seen ->
  let variables = match clause with
  | [] -> seen
  | literal :: rest ->
    let v = variable literal in
    ghost_ (
      has_int_def v seen;
      unique_variables_def (v :: seen));
    match at bindings v with
    | Some (Some binding) when binding.level = level ->
      ghost_ (all_current_def (v :: seen) bindings level);
      current_variables rest bindings level
        (if has_int v seen then seen else v :: seen)
    | Some (Some _) | Some None | None ->
      current_variables rest bindings level seen in
  ghost_ (
    current_covered_def variables bindings level clause;
    match clause with
    | [] ->
      includes_self seen;
      variable_sources_self seen seen []
    | literal :: rest ->
      variable_sources_cons variables literal rest seen;
      includes_variables_def variables (variable literal :: seen);
      includes_member variables seen (variable literal));
  variables

let[@def] rec earlier_variables trail pivot variables =
  match variables with
  | [] -> true
  | v :: rest ->
    (Bigint.equal (Bigint.of_int v) pivot
      || trail_has (Bigint.of_int v) (trail_after pivot trail))
    && earlier_variables trail pivot rest

let rec (latest_variables @ total) : (literal : literal) ->
    (trail : literal list) -> (variables : int list) ->
    {u : unit | if variables_covered (literal :: trail) variables then
      earlier_variables (literal :: trail)
        (Bigint.of_int (variable literal)) variables else true} =
  fun literal trail variables ->
  let pivot = Bigint.of_int (variable literal) in
  variables_covered_def (literal :: trail) variables;
  earlier_variables_def (literal :: trail) pivot variables;
  trail_after_def pivot (literal :: trail);
  match variables with
  | [] -> ()
  | v :: rest ->
    trail_has_def (Bigint.of_int v) (literal :: trail);
    latest_variables literal trail rest

let rec (earlier_variables_cons @ total) : (literal : literal) ->
    (trail : literal list) -> (pivot : int) -> (variables : int list) ->
    {u : unit | if variable literal <> pivot
        && earlier_variables trail (Bigint.of_int pivot) variables then
      earlier_variables (literal :: trail) (Bigint.of_int pivot) variables
      else true} =
  fun literal trail pivot variables ->
  earlier_variables_def trail (Bigint.of_int pivot) variables;
  earlier_variables_def (literal :: trail) (Bigint.of_int pivot) variables;
  trail_after_def (Bigint.of_int pivot) (literal :: trail);
  match variables with
  | [] -> ()
  | _ :: rest -> earlier_variables_cons literal trail pivot rest

let rec (earlier_variables_member @ total) : (trail : literal list) ->
    (pivot : Bigint.t) -> (variables : int list) -> (v : int) ->
    {u : unit | if earlier_variables trail pivot variables
        && has_int v variables then
      Bigint.equal (Bigint.of_int v) pivot
      || trail_has (Bigint.of_int v) (trail_after pivot trail) else true} =
  fun trail pivot variables v ->
  earlier_variables_def trail pivot variables;
  has_int_def v variables;
  match variables with
  | [] -> ()
  | _ :: rest -> earlier_variables_member trail pivot rest v

let rec (find_latest @ total) :
    (bindings : binding option list) @ ghost -> (level : int) @ ghost ->
    (trail : {t : literal list | trail_ordered bindings t level}) ->
    (variables : {vs : int list | all_current vs bindings level
      && variables_covered trail vs && unique_variables vs
      && (match vs with [] -> false | _ :: _ -> true)}) ->
    {v : int | has_int v variables
      && earlier_variables trail (Bigint.of_int v) variables
      && (if level = 0 || (match variables with
          | _ :: _ :: _ -> true | [] | [_] -> false) then
        match at bindings v with
        | Some (Some binding) -> not (binding.reason === Decision)
        | Some None | None -> false
        else true)} =
  fun bindings level trail variables ->
  match trail with
  | [] ->
    ghost_ (variables_covered_def [] variables;
      match variables with
      | [] -> ()
      | first :: _ -> trail_has_def (Bigint.of_int first) []);
    let _ : {u : unit | false} = () in
    0
  | literal :: rest ->
    let v = variable literal in
    if has_int v variables then (
      ghost_ (latest_not_decision bindings literal rest level variables;
        latest_variables literal rest variables);
      v)
    else (
      ghost_ (
        covered_skip literal rest variables;
        trail_ordered_def bindings trail level;
        match at bindings v with
        | Some (Some binding) ->
          preceding_level_def binding;
          ordered_weaken bindings rest (preceding_level binding) level
        | Some None | None -> ());
      let found = find_latest (ghost_ bindings) (ghost_ level) rest variables in
      ghost_ (earlier_variables_cons literal rest found variables);
      found)

let rec (lower_level_earlier @ total) : (bindings : binding option list) ->
    (trail : literal list) -> (level : int) -> (pivot : int) -> (query : int) ->
    {u : unit | if trail_ordered bindings trail level
        && trail_has (Bigint.of_int pivot) trail
        && trail_has (Bigint.of_int query) trail then
      match at bindings pivot, at bindings query with
      | Some (Some chosen), Some (Some binding) ->
        if chosen.level = level && binding.level < level then
          trail_has (Bigint.of_int query)
            (trail_after (Bigint.of_int pivot) trail) else true
      | _ -> true
      else true} =
  fun bindings trail level pivot query ->
  trail_ordered_def bindings trail level;
  trail_has_def (Bigint.of_int pivot) trail;
  trail_has_def (Bigint.of_int query) trail;
  trail_after_def (Bigint.of_int pivot) trail;
  match trail with
  | [] -> ()
  | literal :: rest ->
    if variable literal <> pivot then (
      match at bindings (variable literal) with
      | Some (Some binding) ->
        preceding_level_def binding;
        ordered_member bindings rest (preceding_level binding) pivot;
        ordered_weaken bindings rest (preceding_level binding) level;
        lower_level_earlier bindings rest level pivot query
      | Some None | None -> ());
    ()

let rec (conflict_earlier_clause @ total) : (n : int) ->
    (bindings : binding option list) -> (trail : literal list) ->
    (level : int) -> (pivot : int) -> (variables : int list) ->
    (clause : literal list) ->
    {u : unit | if Vox_sequence.length bindings === Bigint.of_int n
        && valid_clause n clause && false_clause (binding_values bindings)
          clause
        && levels_bounded level bindings && trail_covers bindings trail
        && trail_ordered bindings trail level
        && trail_has (Bigint.of_int pivot) trail
        && current_covered variables bindings level clause
        && earlier_variables trail (Bigint.of_int pivot) variables
        && (match at bindings pivot with
          | Some (Some binding) -> binding.level = level
          | Some None | None -> false) then
      earlier_clause trail (Bigint.of_int pivot) clause else true} =
  fun n bindings trail level pivot variables clause ->
  valid_clause_def n clause;
  false_clause_def (binding_values bindings) clause;
  earlier_clause_def trail (Bigint.of_int pivot) clause;
  current_covered_def variables bindings level clause;
  match clause with
  | [] -> ()
  | literal :: rest ->
    let query = variable literal in
    valid_literal_def n literal;
    variable_def literal;
    partial_literal_def (binding_values bindings) literal;
    lookup_value bindings query;
    at_def bindings query;
    binding_at_bounds bindings (Bigint.of_int query);
    levels_at bindings level (Bigint.of_int query);
    trail_covers_def bindings trail;
    covered_member bindings trail (Vox_sequence.length bindings)
      (Bigint.of_int query);
    earlier_variables_member trail (Bigint.of_int pivot) variables query;
    lower_level_earlier bindings trail level pivot query;
    conflict_earlier_clause n bindings trail level pivot variables rest

let rec (reason_excludes @ total) : (bindings : binding option list) ->
    (pivot : int) -> (level : int) -> (value : bool) ->
    (clause : literal list) ->
    {u : unit | if reason_clause bindings (Bigint.of_int pivot) level value
        clause then
      not (has_literal (if value then Negative pivot else Positive pivot)
        clause)
      else true} =
  fun bindings pivot level value clause ->
  let opposite = if value then Negative pivot else Positive pivot in
  reason_clause_def bindings (Bigint.of_int pivot) level value clause;
  has_literal_def opposite clause;
  match clause with
  | [] -> ()
  | literal :: rest ->
    same_literal_def opposite literal;
    variable_def literal;
    wanted_def literal;
    reason_excludes bindings pivot level value rest

let rec (false_excludes @ total) : (partial : bool option list) ->
    (clause : literal list) -> (query : literal) ->
    {u : unit | if false_clause partial clause
        && partial_literal partial query === Some true then
      not (has_literal query clause) else true} =
  fun partial clause query ->
  false_clause_def partial clause;
  has_literal_def query clause;
  match clause with
  | [] -> ()
  | literal :: rest ->
    same_literal_def query literal;
    partial_literal_def partial query;
    partial_literal_def partial literal;
    false_excludes partial rest query

let rec (reason_false_except @ total) : (bindings : binding option list) ->
    (pivot : int) -> (level : int) -> (value : bool) ->
    (clause : literal list) ->
    {u : unit | if reason_clause bindings (Bigint.of_int pivot) level value
        clause then false_except (binding_values bindings)
        (if value then Positive pivot else Negative pivot) clause else true} =
  fun bindings pivot level value clause ->
  let forced = if value then Positive pivot else Negative pivot in
  reason_clause_def bindings (Bigint.of_int pivot) level value clause;
  false_except_def (binding_values bindings) forced clause;
  match clause with
  | [] -> ()
  | literal :: rest ->
    variable_def literal;
    wanted_def literal;
    same_literal_def literal forced;
    partial_literal_def (binding_values bindings) literal;
    at_def bindings (variable literal);
    binding_at_bounds bindings (Bigint.of_int (variable literal));
    lookup_value bindings (variable literal);
    reason_false_except bindings pivot level value rest

let rec (current_covered_member @ total) : (variables : int list) ->
    (bindings : binding option list) -> (level : int) ->
    (clause : literal list) -> (query : literal) ->
    {u : unit | if current_covered variables bindings level clause
        && has_literal query clause then
      match at bindings (variable query) with
      | Some (Some binding) -> if binding.level = level then
          has_int (variable query) variables else true
      | Some None | None -> true
      else true} =
  fun variables bindings level clause query ->
  current_covered_def variables bindings level clause;
  has_literal_def query clause;
  match clause with
  | [] -> ()
  | literal :: rest ->
    same_literal_def query literal;
    variable_def query;
    variable_def literal;
    current_covered_member variables bindings level rest query

let (resolve_preserves_current @ total) : (bindings : binding option list) ->
    (level : int) -> (variables : int list) -> (pivot : int) ->
    (value : bool) -> (current : literal list) -> (reason : literal list) ->
    {u : unit | if all_current variables bindings level
        && variable_sources variables current [] && unique_variables variables
        && (match variables with _ :: _ :: _ -> true | [] | [_] -> false) then
      not (current_covered [] bindings level
        (if value then resolve_clause pivot reason current
         else resolve_clause pivot current reason)) else true} =
  fun bindings level variables pivot value current reason ->
  unique_variables_def variables;
  match variables with
  | [] | [_] -> ()
  | first :: second :: tail ->
    has_int_def first (second :: tail);
    has_int_def first variables;
    has_int_def second variables;
    has_int_def second (second :: tail);
    let other = if first = pivot then second else first in
    all_current_member variables bindings level other;
    variable_source_member variables current [] other;
    has_int_def other [];
    let query = if has_literal (Positive other) current then Positive other
      else Negative other in
    variable_def query;
    same_literal_def query (Positive pivot);
    same_literal_def query (Negative pivot);
    let resolved = if value then resolve_clause pivot reason current
      else resolve_clause pivot current reason in
    if value then resolve_membership pivot reason current query
    else resolve_membership pivot current reason query;
    current_covered_member [] bindings level resolved query;
    ()

let (latest_binding_valid @ total) :
    (n : {n : int | 0 <= n}) @ ghost ->
    (formula : {f : formula | valid_formula n f}) ->
    (database : {d : proof_result list | database_valid formula d}) ->
    (state : {s : state |
      Vox_sequence.length s.bindings === Bigint.of_int n
      && levels_bounded s.level s.bindings && trail_covers s.bindings s.trail
      && trail_consistent s.bindings s.trail && trail_unique s.trail
      && trail_ordered s.bindings s.trail s.level
      && reason_sources_valid formula database s.learned s.bindings
      && reason_semantics formula database s.learned s.bindings
      && reason_order formula database s.learned s.bindings s.trail}) ->
    (current : {e : proof_result |
      derivation_valid formula e.proof
      && same_clause (conclusion formula e.proof) e.clause
      && false_clause (binding_values state.bindings) e.clause}) ->
    (variables : {vs : int list |
      all_current vs state.bindings state.level && unique_variables vs
      && current_covered vs state.bindings state.level current.clause
      && variable_sources vs current.clause []}) @ ghost ->
    (v : {v : int | has_int v variables
      && earlier_variables state.trail (Bigint.of_int v) variables
      && (match at state.bindings v with
        | Some (Some binding) -> not (binding.reason === Decision)
        | Some None | None -> false)}) ->
    (binding : {b : binding | at state.bindings v === Some (Some b)}) ->
    {u : unit | 0 <= v && v < n && binding.level = state.level
      && trail_has (Bigint.of_int v) state.trail
      && reason_source_valid formula database state.learned binding.reason
      && not (reason_source_clause formula database binding.reason === None)
      && valid_clause n current.clause
      && (match reason_source_clause formula database binding.reason with
        | None -> false
        | Some clause -> earlier_clause state.trail (Bigint.of_int v) clause
          && reason_clause state.bindings (Bigint.of_int v) binding.level
            binding.value clause)} =
  fun n formula database state current variables v binding ->
  ghost_ (
    result_clause_valid n formula current;
    current_variables_covered state.bindings state.trail state.level variables;
    all_current_member variables state.bindings state.level v;
    at_def state.bindings v;
    binding_at_bounds state.bindings (Bigint.of_int v);
    reason_sources_at formula database state.learned state.bindings
      (Bigint.of_int v);
    reason_source_exists formula database state.learned binding.reason;
    implied_reason_def binding.reason;
    reason_semantics_at formula database state.learned state.bindings
      (Bigint.of_int v);
    covered_variable state.trail variables v;
    reason_order_def formula database state.learned state.bindings state.trail;
    reason_order_member formula database state.learned state.bindings
      state.trail state.trail v;
    binding_order_def formula database state.learned state.trail
      (Bigint.of_int v) binding;
    binding_reason_def formula database state.learned state.bindings
      (Bigint.of_int v) binding);
  ()

let (resolution_step_correct @ total) : (n : {n : int | 0 <= n}) ->
    (level : int) -> (trail : {t : literal list | trail_unique t}) ->
    (bindings : {bs : binding option list |
      Vox_sequence.length bs === Bigint.of_int n && levels_bounded level bs
      && trail_covers bs trail && trail_consistent bs trail
      && trail_ordered bs trail level}) ->
    (current : {c : literal list | valid_clause n c
      && false_clause (binding_values bindings) c}) ->
    (variables : {vs : int list | all_current vs bindings level
      && unique_variables vs && current_covered vs bindings level current
      && variable_sources vs current []}) ->
    (value : bool) ->
    (pivot : {v : int | 0 <= v && v < n && has_int v variables
      && trail_has (Bigint.of_int v) trail
      && earlier_variables trail (Bigint.of_int v) variables
      && (match at bindings v with
        | Some (Some binding) -> binding.value = value && binding.level = level
        | Some None | None -> false)}) ->
    (reason : {c : literal list | earlier_clause trail (Bigint.of_int pivot) c
      && reason_clause bindings (Bigint.of_int pivot) level value c}) ->
    (resolved : {c : literal list | c ===
      (if value then resolve_clause pivot reason current
       else resolve_clause pivot current reason)}) ->
    {u : unit | unique_literals resolved
      && false_clause (binding_values bindings) resolved
      && Bigint.compare (clause_rank trail resolved) (clause_rank trail current)
        < 0
      && (if (match variables with _ :: _ :: _ -> true | [] | [_] -> false) then
        not (current_covered [] bindings level resolved) else true)} =
  fun n level trail bindings current variables value pivot reason resolved ->
  let partial = binding_values bindings in
  current_variables_covered bindings trail level variables;
  earlier_clause_rank trail (Bigint.of_int pivot) reason;
  conflict_earlier_clause n bindings trail level pivot variables current;
  earlier_clause_rank trail (Bigint.of_int pivot) current;
  variable_source_member variables current [] pivot;
  has_int_def pivot [];
  clause_rank_contains trail current (Positive pivot);
  clause_rank_contains trail current (Negative pivot);
  variable_def (Positive pivot);
  variable_def (Negative pivot);
  reason_excludes bindings pivot level value reason;
  lookup_value bindings pivot;
  partial_literal_def partial (if value then Positive pivot else Negative
    pivot);
  false_excludes partial current (if value then Positive pivot else Negative
    pivot);
  reason_false_except bindings pivot level value reason;
  resolve_false_clause partial pivot value current reason;
  resolve_preserves_current bindings level variables pivot value current reason;
  resolve_unique pivot reason current;
  resolve_unique pivot current reason;
  if value then resolve_rank trail pivot reason current
  else resolve_rank trail pivot current reason;
  ()

let (resolve_latest @ total) :
    (n : {n : int | 0 <= n}) @ ghost ->
    (formula : {f : formula | valid_formula n f}) ->
    (database : {d : proof_result list | database_valid formula d}) ->
    (state : {s : state |
      Vox_sequence.length s.bindings === Bigint.of_int n
      && levels_bounded s.level s.bindings && trail_covers s.bindings s.trail
      && trail_consistent s.bindings s.trail && trail_unique s.trail
      && trail_ordered s.bindings s.trail s.level
      && reason_sources_valid formula database s.learned s.bindings
      && reason_semantics formula database s.learned s.bindings
      && reason_order formula database s.learned s.bindings s.trail}) ->
    (current : {e : proof_result |
      derivation_valid formula e.proof
      && same_clause (conclusion formula e.proof) e.clause
      && false_clause (binding_values state.bindings) e.clause}) ->
    (variables : {vs : int list |
      all_current vs state.bindings state.level && unique_variables vs
      && current_covered vs state.bindings state.level current.clause
      && variable_sources vs current.clause []}) @ ghost ->
    (v : {v : int | has_int v variables
      && earlier_variables state.trail (Bigint.of_int v) variables
      && (match at state.bindings v with
        | Some (Some binding) -> not (binding.reason === Decision)
        | Some None | None -> false)}) ->
    {r : proof_result |
      unique_literals r.clause && derivation_valid formula r.proof
      && same_clause (conclusion formula r.proof) r.clause
      && false_clause (binding_values state.bindings) r.clause
      && Bigint.compare (clause_rank state.trail r.clause)
        (clause_rank state.trail current.clause) < 0
      && (if (match variables with
          | _ :: _ :: _ -> true | [] | [_] -> false) then
        not (current_covered [] state.bindings state.level r.clause)
        else true)} =
  fun n formula database state current variables v ->
  match at state.bindings v with
  | Some (Some binding) ->
    ghost_ (latest_binding_valid (ghost_ n) formula database state current
      (ghost_ variables) v binding);
    (match fetch_reason formula (ghost_ database) (ghost_ state.learned)
        binding.reason with
     | None ->
       let _ : {u : unit | false} = () in
       current
     | Some reason ->
       if binding.value then (
         let resolved = resolve_result formula v reason current in
         ghost_ (resolution_step_correct n state.level state.trail
           state.bindings
           current.clause variables true v reason.clause resolved.clause);
         resolved)
       else (
         let resolved = resolve_result formula v current reason in
         ghost_ (resolution_step_correct n state.level state.trail
           state.bindings
           current.clause variables false v reason.clause resolved.clause);
         resolved))
  | Some None | None ->
    let _ : {u : unit | false} = () in
    current

let rec (analyze @ total) :
    (n : {n : int | 0 <= n}) @ ghost ->
    (formula : {f : formula | valid_formula n f}) ->
    (database : {d : proof_result list | database_valid formula d}) ->
    (state : {s : state |
      Vox_sequence.length s.bindings === Bigint.of_int n
      && levels_bounded s.level s.bindings && trail_covers s.bindings s.trail
      && trail_consistent s.bindings s.trail && trail_unique s.trail
      && trail_ordered s.bindings s.trail s.level
      && reason_sources_valid formula database s.learned s.bindings
      && reason_semantics formula database s.learned s.bindings
      && reason_order formula database s.learned s.bindings s.trail}) ->
    (stop : {s : int | state.level = 0 || 1 <= s}) ->
    (current : {e : proof_result |
      derivation_valid formula e.proof
      && same_clause (conclusion formula e.proof) e.clause
      && false_clause (binding_values state.bindings) e.clause}) ->
    (rank : {r : Bigint.t | r === clause_rank state.trail current.clause})
      @ ghost ->
    {entry : proof_result |
      (unique_literals entry.clause || entry.clause === current.clause)
      && derivation_valid formula entry.proof
      && same_clause (conclusion formula entry.proof) entry.clause
      && (match scan_formula (partial_of_bindings state.bindings)
          [entry.clause] with
        | Scan_conflict _ -> true
        | Scan_stable | Scan_unit _ -> false)
      && (match current_variables entry.clause
          state.bindings state.level [] with
        | [] -> true
        | [_] -> 1 <= stop
        | _ :: _ :: _ -> false)
      && (if 0 < state.level
          && not (current_covered [] state.bindings state.level current.clause)
        then not (current_covered [] state.bindings state.level entry.clause)
        else true)} =
  fun n formula database state stop current rank ->
  ghost_ (clause_rank_nonnegative state.trail current.clause;
    conflict_characterization (binding_values state.bindings) current.clause;
    let _ = partial_of_bindings state.bindings in
    all_current_def [] state.bindings state.level;
    unique_variables_def []);
  let variables =
    current_variables current.clause state.bindings state.level [] in
  match variables with
  | [] -> current
  | [_] when stop >= 1 -> current
  | _ :: _ ->
    ghost_ (current_variables_covered state.bindings state.trail state.level
      variables);
    let pivot = find_latest (ghost_ state.bindings) (ghost_ state.level)
      state.trail variables in
    let resolved = resolve_latest (ghost_ n) formula database state
      current (ghost_ variables) pivot in
    ghost_ (clause_rank_nonnegative state.trail resolved.clause);
    analyze (ghost_ n) formula database state stop resolved
      (ghost_ (clause_rank state.trail resolved.clause))
[@@decreases rank]

let[@def] at_level (bindings : binding option list) (level : int) literal =
  match at bindings (variable literal) with
  | Some (Some binding) -> binding.level = level
  | Some None | None -> false

let[@def] rec clause_levels_except (bindings : binding option list)
    pivot target clause =
  match clause with
  | [] -> true
  | literal :: rest ->
    (if variable literal = pivot then true
     else match at bindings (variable literal) with
       | Some (Some binding) -> binding.level <= target
       | Some None | None -> false)
    && clause_levels_except bindings pivot target rest

let (singleton_members @ total) : (variables : int list) ->
    (left : int) -> (right : int) ->
    {u : unit | if has_int left variables && has_int right variables
        && (match variables with [] | [_] -> true | _ :: _ :: _ -> false)
      then left = right else true} =
  fun variables left right ->
  has_int_def left variables;
  has_int_def right variables;
  has_int_def left [];
  has_int_def right []

let[@def] rec level_occurs (bindings : binding option list) pivot level clause =
  match clause with
  | [] -> false
  | literal :: rest ->
    (variable literal <> pivot && (match at bindings (variable literal) with
      | Some (Some binding) -> binding.level = level
      | Some None | None -> false)) || level_occurs bindings pivot level rest

let rec (asserting_clause @ total) :
    (n : int) @ ghost ->
    (bindings : {bs : binding option list |
      Vox_sequence.length bs === Bigint.of_int n}) -> (level : int) ->
    (variables : {vs : int list |
      match vs with [] | [_] -> true | _ :: _ :: _ -> false}) @ ghost ->
    (clause : {c : literal list | valid_clause n c
      && current_covered variables bindings level c
      && match scan_formula (binding_values bindings) [c] with
      | Scan_conflict _ -> true | Scan_stable | Scan_unit _ -> false}) ->
    (asserting : {a : literal option | match a with
      | None -> not (current_covered [] bindings level clause)
      | Some literal -> at_level bindings level literal
        && has_int (variable literal) variables
        && partial_literal (binding_values bindings) literal === Some false}) ->
    (target : int) ->
    {result : literal * int | match result with
      | literal, next -> target <= next
        && (if levels_bounded level bindings && target < level then
          next < level else true)
        && (if target < next then
          level_occurs bindings (variable literal) next clause else true)
        && at_level bindings level literal
        && partial_literal (binding_values bindings) literal === Some false
        && clause_levels_except bindings (variable literal) next clause
        && (match asserting with
          | None -> reason_contains (Bigint.of_int (variable literal))
              (wanted literal) clause
          | Some previous -> previous === literal)} =
  fun n bindings level variables clause asserting target ->
  ghost_ (valid_clause_def n clause;
    current_covered_def variables bindings level clause;
    current_covered_def [] bindings level clause);
  let result =
  match clause with
  | [] ->
    (match asserting with
     | Some literal -> literal, target
     | None ->
       let _ : {u : unit | false} = () in
       Positive 0, target)
  | literal :: rest ->
    ghost_ (scan_conflict_head (binding_values bindings) literal rest);
    let v = variable literal in
    ghost_ (valid_literal_def n literal;
      variable_def literal;
      partial_literal_def (binding_values bindings) literal;
      lookup_value bindings v);
    match at bindings v with
    | Some (Some binding) ->
      ghost_ (
        at_def bindings v;
        levels_at bindings level (Bigint.of_int v);
        at_level_def bindings level literal);
      if binding.level = level then
        (match asserting with
         | None ->
           asserting_clause (ghost_ n) bindings level (ghost_ variables)
             rest (Some literal) target
         | Some previous ->
           ghost_ (singleton_members variables (variable previous) v);
           asserting_clause (ghost_ n) bindings level (ghost_ variables)
             rest asserting target)
      else
        let target = if binding.level > target then binding.level else target in
        asserting_clause (ghost_ n) bindings level (ghost_ variables)
          rest asserting target
    | Some None | None ->
      let _ : {u : unit | false} = () in
      Positive 0, target
  in
  ghost_ (let literal, next = result in
    at_level_def bindings level literal;
    level_occurs_def bindings (variable literal) next clause;
    clause_levels_except_def bindings (variable literal) next clause;
    reason_contains_def (Bigint.of_int (variable literal)) (wanted literal)
      clause);
  result

let rec (find_level_literal @ total) : (bindings : binding option list) ->
    (pivot : int) -> (level : int) ->
    (clause : {c : literal list | level_occurs bindings pivot level c}) ->
    {literal : literal | has_literal literal clause && variable literal <> pivot
      && (match at bindings (variable literal) with
        | Some (Some binding) -> binding.level = level
        | Some None | None -> false)} =
  fun bindings pivot level clause ->
  ghost_ (level_occurs_def bindings pivot level clause);
  match clause with
  | [] ->
    let _ : {u : unit | false} = () in
    Positive 0
  | literal :: rest ->
    let found = match at bindings (variable literal) with
      | Some (Some binding) ->
        variable literal <> pivot && binding.level = level
      | Some None | None -> false in
    if found then (
      ghost_ (same_literal_def literal literal;
        has_literal_def literal clause);
      literal)
    else
      let found = find_level_literal bindings pivot level rest in
      ghost_ (has_literal_def found clause);
      found

let rec (prefix_stable_prepend_below @ total) : (formula : formula) ->
    (database : proof_result list) -> (bindings : binding option list) ->
    (bound : int) -> (level : int) -> (entry : proof_result) ->
    (forced : literal) -> (witness : literal) ->
    {u : unit | if level <= bound
        && prefix_stable formula database bindings level
        && at bindings (variable forced) === Some None
        && has_literal forced entry.clause && has_literal witness entry.clause
        && variable forced <> variable witness
        && (match at bindings (variable witness) with
          | Some (Some binding) -> binding.level = bound
          | Some None | None -> false) then
      prefix_stable formula (entry :: database) bindings level else true} =
  fun formula database bindings bound level entry forced witness ->
  prefix_stable_def formula database bindings level;
  prefix_stable_def formula (entry :: database) bindings level;
  if 0 < level then (
    let retained = retained_bindings (level - 1) bindings in
    let partial = binding_values retained in
    variable_def forced;
    variable_def witness;
    same_literal_def forced witness;
    at_def bindings (variable forced);
    at_def bindings (variable witness);
    binding_at_bounds bindings (Bigint.of_int (variable forced));
    binding_at_bounds bindings (Bigint.of_int (variable witness));
    retained_binding_at (level - 1) bindings (Bigint.of_int (variable forced));
    retained_binding_at (level - 1) bindings (Bigint.of_int (variable witness));
    at_def retained (variable forced);
    at_def retained (variable witness);
    lookup_value retained (variable forced);
    lookup_value retained (variable witness);
    partial_literal_def partial forced;
    partial_literal_def partial witness;
    two_unassigned_stable partial entry.clause forced witness;
    database_clauses_def (entry :: database);
    formula_stable_def partial (entry.clause :: database_clauses database);
    prefix_stable_prepend_below formula database bindings bound (level - 1)
      entry forced witness);
  ()
[@@decreases level]

let (prefix_stable_prepend @ total) : (formula : formula) ->
    (database : proof_result list) -> (bindings : binding option list) ->
    (level : int) -> (entry : proof_result) ->
    (forced : literal) -> (witness : literal) ->
    {u : unit | if prefix_stable formula database bindings level
        && at bindings (variable forced) === Some None
        && has_literal forced entry.clause
        && (if 0 < level then has_literal witness entry.clause
          && variable forced <> variable witness
          && (match at bindings (variable witness) with
            | Some (Some binding) -> binding.level = level
            | Some None | None -> false) else true) then
      prefix_stable formula (entry :: database) bindings level else true} =
  fun formula database bindings level entry forced witness ->
  if 0 < level then
    prefix_stable_prepend_below formula database bindings level level
      entry forced witness
  else prefix_stable_def formula (entry :: database) bindings level

let rec (asserting_reason @ total) : (n : int) ->
    (bindings : binding option list) -> (forced : literal) -> (target : int) ->
    (clause : literal list) ->
    {u : unit | if Vox_sequence.length bindings === Bigint.of_int n
        && valid_clause n clause
        && partial_literal (binding_values bindings) forced === Some false
        && clause_levels_except bindings (variable forced) target clause
        && (match scan_formula (binding_values bindings) [clause] with
          | Scan_conflict _ -> true | Scan_stable | Scan_unit _ -> false) then
      reason_clause (retained_bindings target bindings)
        (Bigint.of_int (variable forced)) target (wanted forced) clause
      else true} =
  fun n bindings forced target clause ->
  let next = retained_bindings target bindings in
  valid_clause_def n clause;
  clause_levels_except_def bindings (variable forced) target clause;
  reason_clause_def next (Bigint.of_int (variable forced)) target
    (wanted forced) clause;
  match clause with
  | [] -> ()
  | literal :: rest ->
    scan_conflict_head (binding_values bindings) literal rest;
    valid_literal_def n literal;
    variable_def literal;
    variable_def forced;
    wanted_def literal;
    wanted_def forced;
    partial_literal_def (binding_values bindings) literal;
    partial_literal_def (binding_values bindings) forced;
    lookup_value bindings (variable literal);
    at_def bindings (variable literal);
    at_def next (variable literal);
    retained_binding_at target bindings (Bigint.of_int (variable literal));
    asserting_reason n bindings forced target rest

let (scan_unit_binding @ total) :
    (limit : {n : int | 0 <= n}) @ ghost ->
    (bindings : {bs : binding option list |
      Bigint.compare (Vox_sequence.length bs) 256Z <= 0}) ->
    (formula : {f : formula | clauses_fit limit f}) ->
    {u : unit | match scan_formula (binding_values bindings) formula with
      | Scan_unit (index, literal) ->
        at bindings (variable literal) === Some None
        && (match clause_at formula index with
          | None -> false
          | Some clause -> has_literal literal clause
            && false_except (binding_values bindings) literal
            clause)
      | Scan_stable | Scan_conflict _ -> true} =
  fun limit bindings formula ->
  scan_formula_unit_unassigned (binding_values bindings) formula;
  scan_formula_unit_reason (ghost_ limit) (binding_values bindings) formula;
  match scan_formula (binding_values bindings) formula with
  | Scan_stable | Scan_conflict _ -> ()
  | Scan_unit (_, literal) ->
    variable_def literal;
    match literal with
    | Positive v | Negative v ->
      lookup_unassigned bindings v;
      at_def bindings v

let rec (propagate @ total) : (n : int) @ ghost ->
    (formula : {f : formula | valid_formula n f && clauses_fit 4096 f}) ->
    (database : {d : proof_result list | database_valid formula d}) ->
    (state : {s : state |
      Vox_sequence.length s.bindings === Bigint.of_int n &&
      Bigint.compare (Vox_sequence.length s.bindings) 256Z <= 0
      && trail_levels s.bindings s.trail s.level
      && trail_covers s.bindings s.trail
      && trail_consistent s.bindings s.trail && trail_unique s.trail
      && trail_ordered s.bindings s.trail s.level
      && 0 <= s.level && levels_bounded s.level s.bindings
      && reason_sources_valid formula database s.learned s.bindings}) ->
    (remaining : {count : Bigint.t |
      Bigint.compare count 0Z >= 0
      && count === unassigned state.bindings}) @ ghost ->
    {r : propagation |
      match r with
      | Stable (next, partial) ->
        partial === binding_values next.bindings
        && scan_formula partial formula === Scan_stable
        && scan_formula partial (database_clauses database) === Scan_stable
        && trail_covers next.bindings next.trail
        && trail_consistent next.bindings next.trail && trail_unique next.trail
        && trail_ordered next.bindings next.trail next.level
        && next.learned = state.learned && next.level = state.level
        && next.steps = state.steps
        && trail_levels next.bindings next.trail next.level
        && Bigint.compare (unassigned next.bindings)
          (unassigned state.bindings) <= 0
        && (if prefix_stable formula database state.bindings state.level then
          prefix_stable formula database next.bindings next.level else true)
        && (if prefix_consistent formula database state.bindings state.level
          then
          prefix_consistent formula database next.bindings next.level else true)
        && levels_bounded next.level next.bindings
        && reason_sources_valid formula database next.learned next.bindings
        && (if reason_order formula database state.learned state.bindings
          state.trail
            then reason_order formula database next.learned next.bindings
              next.trail else true)
        && (if reason_semantics formula database state.learned state.bindings
            then reason_semantics formula database next.learned next.bindings
            else true)
        && Vox_sequence.length next.bindings ===
          Vox_sequence.length state.bindings
        && Vox_sequence.length partial ===
          Vox_sequence.length state.bindings
        && (if all_bound next.bindings then
          match complete_partial partial with Some _ -> true | None -> false
          else true)
      | Conflict (entry, next) ->
        derivation_valid formula entry.proof
        && same_clause (conclusion formula entry.proof) entry.clause
        && (clause_member formula entry.clause
          || clause_member (database_clauses database) entry.clause)
        && Vox_sequence.length next.bindings ===
          Vox_sequence.length state.bindings
        && trail_covers next.bindings next.trail
        && trail_consistent next.bindings next.trail && trail_unique next.trail
        && trail_ordered next.bindings next.trail next.level
        && next.learned = state.learned && next.level = state.level
        && next.steps = state.steps
        && trail_levels next.bindings next.trail next.level
        && Bigint.compare (unassigned next.bindings)
          (unassigned state.bindings) <= 0
        && (if prefix_stable formula database state.bindings state.level then
          prefix_stable formula database next.bindings next.level else true)
        && (if prefix_consistent formula database state.bindings state.level
          then
          prefix_consistent formula database next.bindings next.level else true)
        && levels_bounded next.level next.bindings
        && reason_sources_valid formula database next.learned next.bindings
        && (if reason_order formula database state.learned state.bindings
          state.trail
            then reason_order formula database next.learned next.bindings
              next.trail else true)
        && (if reason_semantics formula database state.learned state.bindings
            then reason_semantics formula database next.learned next.bindings
            else true)
        && false_clause (binding_values next.bindings) entry.clause} =
  fun n formula database state remaining ->
  ghost_ (unassigned_bounds state.bindings);
  let partial = partial_of_bindings state.bindings in
  ghost_ (scan_formula_source partial formula);
  match scan_formula partial formula with
  | Scan_conflict index ->
    ghost_ (
      scan_formula_conflict_clause (ghost_ 4096) partial formula;
      source_clause_def formula database (Original_clause index);
      source_clause_member formula database (Original_clause index));
    (match original_result formula index with
     | Some entry -> Conflict (entry, state)
     | None ->
       let _ : {u : unit | false} = () in
       Stable (state, partial))
  | Scan_unit (index, literal) ->
    ghost_ (scan_unit_binding (ghost_ 4096) state.bindings formula);
    ghost_ (
      reason_source_valid_def formula database state.learned (Original index);
      implied_reason_def (Original index);
      reason_source_clause_def formula database (Original index);
      unit_enqueue_reason n formula database state.learned state.bindings
        state.level literal (Original index));
    (match enqueue (ghost_ formula) (ghost_ database) state literal
        (Original index) with
     | None ->
       let _ : {u : unit | false} = () in
       Stable (state, partial)
     | Some state ->
       ghost_ (unassigned_bounds state.bindings);
       propagate (ghost_ n) formula database state
         (ghost_ (Bigint.sub remaining 1Z)))
  | Scan_stable ->
    match scan_database (ghost_ formula) partial database with
    | Database_conflict entry -> Conflict (entry, state)
    | Database_unit (entry, literal) ->
      let reason = Learned (store_result entry) in
      ghost_ (
        unit_clause_scan partial literal entry.clause;
        clauses_fit_def 1 [entry.clause];
        clauses_fit_def 0 [];
        scan_unit_binding (ghost_ 1) state.bindings [entry.clause];
        reason_source_valid_def formula database state.learned reason;
        implied_reason_def reason;
        reason_source_clause_def formula database reason;
        unit_enqueue_reason n formula database state.learned state.bindings
          state.level literal reason);
      (match enqueue (ghost_ formula) (ghost_ database) state literal reason
        with
       | None ->
         let _ : {u : unit | false} = () in
         Stable (state, partial)
       | Some state ->
         ghost_ (unassigned_bounds state.bindings);
         propagate (ghost_ n) formula database state
           (ghost_ (Bigint.sub remaining 1Z)))
    | Database_stable ->
      ghost_ (scan_stable_formula partial (database_clauses database));
      Stable (state, partial)
[@@decreases remaining]

let (statistics @ total) (state : state @ immutable total) :
    {r : statistics | r.steps = state.steps} = {
  decisions = state.decisions;
  conflicts = state.conflicts;
  learned = state.learned;
  backjumps = state.backjumps;
  steps = state.steps;
}

let rec (lookup_assigned @ total) :
    (bindings : binding option list) -> (index : int) ->
    {u : unit | if 0 <= index
        && Bigint.compare (Bigint.of_int index)
          (Vox_sequence.length bindings) < 0
        && not (partial_lookup (binding_values bindings) index === None)
      then match at bindings index with
        | Some (Some _) -> true | Some None | None -> false
      else true} =
  fun bindings index ->
  binding_values_def bindings;
  partial_lookup_def (binding_values bindings) index;
  Vox_sequence.length_def bindings;
  at_def bindings index;
  Vox_sequence.at_def bindings (Bigint.of_int index);
  match bindings with
  | [] -> ()
  | _ :: rest ->
    if index > 0 then (
      lookup_assigned rest (index - 1);
      at_def rest (index - 1));
    ()

let (root_conflict_empty @ total) : (n : int) ->
    (bindings : {bs : binding option list |
      Vox_sequence.length bs === Bigint.of_int n && levels_bounded 0 bs}) ->
    (clause : {c : literal list | valid_clause n c
      && current_variables c bindings 0 [] === []
      && (match scan_formula (binding_values bindings) [c] with
        | Scan_conflict _ -> true | Scan_stable | Scan_unit _ -> false)}) ->
    {u : unit | clause === []} =
  fun n bindings clause ->
  let _ = current_variables clause bindings 0 [] in
  valid_clause_def n clause;
  match clause with
  | [] -> ()
  | literal :: rest ->
    valid_literal_def n literal;
    variable_def literal;
    scan_conflict_head (binding_values bindings) literal rest;
    lookup_assigned bindings (variable literal);
    at_def bindings (variable literal);
    levels_at bindings 0 (Bigint.of_int (variable literal))

let (prefix_prepend @ total) : (formula : formula) ->
    (database : proof_result list) -> (bindings : binding option list) ->
    (level : int) -> (entry : proof_result) -> (forced : literal) ->
    {u : unit | if prefix_consistent formula database bindings level
        && at bindings (variable forced) === Some None
        && reason_contains (Bigint.of_int (variable forced)) (wanted forced)
          entry.clause then
      prefix_consistent formula (entry :: database) bindings level else true} =
  fun formula database bindings level entry forced ->
  prefix_consistent_def formula database bindings level;
  prefix_consistent_def formula (entry :: database) bindings level;
  let retained = retained_bindings (level - 1) bindings in
  let partial = binding_values retained in
  variable_def forced;
  at_def bindings (variable forced);
  binding_at_bounds bindings (Bigint.of_int (variable forced));
  retained_binding_at (level - 1) bindings (Bigint.of_int (variable forced));
  at_def retained (variable forced);
  lookup_value retained (variable forced);
  partial_literal_def partial forced;
  reason_contains_literal forced entry.clause;
  false_clause_member partial entry.clause forced;
  database_clauses_def (entry :: database);
  no_conflict_def partial (entry.clause :: database_clauses database)

let rec (no_current_false_retained @ total) : (n : int) ->
    (bindings : binding option list) -> (level : int) ->
    (clause : literal list) ->
    {u : unit | if 0 < level
        && Vox_sequence.length bindings === Bigint.of_int n
        && levels_bounded level bindings && valid_clause n clause
        && false_clause (binding_values bindings) clause
        && current_covered [] bindings level clause then
      false_clause (binding_values (retained_bindings (level - 1) bindings))
        clause else true} =
  fun n bindings level clause ->
  let partial = binding_values bindings in
  let retained = retained_bindings (level - 1) bindings in
  let prefix = binding_values retained in
  valid_clause_def n clause;
  false_clause_def partial clause;
  false_clause_def prefix clause;
  current_covered_def [] bindings level clause;
  match clause with
  | [] -> ()
  | literal :: rest ->
    let v = variable literal in
    valid_literal_def n literal;
    variable_def literal;
    partial_literal_def partial literal;
    partial_literal_def prefix literal;
    lookup_value bindings v;
    at_def bindings v;
    levels_at bindings level (Bigint.of_int v);
    has_int_def v [];
    retained_binding_at (level - 1) bindings (Bigint.of_int v);
    at_def retained v;
    lookup_value retained v;
    no_current_false_retained n bindings level rest

let (prefix_clause_no_conflict @ total) : (formula : formula) ->
    (database : proof_result list) -> (bindings : binding option list) ->
    (level : int) -> (clause : literal list) ->
    {u : unit | if 0 < level
        && prefix_consistent formula database bindings level
        && (clause_member formula clause
          || clause_member (database_clauses database) clause) then
      not (false_clause
        (binding_values (retained_bindings (level - 1) bindings)) clause)
      else true} =
  fun formula database bindings level clause ->
  prefix_consistent_def formula database bindings level;
  let partial = binding_values (retained_bindings (level - 1) bindings) in
  no_conflict_member partial formula clause;
  no_conflict_member partial (database_clauses database) clause

let[@def] rec remove_literal query clause =
  match clause with
  | [] -> []
  | literal :: rest ->
    if same_literal query literal then remove_literal query rest
    else literal :: remove_literal query rest

let rec (remove_literal_member @ total) : (removed : literal) ->
    (clause : literal list) -> (query : literal) ->
    {u : unit | has_literal query (remove_literal removed clause) =
      (has_literal query clause && not (same_literal query removed))} =
  fun removed clause query ->
  remove_literal_def removed clause;
  has_literal_def query clause;
  same_literal_def query removed;
  match clause with
  | [] -> has_literal_def query []
  | literal :: rest ->
    same_literal_def removed literal;
    same_literal_def query literal;
    remove_literal_member removed rest query;
    has_literal_def query (literal :: remove_literal removed rest)

let rec (remove_literal_length @ total) : (removed : literal) ->
    (clause : literal list) ->
    {u : unit |
      Bigint.compare (Vox_sequence.length (remove_literal removed clause))
        (Vox_sequence.length clause) <= 0
      && (if has_literal removed clause then
        Bigint.compare (Vox_sequence.length (remove_literal removed clause))
          (Vox_sequence.length clause) < 0 else true)} =
  fun removed clause ->
  remove_literal_def removed clause;
  has_literal_def removed clause;
  Vox_sequence.length_def clause;
  match clause with
  | [] -> ()
  | literal :: rest ->
    remove_literal_length removed rest;
    Vox_sequence.length_def (literal :: remove_literal removed rest)

let rec (subset_remove_literal @ total) : (removed : literal) ->
    (part : literal list) -> (whole : literal list) ->
    {u : unit | if clause_subset part whole && not (has_literal removed part)
      then clause_subset part (remove_literal removed whole) else true} =
  fun removed part whole ->
  clause_subset_def part whole;
  clause_subset_def part (remove_literal removed whole);
  has_literal_def removed part;
  match part with
  | [] -> ()
  | literal :: rest ->
    same_literal_def removed literal;
    same_literal_def literal removed;
    remove_literal_member removed whole literal;
    subset_remove_literal removed rest whole

let rec (unique_subset_length @ total) : (part : literal list) ->
    (whole : literal list) ->
    {u : unit | if unique_literals part && clause_subset part whole then
      Bigint.compare (Vox_sequence.length part) (Vox_sequence.length whole) <= 0
      else true} =
  fun part whole ->
  unique_literals_def part;
  clause_subset_def part whole;
  Vox_sequence.length_def part;
  sequence_length_nonnegative whole;
  match part with
  | [] -> ()
  | literal :: rest ->
    subset_remove_literal literal rest whole;
    unique_subset_length rest (remove_literal literal whole);
    remove_literal_length literal whole

let[@def] rec literal_universe n =
  if n <= 0 then []
  else Positive (n - 1) :: Negative (n - 1) :: literal_universe (n - 1)
[@@decreases n]

let rec (literal_universe_length @ total) : (n : {n : int | 0 <= n}) ->
    {u : unit | Vox_sequence.length (literal_universe n) ===
      Bigint.mul 2Z (Bigint.of_int n)} =
  fun n ->
  literal_universe_def n;
  if n <= 0 then Vox_sequence.length_def []
  else (
    literal_universe_length (n - 1);
    Vox_sequence.length_def
      (Positive (n - 1) :: Negative (n - 1) :: literal_universe (n - 1));
    Vox_sequence.length_def (Negative (n - 1) :: literal_universe (n - 1)))
[@@decreases n]

let rec (literal_universe_member @ total) : (n : {n : int | 0 <= n}) ->
    (literal : literal) ->
    {u : unit | valid_literal n literal = has_literal literal (literal_universe
      n)} =
  fun n literal ->
  literal_universe_def n;
  valid_literal_def n literal;
  if n <= 0 then has_literal_def literal []
  else (
    literal_universe_member (n - 1) literal;
    valid_literal_def (n - 1) literal;
    has_literal_def literal
      (Positive (n - 1) :: Negative (n - 1) :: literal_universe (n - 1));
    has_literal_def literal (Negative (n - 1) :: literal_universe (n - 1));
    same_literal_def literal (Positive (n - 1));
    same_literal_def literal (Negative (n - 1)))
[@@decreases n]

let rec (valid_clause_subset @ total) : (n : {n : int | 0 <= n}) ->
    (clause : literal list) ->
    {u : unit | valid_clause n clause = clause_subset clause (literal_universe
      n)} =
  fun n clause ->
  valid_clause_def n clause;
  clause_subset_def clause (literal_universe n);
  match clause with
  | [] -> ()
  | literal :: rest ->
    literal_universe_member n literal;
    valid_clause_subset n rest

let (unique_clause_length @ total) : (n : {n : int | 0 <= n}) ->
    (clause : literal list) ->
    {u : unit | if valid_clause n clause && unique_literals clause then
      Bigint.compare (Vox_sequence.length clause)
        (Bigint.mul 2Z (Bigint.of_int n)) <= 0 else true} =
  fun n clause ->
  valid_clause_subset n clause;
  unique_subset_length clause (literal_universe n);
  literal_universe_length n

let[@def] rec prepend_literal (literal : literal) (clauses : formula) =
  match clauses with
  | [] -> []
  | clause :: rest -> (literal :: clause) :: prepend_literal literal rest

let rec (prepend_literal_member @ total) : (literal : literal) ->
    (clauses : formula) -> (clause : literal list) ->
    {u : unit | if clause_member clauses clause then
      clause_member (prepend_literal literal clauses) (literal :: clause)
      else true} =
  fun literal clauses clause ->
  prepend_literal_def literal clauses;
  clause_member_def clauses clause;
  match clauses with
  | [] -> clause_member_def [] (literal :: clause)
  | first :: rest ->
    clause_member_def ((literal :: first) :: prepend_literal literal rest)
      (literal :: clause);
    same_clause_equal first clause;
    same_clause_reflexive (literal :: clause);
    same_literal_def literal literal;
    prepend_literal_member literal rest clause

let rec (clause_member_append @ total) : (left : formula) ->
    (right : formula) -> (clause : literal list) ->
    {u : unit | clause_member (Vox_sequence.append left right) clause =
      (clause_member left clause || clause_member right clause)} =
  fun left right clause ->
  Vox_sequence.append_def left right;
  clause_member_def left clause;
  match left with
  | [] -> ()
  | first :: rest ->
    clause_member_def (first :: Vox_sequence.append rest right) clause;
    clause_member_append rest right clause

let[@def] rec extend_clauses (literals : literal list) (clauses : formula) =
  match literals with
  | [] -> []
  | literal :: rest -> Vox_sequence.append (prepend_literal literal clauses)
      (extend_clauses rest clauses)

let rec (extend_clauses_member @ total) : (literals : literal list) ->
    (clauses : formula) -> (literal : literal) -> (clause : literal list) ->
    {u : unit | if has_literal literal literals && clause_member clauses clause
      then clause_member (extend_clauses literals clauses) (literal :: clause)
      else true} =
  fun literals clauses literal clause ->
  extend_clauses_def literals clauses;
  has_literal_def literal literals;
  match literals with
  | [] -> clause_member_def [] (literal :: clause)
  | first :: rest ->
    same_literal_def literal first;
    let _ : {u : unit | if same_literal literal first then literal === first
      else true} = () in
    prepend_literal_member literal clauses clause;
    extend_clauses_member rest clauses literal clause;
    clause_member_append (prepend_literal first clauses)
      (extend_clauses rest clauses) (literal :: clause)

let[@def] rec clause_universe n depth =
  if depth <= 0 then [[]]
  else [] :: extend_clauses (literal_universe n) (clause_universe n (depth - 1))
[@@decreases depth]

let rec (clause_universe_member @ total) : (n : {n : int | 0 <= n}) ->
    (depth : {d : int | 0 <= d}) -> (clause : literal list) ->
    {u : unit | if valid_clause n clause && Bigint.compare
      (Vox_sequence.length clause) (Bigint.of_int depth) <= 0 then
      clause_member (clause_universe n depth) clause else true} =
  fun n depth clause ->
  clause_universe_def n depth;
  valid_clause_def n clause;
  Vox_sequence.length_def clause;
  match clause with
  | [] ->
    same_clause_reflexive [];
    if depth <= 0 then clause_member_def [[]] []
    else clause_member_def
      ([] :: extend_clauses (literal_universe n)
        (clause_universe n (depth - 1))) []
  | literal :: rest ->
    sequence_length_nonnegative rest;
    if 0 < depth then (
      clause_universe_member n (depth - 1) rest;
      literal_universe_member n literal;
      extend_clauses_member (literal_universe n) (clause_universe n (depth - 1))
        literal rest;
      clause_member_def
        ([] :: extend_clauses (literal_universe n)
          (clause_universe n (depth - 1))) clause);
    ()
[@@decreases depth]

let[@def] rec count_absent universe database =
  match universe with
  | [] -> 0Z
  | clause :: rest -> Bigint.add
      (if clause_member database clause then 0Z else 1Z)
      (count_absent rest database)

let rec (count_absent_nonnegative @ total) : (universe : formula) ->
    (database : formula) ->
    {u : unit | Bigint.compare (count_absent universe database) 0Z >= 0} =
  fun universe database ->
  count_absent_def universe database;
  match universe with
  | [] -> ()
  | _ :: rest -> count_absent_nonnegative rest database

let rec (count_absent_prepend @ total) : (universe : formula) ->
    (database : formula) -> (clause : literal list) ->
    {u : unit |
      Bigint.compare (count_absent universe (clause :: database))
        (count_absent universe database) <= 0
      && (if clause_member universe clause && not (clause_member database
        clause)
        then Bigint.compare (count_absent universe (clause :: database))
          (count_absent universe database) < 0 else true)} =
  fun universe database clause ->
  count_absent_def universe database;
  count_absent_def universe (clause :: database);
  clause_member_def universe clause;
  match universe with
  | [] -> ()
  | first :: rest ->
    clause_member_def (clause :: database) first;
    same_clause_equal first clause;
    same_clause_reflexive clause;
    count_absent_prepend rest database clause

let rec (trail_has_literal @ total) : (trail : literal list) ->
    (literal : literal) ->
    {u : unit | if has_literal literal trail then
      trail_has (Bigint.of_int (variable literal)) trail else true} =
  fun trail literal ->
  has_literal_def literal trail;
  trail_has_def (Bigint.of_int (variable literal)) trail;
  match trail with
  | [] -> ()
  | first :: rest ->
    same_literal_def literal first;
    variable_def literal;
    variable_def first;
    trail_has_literal rest literal

let rec (trail_unique_literals @ total) : (trail : literal list) ->
    {u : unit | if trail_unique trail then unique_literals trail else true} =
  fun trail ->
  trail_unique_def trail;
  unique_literals_def trail;
  match trail with
  | [] -> ()
  | literal :: rest ->
    trail_has_literal rest literal;
    trail_unique_literals rest

let rec (trail_valid @ total) : (n : int) -> (bindings : binding option list) ->
    (trail : literal list) ->
    {u : unit | if Vox_sequence.length bindings === Bigint.of_int n
        && trail_consistent bindings trail then valid_clause n trail else true}
          =
  fun n bindings trail ->
  trail_consistent_def bindings trail;
  valid_clause_def n trail;
  match trail with
  | [] -> ()
  | literal :: rest ->
    at_def bindings (variable literal);
    binding_at_bounds bindings (Bigint.of_int (variable literal));
    variable_def literal;
    valid_literal_def n literal;
    trail_valid n bindings rest

let (decision_level_bound @ total) : (n : {n : int | 0 <= n && n <= 256}) ->
    (bindings : binding option list) -> (trail : literal list) -> (level : int)
      ->
    {u : unit | if Vox_sequence.length bindings === Bigint.of_int n
        && trail_consistent bindings trail && trail_unique trail
        && trail_levels bindings trail level then 0 <= level && level <= 512
      else true} =
  fun n bindings trail level ->
  trail_unique_literals trail;
  trail_valid n bindings trail;
  unique_clause_length n trail;
  trail_levels_bound bindings trail level

let[@def] rec database_unique database =
  match database with
  | [] -> true
  | entry :: rest ->
    not (clause_member (database_clauses rest) entry.clause)
    && database_unique rest

let[@def] progress_measure n database bindings =
  Bigint.add
    (Bigint.mul
      (count_absent (clause_universe n (2 * n)) (database_clauses database))
      (Bigint.add (Bigint.of_int n) 1Z))
    (unassigned bindings)

let (progress_nonnegative @ total) : (n : {n : int | 0 <= n}) ->
    (database : proof_result list) -> (bindings : binding option list) ->
    {u : unit | Bigint.compare (progress_measure n database bindings) 0Z >= 0} =
  fun n database bindings ->
  progress_measure_def n database bindings;
  count_absent_nonnegative (clause_universe n (2 * n)) (database_clauses
    database);
  unassigned_bounds bindings

let (progress_decision @ total) : (n : int) ->
    (database : proof_result list) -> (before : binding option list) ->
    (after : binding option list) ->
    {u : unit | if Bigint.compare (unassigned after) (unassigned before) < 0
      then
      Bigint.compare (progress_measure n database after)
        (progress_measure n database before) < 0 else true} =
  fun n database before after ->
  progress_measure_def n database before;
  progress_measure_def n database after

let (progress_learning @ total) : (n : {n : int | 0 <= n}) ->
    (database : proof_result list) -> (entry : proof_result) ->
    (before : binding option list) -> (after : binding option list) ->
    {u : unit | if Vox_sequence.length after === Bigint.of_int n
        && Bigint.compare
          (count_absent (clause_universe n (2 * n))
            (entry.clause :: database_clauses database))
          (count_absent (clause_universe n (2 * n))
            (database_clauses database)) < 0 then
      Bigint.compare (progress_measure n (entry :: database) after)
        (progress_measure n database before) < 0 else true} =
  fun n database entry before after ->
  progress_measure_def n database before;
  progress_measure_def n (entry :: database) after;
  database_clauses_def (entry :: database);
  unassigned_bounds before;
  unassigned_bounds after

let (prepare_learning @ total) : (n : {n : int | 0 <= n && n <= 256}) ->
    (formula : formula) ->
    (database : {d : proof_result list | database_unique d}) ->
    (old : {s : state | Vox_sequence.length s.bindings === Bigint.of_int n
      && prefix_stable formula database s.bindings s.level}) ->
    (learned : {e : proof_result | valid_clause n e.clause
      && (unique_literals e.clause || clause_member formula e.clause
        || clause_member (database_clauses database) e.clause)
      && (match scan_formula (binding_values old.bindings) [e.clause] with
        | Scan_conflict _ -> true | Scan_stable | Scan_unit _ -> false)}) ->
    (literal : literal) ->
    (target : {t : int | 0 <= t && t < old.level
      && at_level old.bindings old.level literal
      && partial_literal (binding_values old.bindings) literal === Some false
      && clause_levels_except old.bindings (variable literal) t learned.clause
      && reason_contains (Bigint.of_int (variable literal)) (wanted literal)
        learned.clause
      && (if 0 < t then level_occurs old.bindings (variable literal) t
        learned.clause
        else true)}) ->
    (next : {s : state | s.bindings === retained_bindings target old.bindings
      && s.level = target
      && prefix_consistent formula database s.bindings s.level
      && prefix_stable formula database s.bindings s.level
      && reason_order formula database s.learned s.bindings s.trail
      && reason_semantics formula database s.learned s.bindings
      && reason_sources_valid formula database s.learned s.bindings}) ->
    {u : unit |
      at next.bindings (variable literal) === Some None
      && reason_clause next.bindings (Bigint.of_int (variable literal))
        target (wanted literal) learned.clause
      && database_unique (learned :: database)
      && prefix_consistent formula (learned :: database) next.bindings
        next.level
      && prefix_stable formula (learned :: database) next.bindings next.level
      && reason_order formula (learned :: database) (next.learned + 1)
        next.bindings next.trail
      && reason_semantics formula (learned :: database) (next.learned + 1)
        next.bindings
      && reason_sources_valid formula (learned :: database) (next.learned + 1)
        next.bindings
      && Bigint.compare
        (count_absent (clause_universe n (2 * n))
          (learned.clause :: database_clauses database))
        (count_absent (clause_universe n (2 * n))
          (database_clauses database)) < 0} =
  fun n formula database old learned literal target next ->
  ghost_ (
    let witness = if 0 < target then
      find_level_literal old.bindings (variable literal) target learned.clause
      else Positive 0 in
    asserting_reason n old.bindings literal target
      learned.clause;
    prefix_stable_at formula database old.bindings old.level
      target;
    variable_def literal;
    wanted_def literal;
    reason_contains_literal literal learned.clause;
    reason_false_except (retained_bindings target old.bindings)
      (variable literal) target (wanted literal) learned.clause;
    if 0 < target then (
      at_def old.bindings (variable witness);
      retained_binding_at target old.bindings
        (Bigint.of_int (variable witness));
      at_def (retained_bindings target old.bindings)
        (variable witness));
    at_level_def old.bindings old.level literal;
    at_def old.bindings (variable literal);
    retained_binding_at target old.bindings
      (Bigint.of_int (variable literal));
    at_def next.bindings (variable literal);
    binding_at_bounds next.bindings
      (Bigint.of_int (variable literal));
    lookup_value next.bindings (variable literal);
    partial_literal_def (binding_values next.bindings) literal;
    unit_clause_unstable (binding_values next.bindings) literal
      learned.clause;
    formula_stable_member (binding_values next.bindings)
      (database_clauses database) learned.clause;
    formula_stable_member (binding_values next.bindings)
      formula learned.clause;
    let _ : {u : unit | unique_literals learned.clause} = () in
    unique_clause_length n learned.clause;
    clause_universe_member n (2 * n) learned.clause;
    count_absent_prepend (clause_universe n (2 * n))
      (database_clauses database) learned.clause;
    let _ : {u : unit | Bigint.compare
      (count_absent (clause_universe n (2 * n))
        (learned.clause :: database_clauses database))
      (count_absent (clause_universe n (2 * n))
        (database_clauses database)) < 0} = () in
    let _ : {u : unit |
      not (clause_member (database_clauses database) learned.clause)}
      = () in ();
    prefix_prepend formula database next.bindings next.level
      learned literal;
    prefix_stable_prepend formula database next.bindings next.level
      learned literal witness;
    reason_order_prepend formula database next.learned
      learned next.bindings next.trail;
    reason_semantics_prepend formula database next.learned
      learned next.bindings;
    reason_sources_prepend formula database
      next.learned learned next.bindings;
    database_unique_def (learned :: database));
  ()

let rec (search @ total) :
    (n : {n : int | 0 <= n && n <= 256}) @ ghost ->
    (formula : {f : formula | valid_formula n f && clauses_fit 4096 f}) ->
    (scores : int list) ->
    (database : {d : proof_result list | database_valid formula d
      && database_unique d}) ->
    (state : {s : state |
      Vox_sequence.length s.bindings === Bigint.of_int n &&
      Bigint.compare (Vox_sequence.length s.bindings) 256Z <= 0
      && prefix_consistent formula database s.bindings s.level
      && prefix_stable formula database s.bindings s.level
      && trail_levels s.bindings s.trail s.level
      && trail_covers s.bindings s.trail
      && trail_consistent s.bindings s.trail && trail_unique s.trail
      && trail_ordered s.bindings s.trail s.level
      && 0 <= s.level && levels_bounded s.level s.bindings
      && reason_sources_valid formula database s.learned s.bindings
      && reason_semantics formula database s.learned s.bindings
      && reason_order formula database s.learned s.bindings s.trail}) ->
    (fuel : {f : int option | match f with
      | None -> true | Some remaining -> 0 <= remaining}) ->
    (rank : {r : Bigint.t |
      r === progress_measure n database state.bindings}) @ ghost ->
    {r : report |
      match r.answer with
      | Sat assignment -> eval_formula assignment formula
        && Vox_sequence.length assignment ===
          Vox_sequence.length state.bindings
      | Unsat entry ->
        derivation_valid formula entry.proof
        && same_clause (conclusion formula entry.proof) entry.clause
        && entry.clause === []
      | Unknown -> match fuel with
        | None -> false
        | Some remaining -> r.statistics.steps = state.steps + remaining} =
  fun n formula scores database state fuel rank ->
  let initial_bindings = ghost_ state.bindings in
  ghost_ (progress_nonnegative n database state.bindings);
  match fuel with
  | Some 0 -> {answer = Unknown; statistics = statistics state}
  | Some _ | None ->
    let remaining = match fuel with
      | None -> None | Some amount -> Some (amount - 1) in
    let state = {state with steps = state.steps + 1} in
    ghost_ (unassigned_bounds state.bindings);
    match propagate (ghost_ n) formula database state
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
            ghost_ (decision_level_bound n state.bindings state.trail
              state.level);
            ghost_ (prefix_decision formula database state.bindings state.level;
              prefix_stable_decision formula database state.bindings
                state.level;
              at_def state.bindings v);
            ghost_ (variable_def (Positive v));
            ghost_ (levels_weaken state.bindings state.level (state.level + 1));
            ghost_ (ordered_weaken state.bindings state.trail state.level
              (state.level + 1));
            let state = {state with
              level = state.level + 1;
              decisions = state.decisions + 1} in
            ghost_ (reason_source_valid_def formula database state.learned
              Decision;
              enqueue_reason_def formula database state.learned state.bindings
                (Positive v) state.level Decision;
              implied_reason_def Decision);
            (match enqueue (ghost_ formula) (ghost_ database) state (Positive
              v) Decision with
             | None ->
               let _ : {u : unit | false} = () in
               {answer = Unknown; statistics = statistics state}
             | Some state ->
               ghost_ (progress_decision n database initial_bindings
                 state.bindings;
                 progress_nonnegative n database state.bindings);
               search (ghost_ n) formula scores database state remaining
                 (ghost_ (progress_measure n database state.bindings)))))
    | Conflict (entry, state) ->
      let state = {state with conflicts = state.conflicts + 1} in
      ghost_ (
        result_clause_valid n formula entry;
        prefix_clause_no_conflict formula database state.bindings state.level
          entry.clause;
        no_current_false_retained n state.bindings state.level entry.clause;
        let _ : {u : unit | if 0 < state.level then
          not (current_covered [] state.bindings state.level entry.clause)
          else true} = () in ());
      let stop = if state.level = 0 then 0 else 1 in
      let learned = analyze (ghost_ n) formula database state stop entry
        (ghost_ (clause_rank state.trail entry.clause)) in
      ghost_ (result_clause_valid n formula learned);
      if state.level = 0 then (
        ghost_ (
          result_clause_valid n formula learned;
          let _ = partial_of_bindings state.bindings in
          root_conflict_empty n state.bindings learned.clause);
        {answer = Unsat learned; statistics = statistics state})
      else (
        ghost_ (let _ = partial_of_bindings state.bindings in ());
        ghost_ (all_current_def [] state.bindings state.level);
        let variables = ghost_ (current_variables learned.clause
          state.bindings state.level []) in
        let literal, target =
          asserting_clause (ghost_ n) state.bindings state.level
            (ghost_ variables) learned.clause None 0 in
        let previous = ghost_ state in
        let old_level = state.level in
        let state = backtrack (ghost_ formula) (ghost_ database)
          state target in
        ghost_ (prepare_learning n formula database previous learned
          literal target state);
        let state = {state with
          learned = state.learned + 1;
          backjumps = state.backjumps +
            (if target < old_level - 1 then 1 else 0)} in
        let previous_database = ghost_ database in
        let database = database_cons formula learned database in
        let reason = Learned (store_result learned) in
        ghost_ (
          reason_source_valid_def formula database state.learned
            reason;
          enqueue_reason_def formula database state.learned
            state.bindings literal state.level reason;
          implied_reason_def reason;
          reason_source_clause_def formula database reason);
        (match enqueue (ghost_ formula) (ghost_ database) state
          literal reason with
         | None ->
           let _ : {u : unit | false} = () in
           {answer = Unknown; statistics = statistics state}
         | Some state ->
           ghost_ (progress_learning n previous_database learned
             initial_bindings state.bindings;
             progress_nonnegative n database state.bindings);
           search (ghost_ n) formula scores database state remaining
             (ghost_ (progress_measure n database state.bindings))))
[@@decreases rank]

let (start_search @ total) :
    (fuel : {f : int option | match f with
      | None -> true | Some remaining -> 0 <= remaining}) ->
    (n : {n : int | 0 <= n && n <= 256}) ->
    (formula : {f : formula | valid_formula n f && clauses_fit 4096 f}) ->
    {report : report | match report.answer with
      | Sat assignment -> check n formula assignment
      | Unsat entry -> derivation_valid formula entry.proof
        && same_clause (conclusion formula entry.proof) entry.clause
        && entry.clause === []
      | Unknown -> match fuel with
        | None -> false | Some remaining -> report.statistics.steps = remaining}
          =
  fun fuel n formula ->
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
    trail_levels_def initial.bindings [] 0;
    trail_consistent_def initial.bindings [];
    trail_unique_def [];
    trail_ordered_def initial.bindings [] 0;
    unassigned_levels initial.bindings 0;
    unassigned_covered initial.bindings initial.trail
      (Vox_sequence.length initial.bindings);
    trail_covers_def initial.bindings initial.trail);
  let scores = occurrence_scores formula n 0 in
  let database = database_empty formula in
  ghost_ (
    Vox_sequence.length_def database;
    database_unique_def database;
    unassigned_reason_sources formula database 0 initial.bindings;
    unassigned_reason_semantics formula database 0 initial.bindings;
    prefix_consistent_def formula database initial.bindings 0;
    prefix_stable_def formula database initial.bindings 0;
    reason_order_def formula database 0 initial.bindings [];
    reason_order_from_def formula database 0 initial.bindings [] []);
  let report = search (ghost_ n) formula scores database initial fuel
    (ghost_ (progress_measure n database initial.bindings)) in
  match report.answer with
  | Sat assignment ->
    ghost_ (assignment_length n assignment);
    ghost_ (check_def n formula assignment);
    report
  | Unsat _ | Unknown -> report

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
        | Unknown -> report.statistics.steps = fuel} =
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
    Ok (start_search (Some fuel) n formula)

let (solve_complete @ total) : (n : int) -> (formula : formula) ->
    {r : (report, input_error) result | match r with
      | Error Invalid_fuel -> false
      | Error (Invalid_input error) -> classify_input n formula === Some error
      | Ok report -> classify_input n formula === None
        && match report.answer with
        | Sat assignment -> check n formula assignment
        | Unsat entry -> derivation_valid formula entry.proof
          && same_clause (conclusion formula entry.proof) entry.clause
          && entry.clause === []
        | Unknown -> false} =
  fun n formula ->
  ghost_ (classify_input_def n formula);
  if n < 0 || n > 256 then Error (Invalid_input Unsupported_variable_count)
  else if not (clauses_fit 4096 formula) then Error (Invalid_input
    Too_many_clauses)
  else if not (literals_fit 65536 formula) then Error (Invalid_input
    Too_many_literals)
  else if not (valid_formula n formula) then Error (Invalid_input
    Invalid_formula)
  else Ok (start_search None n formula)

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
        | Unknown -> depth_fuel <= n && report.statistics.steps = fuel} =
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

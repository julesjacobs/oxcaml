module Dfa_proof : sig
  type row = (int * int) list * int
  type raw = int * (int * bool * row) list
  type machine = Dfa_semantics.machine
  type relation = (int * int) list
  type decision = Equal of relation | Different of int list | Limit
  [@@inductive]
  type reduction_certificate =
    relation * (int * int list) list * (int * int * int list) list

  val valid : machine -> bool @@ total
  val of_raw : raw @ total -> machine option @ total @@ total
  val of_raw_valid : (raw : raw) @ total ->
    {u : unit | match of_raw raw with
      | None -> true
      | Some machine -> Dfa_semantics.valid machine} @@ total
  val reject_all : machine @@ total
  val run : machine -> int list -> bool @@ total
  val run_from : machine -> int -> int list -> bool @@ total
  val final : machine -> int -> bool @@ total
  val step : machine -> int -> int -> int @@ total
  val raw_final : raw -> int -> bool @@ total
  val raw_step : raw -> int -> int -> int @@ total
  val raw_view : (int * bool * row) list -> int -> bool * row @@ total
  val raw_has_key : (int * bool * row) list -> int -> bool @@ total
  val raw_unique_keys : (int * bool * row) list -> bool @@ total
  val raw_unique_cons : (key : int) -> (accepting : bool) ->
    (row : row) -> (rest : (int * bool * row) list) ->
    {u : unit | raw_unique_keys ((key, accepting, row) :: rest) ===
      (not (raw_has_key rest key) && raw_unique_keys rest)} @@ total
  val raw_valid : raw -> bool @@ total
  val of_raw_raw_valid : (raw : raw) ->
    {u : unit | match of_raw raw with
      | None -> true
      | Some _ -> raw_valid raw} @@ total
  val raw_has_key_empty : (state : int) ->
    {u : unit | raw_has_key [] state === false} @@ total
  val raw_has_key_cons : (key : int) -> (accepting : bool) ->
    (row : row) -> (rest : (int * bool * row) list) -> (state : int) ->
    {u : unit | raw_has_key ((key, accepting, row) :: rest) state ===
      (key = state || raw_has_key rest state)} @@ total
  val raw_unique_head : (key : int) -> (accepting : bool) ->
    (row : row) -> (rest : (int * bool * row) list) ->
    {u : unit | if raw_unique_keys ((key, accepting, row) :: rest)
      then not (raw_has_key rest key) else true} @@ total
  val raw_tail_key_distinct : (key : int) -> (accepting : bool) ->
    (row : row) -> (rest : (int * bool * row) list) ->
    (state : int) ->
    {u : unit | if raw_unique_keys ((key, accepting, row) :: rest) &&
      raw_has_key rest state then state <> key else true} @@ total
  val raw_unique_tail : (key : int) -> (accepting : bool) ->
    (row : row) -> (rest : (int * bool * row) list) ->
    {u : unit | if raw_unique_keys ((key, accepting, row) :: rest)
      then raw_unique_keys rest else true} @@ total
  val raw_valid_unique : (raw : raw) ->
    {u : unit | let _, table = raw in
      if raw_valid raw then raw_unique_keys table else true} @@ total
  val raw_view_empty : (state : int) ->
    {u : unit | raw_view [] state === (false, ([], 0))} @@ total
  val raw_view_cons : (key : int) -> (accepting : bool) ->
    (row : row) -> (rest : (int * bool * row) list) -> (state : int) ->
    {u : unit | raw_view ((key, accepting, row) :: rest) state ===
      (if key = state then accepting, row else raw_view rest state)} @@ total
  val raw_row_step : row -> int -> int @@ total
  val raw_edge_step : (int * int) list -> int -> int -> int @@ total
  val raw_edge_step_empty : (fallback : int) -> (letter : int) ->
    {u : unit | raw_edge_step [] fallback letter === fallback} @@ total
  val raw_edge_step_cons : (label : int) -> (target : int) ->
    (rest : (int * int) list) -> (fallback : int) -> (letter : int) ->
    {u : unit | raw_edge_step ((label, target) :: rest) fallback letter ===
      (if label = letter then target
       else raw_edge_step rest fallback letter)} @@ total
  val raw_final_view : (raw : raw) -> (state : int) ->
    {u : unit | let _, table = raw in
      let accepting, _ = raw_view table state in
      raw_final raw state === accepting} @@ total
  val raw_step_view : (raw : raw) -> (state : int) -> (letter : int) ->
    {u : unit | let _, table = raw in
      let _, row = raw_view table state in
      raw_step raw state letter === raw_row_step row letter} @@ total
  val raw_row_step_edges : (edges : (int * int) list) ->
    (fallback : int) -> (letter : int) ->
    {u : unit | raw_row_step (edges, fallback) letter ===
      raw_edge_step edges fallback letter} @@ total
  val raw_run : raw -> int list -> bool @@ total
  val raw_run_from : raw -> int -> int list -> bool @@ total
  val raw_run_initial : (raw : raw) -> (word : int list) ->
    {u : unit | let initial, _ = raw in
      raw_run raw word === raw_run_from raw initial word} @@ total
  val raw_run_from_empty : (raw : raw) -> (state : int) ->
    {u : unit | raw_run_from raw state [] === raw_final raw state} @@ total
  val raw_run_from_letter : (raw : raw) -> (state : int) ->
    (letter : int) -> (suffix : int list) ->
    {u : unit | raw_run_from raw state (letter :: suffix) ===
      raw_run_from raw (raw_step raw state letter) suffix} @@ total
  val of_raw_run : (raw : raw) -> (machine : machine) ->
    (word : int list) ->
    {u : unit | if of_raw raw === Some machine then
      Dfa_semantics.run machine word === raw_run raw word else true} @@ total
  val of_raw_final : (raw : raw) -> (machine : machine) -> (state : int) ->
    {u : unit | if of_raw raw === Some machine then
      Dfa_semantics.final machine state === raw_final raw state else true} @@ total
  val of_raw_step : (raw : raw) -> (machine : machine) ->
    (state : int) -> (letter : int) ->
    {u : unit | if of_raw raw === Some machine then
      Dfa_semantics.step machine state letter === raw_step raw state letter else true}
    @@ total
  val run_from_empty : (machine : machine) -> (state : int) ->
    {u : unit | run_from machine state [] === Dfa_semantics.final machine state} @@ total
  val run_from_letter : (machine : machine) -> (state : int) ->
    (letter : int) -> (suffix : int list) ->
    {u : unit | run_from machine state (letter :: suffix) ===
      run_from machine (Dfa_semantics.step machine state letter) suffix} @@ total
  val reached : machine -> int list -> int @@ total
  val state_count : machine -> int @@ total
  val state_size : machine -> Bigint.t @@ total
  val has_state : machine -> int -> bool @@ total
  val reached_valid : (machine : machine) -> (word : int list) ->
    {u : unit | if Dfa_semantics.valid machine then Dfa_semantics.has_state machine (reached machine word)
      else true} @@ total
  val check : machine -> machine -> relation -> bool @@ total
  val valid_decision : machine -> machine -> decision -> bool @@ total
  val labels_bounded : machine -> bool @@ total
  val diagnose_comparison : (left : machine) -> (right : machine) -> (limit : int) ->
    {decision : decision | valid_decision left right decision &&
      (if Dfa_semantics.valid left && Dfa_semantics.valid right && Dfa_semantics.labels_bounded left && Dfa_semantics.labels_bounded right &&
        0 < limit && limit <= 65_536 &&
        Bigint.compare (Bigint.mul (Dfa_semantics.state_size left) (Dfa_semantics.state_size right))
          (Bigint.of_int limit) <= 0 then
        match decision with Limit -> false | Equal _ | Different _ -> true
       else true)} @@ total
  type comparison = Equivalent | Inequivalent | Comparison_limit
  [@@inductive]
  val compare : machine -> machine -> int -> comparison @ total @@ total
  val compare_complete : (left : machine) -> (right : machine) -> (limit : int) ->
    {u : unit | if Dfa_semantics.valid left && Dfa_semantics.valid right && Dfa_semantics.labels_bounded left && Dfa_semantics.labels_bounded right &&
      0 < limit && limit <= 65_536 &&
      Bigint.compare (Bigint.mul (Dfa_semantics.state_size left) (Dfa_semantics.state_size right))
        (Bigint.of_int limit) <= 0 then
      (match compare left right limit with Comparison_limit -> false
         | Equivalent | Inequivalent -> true) else true} @@ total
  val compare_equal : (left : machine) -> (right : machine) -> (limit : int) ->
    (word : int list) ->
    {u : unit | if compare left right limit === Equivalent then
      Dfa_semantics.run left word === Dfa_semantics.run right word else true} @@ total
  val comparison_witness : (left : machine) -> (right : machine) -> (limit : int) ->
    {witness : int list Ghost.t | if compare left right limit === Inequivalent then
      Dfa_semantics.run left witness.ghost <> Dfa_semantics.run right witness.ghost else true} @@ total
  val decision_correct : (left : machine) -> (right : machine) ->
    (decision : decision) -> (word : int list) ->
    {u : unit | if valid_decision left right decision then
      match decision with
      | Equal _ -> Dfa_semantics.run left word === Dfa_semantics.run right word
      | Different witness -> Dfa_semantics.run left witness <> Dfa_semantics.run right witness
      | Limit -> true else true} @@ total
  val check_reduction : machine -> machine -> reduction_certificate -> bool
    @@ total
  val reduction_preserves : (source : machine) -> (candidate : machine) ->
    (certificate : reduction_certificate) -> (word : int list) ->
    {u : unit | if check_reduction source candidate certificate then
      Dfa_semantics.run source word === Dfa_semantics.run candidate word else true} @@ total
  val access_for : (source : machine) -> (candidate : machine) ->
    (certificate : reduction_certificate) -> (state : int) ->
    {word : int list option |
      if check_reduction source candidate certificate &&
        Dfa_semantics.has_state candidate state then
        match word with
        | None -> false
        | Some word -> reached candidate word === state
      else true} @@ total
  val separating_for : (source : machine) -> (candidate : machine) ->
    (certificate : reduction_certificate) -> (p : int) -> (q : int) ->
    {word : int list option |
      if check_reduction source candidate certificate &&
        Dfa_semantics.has_state candidate p && Dfa_semantics.has_state candidate q && p <> q then
        match word with
        | None -> false
        | Some word -> run_from candidate p word <>
          run_from candidate q word
      else true} @@ total
  val access_image : reduction_certificate -> machine -> int -> int option
    @@ total
  val image_valid : (source : machine) -> (candidate : machine) ->
    (certificate : reduction_certificate) -> (other : machine) ->
    (state : int) ->
    {u : unit | if check_reduction source candidate certificate &&
      Dfa_semantics.has_state candidate state && Dfa_semantics.valid other then
      match access_image certificate other state with
      | None -> false
      | Some image -> Dfa_semantics.has_state other image
      else true} @@ total
  val images_distinct : (source : machine) -> (candidate : machine) ->
    (certificate : reduction_certificate) -> (other : machine) ->
    (relation : relation) -> (p : int) -> (q : int) ->
    {u : unit | if check_reduction source candidate certificate &&
      check candidate other relation && Dfa_semantics.has_state candidate p &&
      Dfa_semantics.has_state candidate q && p <> q then
      match access_image certificate other p,
        access_image certificate other q with
      | Some left, Some right -> left <> right
      | _ -> false else true} @@ total
  val minimum_count : (source : machine) -> (candidate : machine) ->
    (certificate : reduction_certificate) -> (other : machine) ->
    (relation : relation) ->
    {u : unit | if check_reduction source candidate certificate &&
      check candidate other relation && Dfa_semantics.valid other then
      Bigint.compare (Dfa_semantics.state_size candidate) (Dfa_semantics.state_size other) <= 0
      else true} @@ total
  val minimum_count_semantic : (source : machine) ->
    (candidate : machine) -> (certificate : reduction_certificate) ->
    (other : machine) ->
    (agreement : ((word : int list) ->
      {u : unit | Dfa_semantics.run candidate word === Dfa_semantics.run other word})) @ total ->
    {u : unit | if check_reduction source candidate certificate &&
      Dfa_semantics.valid other then
      Bigint.compare (Dfa_semantics.state_size candidate) (Dfa_semantics.state_size other) <= 0
      else true} @@ total
  val minimum_count_source_semantic : (source : machine) ->
    (candidate : machine) -> (certificate : reduction_certificate) ->
    (other : machine) ->
    (agreement : ((word : int list) ->
      {u : unit | Dfa_semantics.run source word === Dfa_semantics.run other word})) @ total ->
    {u : unit | if check_reduction source candidate certificate &&
      Dfa_semantics.valid other then
      Bigint.compare (Dfa_semantics.state_size candidate) (Dfa_semantics.state_size other) <= 0
      else true} @@ total
  val diagnose_reduction : (source : machine) -> (limit : int) ->
    {result : (machine * reduction_certificate) option |
      (if Dfa_semantics.valid source && Dfa_semantics.labels_bounded source &&
        0 < limit && limit <= 64 &&
        Bigint.compare (Dfa_semantics.state_size source) (Bigint.of_int limit) <= 0 then
        match result with None -> false | Some _ -> true else true) &&
      match result with
      | None -> true
      | Some (candidate, certificate) ->
        check_reduction source candidate certificate} @@ total
  val reduce : machine -> int -> machine option @ total @@ total
  val reduce_complete : (source : machine) -> (limit : int) ->
    {u : unit | if Dfa_semantics.valid source && Dfa_semantics.labels_bounded source &&
      0 < limit && limit <= 64 &&
      Bigint.compare (Dfa_semantics.state_size source) (Bigint.of_int limit) <= 0 then
      match reduce source limit with None -> false | Some candidate -> Dfa_semantics.valid candidate
      else true} @@ total
  val reduce_preserves : (source : machine) -> (limit : int) ->
    (word : int list) ->
    {u : unit | let result = reduce source limit in
      match result with None -> true | Some candidate ->
        Dfa_semantics.run source word === Dfa_semantics.run candidate word} @@ total
  val reduce_minimum : (source : machine) -> (limit : int) ->
    (other : machine) ->
    (agreement : ((word : int list) ->
      {u : unit | Dfa_semantics.run source word === Dfa_semantics.run other word})) @ total ->
    {u : unit | let result = reduce source limit in
      match result with None -> true | Some candidate ->
        if Dfa_semantics.valid other then
          Bigint.compare (Dfa_semantics.state_size candidate) (Dfa_semantics.state_size other) <= 0
        else true} @@ total
  val check_agrees : (left : machine) -> (right : machine) ->
    (relation : relation) -> (word : int list) ->
    {u : unit | if check left right relation then
      Dfa_semantics.run left word === Dfa_semantics.run right word else true} @@ total
  val valid_semantics : (machine : machine) -> {u : unit | valid machine === Dfa_semantics.valid machine} @@ total
  val run_semantics : (machine : machine) -> (word : int list) -> {u : unit | run machine word === Dfa_semantics.run machine word} @@ total
  val state_size_semantics : (machine : machine) -> {u : unit | state_size machine === Dfa_semantics.state_size machine} @@ total
  val labels_bounded_semantics : (machine : machine) -> {u : unit | labels_bounded machine === Dfa_semantics.labels_bounded machine} @@ total
end = struct
  open Dfa_semantics
  type row = Dfa_semantics.row
  type raw = Dfa_semantics.raw
  type machine = Dfa_semantics.machine
  type state_pair = int * int
  type relation = (int * int) list
  type decision = Equal of relation | Different of int list | Limit
  [@@inductive]
  type reduction_certificate =
    relation * (int * int list) list * (int * int * int list) list

  external ( >= ) : int -> int -> bool @@ total = "%greaterequal"
  external equal_int : int -> int -> bool @@ total = "%equal"

  let[@def] state_count (machine : machine) =
    let _, table = machine in
    List.length table

  let[@def] of_raw (raw : raw @ total) : machine option @ total =
    if valid raw then Some raw else None

  let (of_raw_valid @ total) (raw : raw @ total) :
      {u : unit | match of_raw raw with
        | None -> true
        | Some machine -> valid machine} =
    ghost_ (of_raw_def raw);
    let u = () in u

  let (of_raw_identity @ total) (raw : raw) (machine : machine) :
      {u : unit | if of_raw raw === Some machine then
        machine === raw else true} =
    ghost_ (of_raw_def raw);
    let u = () in u

  let reject_all : machine @ total = 0, [0, false, ([], 0)]

  let[@def] raw_final (raw : raw) state = final raw state

  let[@def] raw_step (raw : raw) state letter = step raw state letter

  let[@def] raw_view table state = view table state

  let[@def] raw_has_key table state = has_key state table

  let[@def] raw_unique_keys table = unique_keys table

  let (raw_unique_cons @ total) key accepting row rest :
      {u : unit | raw_unique_keys ((key, accepting, row) :: rest) ===
        (not (raw_has_key rest key) && raw_unique_keys rest)} =
    let table = (key, accepting, row) :: rest in
    ghost_ (raw_unique_keys_def table);
    ghost_ (raw_unique_keys_def rest);
    ghost_ (raw_has_key_def rest key);
    ghost_ (unique_keys_def table);
    let u = () in u

  let[@def] raw_valid (raw : raw) = valid raw

  let (of_raw_raw_valid @ total) (raw : raw) :
      {u : unit | match of_raw raw with
        | None -> true
        | Some _ -> raw_valid raw} =
    ghost_ (of_raw_def raw);
    ghost_ (raw_valid_def raw);
    let u = () in u

  let (raw_has_key_empty @ total) state :
      {u : unit | raw_has_key [] state === false} =
    let nil = [] in
    ghost_ (raw_has_key_def nil state);
    ghost_ (has_key_def state nil);
    let u = () in u

  let (raw_has_key_cons @ total) (key : int) accepting row rest
      (state : int) :
      {u : unit | raw_has_key ((key, accepting, row) :: rest) state ===
        (key = state || raw_has_key rest state)} =
    let table = (key, accepting, row) :: rest in
    ghost_ (raw_has_key_def table state);
    ghost_ (raw_has_key_def rest state);
    ghost_ (has_key_def state table);
    let u = () in u

  let (raw_unique_head @ total) (key : int) accepting row rest :
      {u : unit | if raw_unique_keys ((key, accepting, row) :: rest)
        then not (raw_has_key rest key) else true} =
    let table = (key, accepting, row) :: rest in
    ghost_ (raw_unique_keys_def table);
    ghost_ (unique_keys_def table);
    ghost_ (raw_has_key_def rest key);
    let u = () in u

  let (raw_tail_key_distinct @ total) (key : int) accepting row rest
      (state : int) :
      {u : unit | if raw_unique_keys ((key, accepting, row) :: rest) &&
        raw_has_key rest state then state <> key else true} =
    ghost_ (raw_unique_head key accepting row rest);
    let u = () in u

  let (raw_unique_tail @ total) (key : int) accepting row rest :
      {u : unit | if raw_unique_keys ((key, accepting, row) :: rest)
        then raw_unique_keys rest else true} =
    let table = (key, accepting, row) :: rest in
    ghost_ (raw_unique_keys_def table);
    ghost_ (raw_unique_keys_def rest);
    ghost_ (unique_keys_def table);
    let u = () in u

  let (raw_valid_unique @ total) (raw : raw) :
      {u : unit | let _, table = raw in
        if raw_valid raw then raw_unique_keys table else true} =
    let _, table = raw in
    ghost_ (raw_valid_def raw);
    ghost_ (valid_def raw);
    ghost_ (raw_unique_keys_def table);
    let u = () in u

  let (raw_view_empty @ total) (state : int) :
      {u : unit | raw_view [] state === (false, ([], 0))} =
    let nil = [] in
    ghost_ (raw_view_def nil state);
    ghost_ (view_def nil state);
    let u = () in u

  let (raw_view_cons @ total) (key : int) accepting row rest
      (state : int) :
      {u : unit | raw_view ((key, accepting, row) :: rest) state ===
        (if key = state then accepting, row else raw_view rest state)} =
    let table = (key, accepting, row) :: rest in
    ghost_ (raw_view_def table state);
    ghost_ (raw_view_def rest state);
    ghost_ (view_def table state);
    let u = () in u

  let[@def] raw_row_step row letter = row_step row letter

  let[@def] raw_edge_step edges fallback letter =
    edge_step edges fallback letter

  let (raw_edge_step_empty @ total) fallback letter :
      {u : unit | raw_edge_step [] fallback letter === fallback} =
    let nil = [] in
    ghost_ (raw_edge_step_def nil fallback letter);
    ghost_ (edge_step_def nil fallback letter);
    let u = () in u

  let (raw_edge_step_cons @ total) (label : int) target rest fallback
      (letter : int) :
      {u : unit | raw_edge_step ((label, target) :: rest) fallback letter ===
        (if label = letter then target
         else raw_edge_step rest fallback letter)} =
    let edges = (label, target) :: rest in
    ghost_ (raw_edge_step_def edges fallback letter);
    ghost_ (raw_edge_step_def rest fallback letter);
    ghost_ (edge_step_def edges fallback letter);
    let u = () in u

  let (raw_final_view @ total) (raw : raw) (state : int) :
      {u : unit | let _, table = raw in
        let accepting, _ = raw_view table state in
        raw_final raw state === accepting} =
    let _, table = raw in
    ghost_ (raw_final_def raw state);
    ghost_ (final_def raw state);
    ghost_ (raw_view_def table state);
    let u = () in u

  let (raw_step_view @ total) (raw : raw) (state : int) (letter : int) :
      {u : unit | let _, table = raw in
        let _, row = raw_view table state in
        raw_step raw state letter === raw_row_step row letter} =
    let _, table = raw in
    let _, row = raw_view table state in
    ghost_ (raw_step_def raw state letter);
    ghost_ (step_def raw state letter);
    ghost_ (raw_view_def table state);
    ghost_ (raw_row_step_def row letter);
    let u = () in u

  let (raw_row_step_edges @ total) edges fallback letter :
      {u : unit | raw_row_step (edges, fallback) letter ===
        raw_edge_step edges fallback letter} =
    let row = edges, fallback in
    ghost_ (raw_row_step_def row letter);
    ghost_ (raw_edge_step_def edges fallback letter);
    ghost_ (row_step_def row letter);
    let u = () in u

  let (of_raw_final @ total) (raw : raw) (machine : machine) state :
      {u : unit | if of_raw raw === Some machine then
        final machine state === raw_final raw state else true} =
    ghost_ (of_raw_def raw);
    ghost_ (raw_final_def raw state);
    let u = () in u

  let (of_raw_step @ total) (raw : raw) (machine : machine) state letter :
      {u : unit | if of_raw raw === Some machine then
        step machine state letter === raw_step raw state letter else true} =
    ghost_ (of_raw_def raw);
    ghost_ (raw_step_def raw state letter);
    let u = () in u

  let[@def] default (machine : machine) state =
    let _, table = machine in
    match view table state with _, (_, target) -> target

  let[@def] rec drive machine state word =
    match word with
    | [] -> state
    | c :: rest -> drive machine (step machine state c) rest

  let[@def] raw_run (raw : raw) word = run raw word

  let (of_raw_run @ total) (raw : raw) (machine : machine)
      (word : int list) :
      {u : unit | if of_raw raw === Some machine then
        run machine word === raw_run raw word else true} =
    ghost_ (of_raw_def raw);
    ghost_ (raw_run_def raw word);
    let u = () in u

  let[@def] run_from (machine : machine) state word =
    execute machine state word

  let rec (execute_same_table @ total) :
      (left_initial : int) -> (right_initial : int) ->
      (table : (int * bool * row) list) ->
      (state : int) -> (word : int list) ->
      {u : unit | execute (left_initial, table) state word ===
        execute (right_initial, table) state word}
        @ immutable contended =
    fun left_initial right_initial table state word ->
    let left = left_initial, table in
    let right = right_initial, table in
    ghost_ (execute_def left state word);
    ghost_ (execute_def right state word);
    let u = () in
    match word with
    | [] ->
      ghost_ (final_def left state);
      ghost_ (final_def right state);
      u
    | letter :: suffix ->
      ghost_ (step_def left state letter);
      ghost_ (step_def right state letter);
      let target = step left state letter in
      ghost_ (execute_same_table left_initial right_initial table target suffix);
      u

  let (run_rebased @ total) (source : machine) (initial : int)
      (word : int list) :
      {u : unit | let source_initial, table = source in
        run (initial, table) word === run_from source initial word} =
    let source_initial, table = source in
    let rebased = initial, table in
    ghost_ (run_def rebased word);
    ghost_ (run_from_def source initial word);
    ghost_ (execute_same_table initial source_initial table initial word);
    let u = () in u

  let[@def] raw_run_from (raw : raw) state word = run_from raw state word

  let (raw_run_initial @ total) (raw : raw) word :
      {u : unit | let initial, _ = raw in
        raw_run raw word === raw_run_from raw initial word} =
    let initial, _ = raw in
    ghost_ (raw_run_def raw word);
    ghost_ (run_def raw word);
    ghost_ (raw_run_from_def raw initial word);
    ghost_ (run_from_def raw initial word);
    let u = () in u

  let (run_from_empty @ total) (machine : machine) state :
      {u : unit | run_from machine state [] === final machine state} =
    let nil = [] in
    ghost_ (run_from_def machine state nil);
    ghost_ (execute_def machine state nil);
    let u = () in u

  let (run_from_letter @ total) (machine : machine) state letter suffix :
      {u : unit | run_from machine state (letter :: suffix) ===
        run_from machine (step machine state letter) suffix} =
    let word = letter :: suffix in
    let target = step machine state letter in
    ghost_ (run_from_def machine state word);
    ghost_ (execute_def machine state word);
    ghost_ (run_from_def machine target suffix);
    let u = () in u

  let (raw_run_from_empty @ total) (raw : raw) state :
      {u : unit | raw_run_from raw state [] === raw_final raw state} =
    let nil = [] in
    ghost_ (raw_run_from_def raw state nil);
    ghost_ (raw_final_def raw state);
    ghost_ (run_from_empty raw state);
    let u = () in u

  let (raw_run_from_letter @ total) (raw : raw) state letter suffix :
      {u : unit | raw_run_from raw state (letter :: suffix) ===
        raw_run_from raw (raw_step raw state letter) suffix} =
    let word = letter :: suffix in
    let target = raw_step raw state letter in
    ghost_ (raw_run_from_def raw state word);
    ghost_ (raw_run_from_def raw target suffix);
    ghost_ (raw_step_def raw state letter);
    ghost_ (run_from_letter raw state letter suffix);
    let u = () in u

  let[@def] reached (machine : machine) word =
    let initial, _ = machine in
    drive machine initial word

  let rec (edge_step_valid @ total) :
      (table : (int * bool * row) list) ->
      (edges : (int * int) list) -> (fallback : int) -> (c : int) ->
      {u : unit | if has_key fallback table && targets_valid table edges
        then has_key (edge_step edges fallback c) table else true}
        @ immutable contended =
    fun table edges fallback c ->
    ghost_ (edge_step_def edges fallback c);
    ghost_ (targets_valid_def table edges);
    let u = () in
    match edges with
    | [] -> u
    | (_, _) :: rest ->
      ghost_ (edge_step_valid table rest fallback c);
      u

  let rec (view_step_valid @ total) :
      (table : (int * bool * row) list) ->
      (remaining : (int * bool * row) list) ->
      (state : int) -> (c : int) ->
      {u : unit | if rows_valid table remaining && has_key state remaining
        then let _, row = view remaining state in
          has_key (row_step row c) table else true}
        @ immutable contended =
    fun table remaining state c ->
    ghost_ (rows_valid_def table remaining);
    ghost_ (has_key_def state remaining);
    ghost_ (view_def remaining state);
    let u = () in
    match remaining with
    | [] -> u
    | (key, _, (edges, fallback)) :: rest ->
      if state = key then begin
        let row = edges, fallback in
        ghost_ (row_step_def row c);
        ghost_ (edge_step_valid table edges fallback c);
        u
      end else begin
        ghost_ (view_step_valid table rest state c);
        u
      end

  let (step_valid @ total) (machine : machine) state c :
      {u : unit | if valid machine && has_state machine state
        then has_state machine (step machine state c) else true} =
    let _, table = machine in
    ghost_ (valid_def machine);
    ghost_ (has_state_def machine state);
    let next = step machine state c in
    ghost_ (step_def machine state c);
    ghost_ (has_state_def machine next);
    ghost_ (view_step_valid table table state c);
    let u = () in u

  let rec (drive_valid @ total) :
      (machine : machine) -> (state : int) -> (word : int list) ->
      {u : unit | if valid machine && has_state machine state then
        has_state machine (drive machine state word) else true}
        @ immutable contended =
    fun machine state word ->
    ghost_ (drive_def machine state word);
    let u = () in
    match word with
    | [] -> u
    | c :: rest ->
      let next = step machine state c in
      ghost_ (step_valid machine state c);
      ghost_ (drive_valid machine next rest);
      u

  let (reached_valid @ total) (machine : machine) (word : int list) :
      {u : unit | if valid machine then
        has_state machine (reached machine word) else true} =
    let initial, _ = machine in
    ghost_ (valid_def machine);
    ghost_ (has_state_def machine initial);
    let state = reached machine word in
    ghost_ (reached_def machine word);
    ghost_ (drive_valid machine initial word);
    ghost_ (has_state_def machine state);
    let u = () in u

  let[@def] rec append xs ys =
    match xs with [] -> ys | x :: rest -> x :: append rest ys

  let rec (execute_after @ total) :
      (machine : machine) -> (state : int) ->
      (prefix : int list) -> (suffix : int list) ->
      {u : unit | execute machine (drive machine state prefix) suffix ===
        execute machine state (append prefix suffix)}
        @ immutable contended =
    fun machine state prefix suffix ->
    let combined = append prefix suffix in
    ghost_ (append_def prefix suffix);
    ghost_ (drive_def machine state prefix);
    ghost_ (execute_def machine state combined);
    let u = () in
    match prefix with
    | [] -> u
    | c :: rest ->
      let next = step machine state c in
      ghost_ (execute_after machine next rest suffix);
      u

  let rec (drive_after @ total) :
      (machine : machine) -> (state : int) ->
      (prefix : int list) -> (suffix : int list) ->
      {u : unit | drive machine state (append prefix suffix) ===
        drive machine (drive machine state prefix) suffix}
        @ immutable contended =
    fun machine state prefix suffix ->
    let combined = append prefix suffix in
    ghost_ (append_def prefix suffix);
    ghost_ (drive_def machine state combined);
    ghost_ (drive_def machine state prefix);
    let u = () in
    match prefix with
    | [] -> u
    | c :: rest ->
      let next = step machine state c in
      ghost_ (drive_after machine next rest suffix);
      u

  let rec (execute_reached @ total) :
      (machine : machine) -> (state : int) -> (word : int list) ->
      {u : unit | execute machine state word ===
        final machine (drive machine state word)}
        @ immutable contended =
    fun machine state word ->
    ghost_ (execute_def machine state word);
    ghost_ (drive_def machine state word);
    let u = () in
    match word with
    | [] -> u
    | c :: rest ->
      let next = step machine state c in
      ghost_ (execute_reached machine next rest);
      u

  let[@def] rec has_letter (c : int) (letters : int list) =
    match letters with [] -> false | x :: rest -> c = x || has_letter c rest

  let[@def] rec distinct_ints xs =
    match xs with
    | [] -> true
    | x :: rest -> not (has_letter x rest) && distinct_ints rest

  let[@def] rec included xs ys =
    match xs with
    | [] -> true
    | x :: rest -> has_letter x ys && included rest ys

  external same_int : int -> int -> bool @@ total = "%equal"

  let[@def] rec remove_one x ys =
    match ys with
    | [] -> []
    | y :: rest -> if same_int x y then rest else y :: remove_one x rest

  let rec (remove_preserves_other @ total) :
      (x : int) -> (y : int) -> (ys : int list) ->
      {u : unit | if x <> y && has_letter y ys then
        has_letter y (remove_one x ys) else true}
        @ immutable contended =
    fun x y ys ->
    ghost_ (has_letter_def y ys);
    ghost_ (remove_one_def x ys);
    let u = () in
    match ys with
    | [] -> u
    | head :: rest ->
      let filtered = remove_one x ys in
      ghost_ (has_letter_def y rest);
      ghost_ (has_letter_def y filtered);
      ghost_ (remove_preserves_other x y rest);
      u

  let rec (remove_size @ total) :
      (x : int) -> (ys : int list) ->
      {u : unit | if has_letter x ys then
        big_length ys === Bigint.add 1Z (big_length (remove_one x ys))
        else true} @ immutable contended =
    fun x ys ->
    ghost_ (has_letter_def x ys);
    ghost_ (big_length_def ys);
    ghost_ (remove_one_def x ys);
    let u = () in
    match ys with
    | [] -> u
    | _ :: rest ->
      let filtered = remove_one x ys in
      let filtered_rest = remove_one x rest in
      ghost_ (big_length_def filtered);
      ghost_ (big_length_def rest);
      ghost_ (big_length_def filtered_rest);
      ghost_ (has_letter_def x rest);
      ghost_ (remove_size x rest);
      u

  let rec (included_remove @ total) :
      (x : int) -> (xs : int list) -> (ys : int list) ->
      {u : unit | if not (has_letter x xs) && included xs ys then
        included xs (remove_one x ys) else true}
        @ immutable contended =
    fun x xs ys ->
    ghost_ (has_letter_def x xs);
    ghost_ (included_def xs ys);
    let filtered = remove_one x ys in
    ghost_ (included_def xs filtered);
    let u = () in
    match xs with
    | [] -> u
    | head :: rest ->
      ghost_ (remove_preserves_other x head ys);
      ghost_ (included_remove x rest ys);
      u

  let rec (big_length_nonnegative @ total) :
      (xs : int list) ->
      {u : unit | Bigint.compare 0Z (big_length xs) <= 0}
        @ immutable contended =
    fun xs ->
    ghost_ (big_length_def xs);
    let u = () in
    match xs with
    | [] -> u
    | _ :: rest ->
      ghost_ (big_length_nonnegative rest);
      u

  let rec (included_bound @ total) :
      (xs : int list) -> (ys : int list) ->
      {u : unit | if distinct_ints xs && included xs ys then
        Bigint.compare (big_length xs) (big_length ys) <= 0 else true}
        @ immutable contended =
    fun xs ys ->
    ghost_ (distinct_ints_def xs);
    ghost_ (included_def xs ys);
    ghost_ (big_length_def xs);
    let u = () in
    match xs with
    | [] ->
      ghost_ (big_length_nonnegative ys);
      u
    | head :: rest ->
      let filtered = remove_one head ys in
      ghost_ (included_remove head rest ys);
      ghost_ (remove_size head ys);
      ghost_ (included_bound rest filtered);
      u

  let[@def] same_pair (pair : int * int) (other : int * int) =
    let p, q = pair in
    let r, s = other in
    p = r && q = s

  let (same_pair_correct @ total) (pair : int * int) (other : int * int) :
      {u : unit | same_pair pair other === (pair === other)} =
    ghost_ (same_pair_def pair other);
    let u = () in u

  let[@def] rec related (pair : int * int) (relation : relation) =
    match relation with
    | [] -> false
    | head :: rest -> same_pair pair head || related pair rest

  let[@def] rec labelled_closed left right p q letters relation =
    match letters with
    | [] -> true
    | c :: rest ->
      related (step left p c, step right q c) relation &&
      labelled_closed left right p q rest relation

  let[@def] pair_closed left right p q relation =
    final left p = final right q &&
    related (default left p, default right q) relation &&
    labelled_closed left right p q
      (append (labels left p) (labels right q)) relation

  let[@def] rec all_closed left right relation pairs =
    match pairs with
    | [] -> true
    | (p, q) :: rest ->
      pair_closed left right p q relation &&
      all_closed left right relation rest

  let[@def] check left right relation =
    let left_initial, _ = left in
    let right_initial, _ = right in
    related (left_initial, right_initial) relation &&
    all_closed left right relation relation

  let rec (append_letter @ total) :
      (xs : int list) -> (ys : int list) -> (c : int) ->
      {u : unit | has_letter c (append xs ys) ===
        (has_letter c xs || has_letter c ys)} @ immutable contended =
    fun xs ys c ->
    let joined = append xs ys in
    ghost_ (append_def xs ys);
    ghost_ (has_letter_def c xs);
    ghost_ (has_letter_def c joined);
    let u = () in
    match xs with
    | [] -> u
    | _ :: rest ->
      ghost_ (append_letter rest ys c);
      u

  let rec (edge_outside @ total) : (edges : (int * int) list) ->
      (default : int) -> (c : int) ->
      {u : unit | if not (has_letter c (edge_labels edges))
        then edge_step edges default c === default
        else true} @ immutable contended =
    fun edges default c ->
    let letters = edge_labels edges in
    ghost_ (edge_labels_def edges);
    ghost_ (edge_step_def edges default c);
    ghost_ (has_letter_def c letters);
    let u = () in
    match edges with
    | [] -> u
    | (label, _) :: rest ->
      ghost_ (edge_outside rest default c);
      u

  let (row_outside @ total) (row : row) c :
      {u : unit | if not (has_letter c (row_labels row))
        then row_step row c === (match row with _, default -> default)
        else true} =
    let edges, default = row in
    ghost_ (row_labels_def row);
    ghost_ (row_step_def row c);
    ghost_ (edge_outside edges default c);
    let u = () in u

  let (step_outside @ total) (machine : machine) state c :
      {u : unit | if not (has_letter c (labels machine state))
        then step machine state c === default machine state else true} =
    let _, table = machine in
    let _, row = view table state in
    ghost_ (labels_def machine state);
    ghost_ (step_def machine state c);
    ghost_ (default_def machine state);
    ghost_ (row_outside row c);
    let u = () in u

  let rec (labelled_closed_member @ total) :
      (left : machine) -> (right : machine) -> (p : int) -> (q : int) ->
      (letters : int list) -> (relation : relation) -> (c : int) ->
      {u : unit | if labelled_closed left right p q letters relation &&
        has_letter c letters then
        related (step left p c, step right q c) relation else true}
        @ immutable contended =
    fun left right p q letters relation c ->
    ghost_ (labelled_closed_def left right p q letters relation);
    ghost_ (has_letter_def c letters);
    let u = () in
    match letters with
    | [] -> u
    | head :: rest ->
      ghost_ (labelled_closed_member left right p q rest relation c);
      u

  let rec (all_closed_member @ total) :
      (left : machine) -> (right : machine) -> (relation : relation) ->
      (pairs : relation) -> (p : int) -> (q : int) ->
      {u : unit | if all_closed left right relation pairs &&
        related (p, q) pairs then pair_closed left right p q relation else true}
        @ immutable contended =
    fun left right relation pairs p q ->
    let pair = (p, q) in
    ghost_ (all_closed_def left right relation pairs);
    ghost_ (related_def pair pairs);
    let u = () in
    match pairs with
    | [] -> u
    | ((head_p, head_q) as head) :: rest ->
      ghost_ (same_pair_correct pair head);
      ghost_ (all_closed_member left right relation rest p q);
      u

  let (closed_step @ total) (left : machine) (right : machine)
      (relation : relation) p q c :
      {u : unit | if pair_closed left right p q relation then
        related (step left p c, step right q c) relation else true} =
    let left_labels = labels left p in
    let right_labels = labels right q in
    let joined = append left_labels right_labels in
    ghost_ (pair_closed_def left right p q relation);
    ghost_ (append_letter left_labels right_labels c);
    let u = () in
    if has_letter c joined then begin
      ghost_ (labelled_closed_member left right p q joined relation c);
      u
    end else begin
      ghost_ (step_outside left p c);
      ghost_ (step_outside right q c);
      u
    end

  let rec (labelled_closed_rebased @ total) :
      (source : machine) -> (left_initial : int) -> (right_initial : int) ->
      (p : int) -> (q : int) -> (letters : int list) ->
      (relation : relation) ->
      {u : unit | let _, table = source in
        labelled_closed (left_initial, table) (right_initial, table)
          p q letters relation ===
        labelled_closed source source p q letters relation}
        @ immutable contended =
    fun source left_initial right_initial p q letters relation ->
    let _, table = source in
    let left = left_initial, table in
    let right = right_initial, table in
    ghost_ (labelled_closed_def left right p q letters relation);
    ghost_ (labelled_closed_def source source p q letters relation);
    let u = () in
    match letters with
    | [] -> u
    | letter :: rest ->
      ghost_ (step_def left p letter);
      ghost_ (step_def right q letter);
      ghost_ (step_def source p letter);
      ghost_ (step_def source q letter);
      ghost_ (labelled_closed_rebased source left_initial right_initial p q rest
        relation);
      u

  let rec (all_closed_rebased @ total) :
      (source : machine) -> (left_initial : int) -> (right_initial : int) ->
      (relation : relation) -> (pairs : relation) ->
      {u : unit | let _, table = source in
        all_closed (left_initial, table) (right_initial, table)
          relation pairs === all_closed source source relation pairs}
        @ immutable contended =
    fun source left_initial right_initial relation pairs ->
    let _, table = source in
    let left = left_initial, table in
    let right = right_initial, table in
    ghost_ (all_closed_def left right relation pairs);
    ghost_ (all_closed_def source source relation pairs);
    let u = () in
    match pairs with
    | [] -> u
    | (p, q) :: rest ->
      ghost_ (pair_closed_def left right p q relation);
      ghost_ (pair_closed_def source source p q relation);
      ghost_ (final_def left p);
      ghost_ (final_def right q);
      ghost_ (final_def source p);
      ghost_ (final_def source q);
      ghost_ (default_def left p);
      ghost_ (default_def right q);
      ghost_ (default_def source p);
      ghost_ (default_def source q);
      ghost_ (labels_def left p);
      ghost_ (labels_def right q);
      ghost_ (labels_def source p);
      ghost_ (labels_def source q);
      let letters = append (labels source p) (labels source q) in
      ghost_ (labelled_closed_rebased source left_initial right_initial p q letters
        relation);
      ghost_ (all_closed_rebased source left_initial right_initial relation rest);
      u

  let rec (execute_agrees @ total) :
      (left : machine) -> (right : machine) -> (relation : relation) ->
      (p : int) -> (q : int) -> (word : int list) ->
      {u : unit | if all_closed left right relation relation &&
        related (p, q) relation then
        execute left p word === execute right q word else true}
        @ immutable contended =
    fun left right relation p q word ->
    ghost_ (execute_def left p word);
    ghost_ (execute_def right q word);
    ghost_ (all_closed_member left right relation relation p q);
    let u = () in
    match word with
    | [] ->
      ghost_ (pair_closed_def left right p q relation);
      u
    | c :: rest ->
      ghost_ (closed_step left right relation p q c);
      let next_p = step left p c in
      let next_q = step right q c in
      ghost_ (execute_agrees left right relation
        next_p next_q rest);
      u

  let (check_agrees @ total) (left : machine) (right : machine)
      (relation : relation) (word : int list) :
      {u : unit | if check left right relation then
        run left word === run right word else true} =
    let left_initial, _ = left in
    let right_initial, _ = right in
    ghost_ (check_def left right relation);
    ghost_ (run_def left word);
    ghost_ (run_def right word);
    ghost_ (execute_agrees left right relation left_initial right_initial word);
    let u = () in u

  let[@def] valid_decision (left : machine) (right : machine)
      (decision : decision) =
    match decision with
    | Equal relation -> check left right relation
    | Different word -> run left word <> run right word
    | Limit -> true

  let (decision_correct @ total) (left : machine) (right : machine)
      (decision : decision) (word : int list) :
      {u : unit | if valid_decision left right decision then
        match decision with
        | Equal _ -> run left word === run right word
        | Different witness -> run left witness <> run right witness
        | Limit -> true else true} =
    ghost_ (valid_decision_def left right decision);
    let u = () in
    match decision with
    | Equal relation ->
      ghost_ (check_agrees left right relation word);
      u
    | Different _ | Limit -> u

  let rec (ids_member @ total) : (table : (int * bool * row) list) ->
      (state : int) ->
      {u : unit | has_letter state (state_ids table) === has_key state table}
      @ immutable contended =
    fun table state ->
    let states = state_ids table in
    ghost_ (state_ids_def table);
    ghost_ (has_key_def state table);
    ghost_ (has_letter_def state states);
    let u = () in
    match table with
    | [] -> u
    | _ :: rest ->
      ghost_ (ids_member rest state);
      u

  let rec (included_weaken @ total) :
      (head : int) -> (xs : int list) -> (ys : int list) ->
      {u : unit | if included xs ys then included xs (head :: ys) else true}
        @ immutable contended =
    fun head xs ys ->
    ghost_ (included_def xs ys);
    let larger = head :: ys in
    ghost_ (included_def xs larger);
    let u = () in
    match xs with
    | [] -> u
    | state :: rest ->
      ghost_ (has_letter_def state larger);
      ghost_ (included_weaken head rest ys);
      u

  let rec (self_included @ total) :
      (states : int list) -> {u : unit | included states states}
        @ immutable contended =
    fun states ->
    ghost_ (included_def states states);
    let u = () in
    match states with
    | [] -> u
    | state :: rest ->
      ghost_ (has_letter_def state states);
      ghost_ (self_included rest);
      ghost_ (included_weaken state rest rest);
      u

  let rec (state_ids_unique @ total) :
      (table : (int * bool * row) list) ->
      {u : unit | if unique_keys table then
        distinct_ints (state_ids table) else true}
        @ immutable contended =
    fun table ->
    let states = state_ids table in
    ghost_ (unique_keys_def table);
    ghost_ (state_ids_def table);
    ghost_ (distinct_ints_def states);
    let u = () in
    match table with
    | [] -> u
    | (state, _, _) :: rest ->
      ghost_ (ids_member rest state);
      ghost_ (state_ids_unique rest);
      u

  let[@def] rec access_word (state : int)
      (access : (int * int list) list) =
    match access with
    | [] -> None
    | (key, word) :: rest ->
      if state = key then Some word else access_word state rest

  let[@def] rec all_access (machine : machine) (states : int list)
      (access : (int * int list) list) =
    let initial, _ = machine in
    match states with
    | [] -> true
    | state :: rest ->
      (match access_word state access with
       | None -> false
       | Some word -> drive machine initial word = state) &&
      all_access machine rest access

  let rec (all_access_member @ total) :
      (machine : machine) -> (states : int list) ->
      (access : (int * int list) list) -> (state : int) ->
      {u : unit | if all_access machine states access &&
        has_letter state states then
        match access_word state access with
        | None -> false
        | Some word -> let initial, _ = machine in
          drive machine initial word === state
        else true} @ immutable contended =
    fun machine states access state ->
    let word = access_word state access in
    ghost_ (all_access_def machine states access);
    ghost_ (has_letter_def state states);
    let u = () in
    match states with
    | [] -> u
    | _ :: rest ->
      ghost_ (all_access_member machine rest access state);
      (match word with None -> u | Some _ -> u)

  let[@def] rec separating_word (p : int) (q : int)
      (separate : (int * int * int list) list) =
    match separate with
    | [] -> None
    | (left, right, word) :: rest ->
      if (p = left && q = right) || (p = right && q = left)
      then Some word
      else separating_word p q rest

  let[@def] rec (separates_from @ total) :
      machine -> int -> int list ->
      (int * int * int list) list -> bool @ immutable contended =
    fun machine p others separate ->
    match others with
    | [] -> true
    | q :: rest ->
      (p = q ||
       (match separating_word p q separate with
        | None -> false
       | Some word -> execute machine p word <> execute machine q word)) &&
      separates_from machine p rest separate

  let (separates_from_empty @ total) (machine : machine)
      (p : int) (separate : (int * int * int list) list) :
      {u : unit | separates_from machine p [] separate} =
    let empty = [] in
    ghost_ (separates_from_def machine p empty separate);
    let u = () in u

  let rec (separates_from_prepend_valid @ total) :
      (machine : machine) -> (p : int) -> (others : int list) ->
      (separate : (int * int * int list) list) ->
      (left : int) -> (right : int) -> (word : int list) ->
      {u : unit | if separates_from machine p others separate &&
        execute machine left word <> execute machine right word then
        separates_from machine p others
          ((left, right, word) :: separate) else true}
        @ immutable contended =
    fun machine p others separate left right word ->
    let extended = (left, right, word) :: separate in
    ghost_ (separates_from_def machine p others separate);
    ghost_ (separates_from_def machine p others extended);
    let u = () in
    match others with
    | [] -> u
    | q :: rest ->
      ghost_ (separating_word_def p q extended);
      ghost_ (separates_from_prepend_valid machine p rest separate left right word);
      u

  let rec (separates_from_member @ total) :
      (machine : machine) -> (p : int) -> (others : int list) ->
      (separate : (int * int * int list) list) -> (q : int) ->
      {u : unit | if separates_from machine p others separate &&
        has_letter q others && p <> q then
        match separating_word p q separate with
        | None -> false
        | Some word -> execute machine p word <>
          execute machine q word
        else true} @ immutable contended =
    fun machine p others separate q ->
    ghost_ (separates_from_def machine p others separate);
    ghost_ (has_letter_def q others);
    let u = () in
    match others with
    | [] -> u
    | _ :: rest ->
      ghost_ (separates_from_member machine p rest separate q);
      u

  let[@def] rec all_separated (machine : machine) (states : int list)
      (remaining : int list) (separate : (int * int * int list) list) =
    match remaining with
    | [] -> true
    | p :: rest ->
      separates_from machine p states separate &&
      all_separated machine states rest separate

  let rec (all_separated_member @ total) :
      (machine : machine) -> (states : int list) ->
      (remaining : int list) ->
      (separate : (int * int * int list) list) -> (p : int) ->
      (q : int) ->
      {u : unit | if all_separated machine states remaining separate &&
        has_letter p remaining && has_letter q states && p <> q then
        match separating_word p q separate with
        | None -> false
        | Some word -> execute machine p word <>
          execute machine q word
        else true} @ immutable contended =
    fun machine states remaining separate p q ->
    ghost_ (all_separated_def machine states remaining separate);
    ghost_ (has_letter_def p remaining);
    let u = () in
    match remaining with
    | [] -> u
    | _ :: rest ->
      ghost_ (separates_from_member machine p states separate q);
      ghost_ (all_separated_member machine states rest separate p q);
      u

  let rec (all_separated_prepend_valid @ total) :
      (machine : machine) -> (states : int list) ->
      (remaining : int list) ->
      (separate : (int * int * int list) list) ->
      (left : int) -> (right : int) -> (word : int list) ->
      {u : unit | if all_separated machine states remaining separate &&
        execute machine left word <> execute machine right word then
        all_separated machine states remaining
          ((left, right, word) :: separate) else true}
        @ immutable contended =
    fun machine states remaining separate left right word ->
    let extended = (left, right, word) :: separate in
    ghost_ (all_separated_def machine states remaining separate);
    ghost_ (all_separated_def machine states remaining extended);
    let u = () in
    match remaining with
    | [] -> u
    | p :: rest ->
      ghost_ (separates_from_prepend_valid machine p states separate
        left right word);
      ghost_ (all_separated_prepend_valid machine states rest separate
        left right word);
      u

  let[@def] check_reduction (source : machine) (candidate : machine)
      (certificate : reduction_certificate) =
    let equivalent, access, separate = certificate in
    let _, table = candidate in
    let states = state_ids table in
    check source candidate equivalent &&
    unique_keys table &&
    all_access candidate states access &&
    all_separated candidate states states separate

  let (reduction_preserves @ total) (source : machine)
      (candidate : machine) (certificate : reduction_certificate)
      (word : int list) :
      {u : unit | if check_reduction source candidate certificate then
        run source word === run candidate word else true} =
    let equivalent, _, _ = certificate in
    ghost_ (check_reduction_def source candidate certificate);
    ghost_ (check_agrees source candidate equivalent word);
    let u = () in u

  let (access_for @ total) (source : machine) (candidate : machine)
      (certificate : reduction_certificate) state :
      {word : int list option |
        if check_reduction source candidate certificate &&
          has_state candidate state then
          match word with
          | None -> false
          | Some word -> reached candidate word === state
        else true} =
    let _, access, _ = certificate in
    let _, table = candidate in
    let states = state_ids table in
    let word = access_word state access in
    ghost_ (check_reduction_def source candidate certificate);
    ghost_ (has_state_def candidate state);
    ghost_ (ids_member table state);
    ghost_ (all_access_member candidate states access state);
    match word with
    | None -> word
    | Some found ->
      ghost_ (reached_def candidate found);
      word

  let (separating_for @ total) (source : machine) (candidate : machine)
      (certificate : reduction_certificate) (p : int) (q : int) :
      {word : int list option |
        if check_reduction source candidate certificate &&
          has_state candidate p && has_state candidate q && p <> q then
          match word with
          | None -> false
          | Some word -> run_from candidate p word <>
            run_from candidate q word
        else true} =
    let _, _, separate = certificate in
    let _, table = candidate in
    let states = state_ids table in
    let word = separating_word p q separate in
    ghost_ (check_reduction_def source candidate certificate);
    ghost_ (has_state_def candidate p);
    ghost_ (has_state_def candidate q);
    ghost_ (ids_member table p);
    ghost_ (ids_member table q);
    ghost_ (all_separated_member candidate states states separate p q);
    match word with
    | None -> word
    | Some found ->
      ghost_ (run_from_def candidate p found);
      ghost_ (run_from_def candidate q found);
      word

  let (distinct_images @ total) (source : machine) (candidate : machine)
      (certificate : reduction_certificate) (other : machine)
      (relation : relation) (p : int) (q : int) :
      {u : unit | if check_reduction source candidate certificate &&
        check candidate other relation && has_state candidate p &&
        has_state candidate q && p <> q then
        let _, access, separate = certificate in
        match access_word p access, access_word q access,
          separating_word p q separate with
        | Some up, Some uq, Some _ ->
          reached other up <> reached other uq
        | _ -> false
        else true} =
    let _, access, separate = certificate in
    let _, candidate_table = candidate in
    let candidate_initial, _ = candidate in
    let other_initial, _ = other in
    let states = state_ids candidate_table in
    let up = access_word p access in
    let uq = access_word q access in
    let suffix = separating_word p q separate in
    ghost_ (check_reduction_def source candidate certificate);
    ghost_ (has_state_def candidate p);
    ghost_ (has_state_def candidate q);
    ghost_ (ids_member candidate_table p);
    ghost_ (ids_member candidate_table q);
    ghost_ (all_access_member candidate states access p);
    ghost_ (all_access_member candidate states access q);
    ghost_ (all_separated_member candidate states states separate p q);
    let u = () in
    match up, uq, suffix with
    | Some wp, Some wq, Some separating ->
      let first = append wp separating in
      let second = append wq separating in
      ghost_ (reached_def candidate wp);
      ghost_ (reached_def candidate wq);
      ghost_ (run_from_def candidate p separating);
      ghost_ (run_from_def candidate q separating);
      ghost_ (execute_after candidate candidate_initial wp separating);
      ghost_ (execute_after candidate candidate_initial wq separating);
      ghost_ (execute_after other other_initial wp separating);
      ghost_ (execute_after other other_initial wq separating);
      ghost_ (check_agrees candidate other relation first);
      ghost_ (check_agrees candidate other relation second);
      ghost_ (run_def candidate first);
      ghost_ (run_def candidate second);
      ghost_ (run_def other first);
      ghost_ (run_def other second);
      ghost_ (reached_def other wp);
      ghost_ (reached_def other wq);
      u
    | _ -> u

  let (distinct_images_semantic @ total) (source : machine)
      (candidate : machine) (certificate : reduction_certificate)
      (other : machine)
      (agreement : ((word : int list) ->
        {u : unit | run candidate word === run other word}) @ total)
      (p : int) (q : int) :
      {u : unit | if check_reduction source candidate certificate &&
        has_state candidate p && has_state candidate q && p <> q then
        let _, access, separate = certificate in
        match access_word p access, access_word q access,
          separating_word p q separate with
        | Some up, Some uq, Some _ ->
          reached other up <> reached other uq
        | _ -> false
        else true} =
    let _, access, separate = certificate in
    let _, candidate_table = candidate in
    let candidate_initial, _ = candidate in
    let other_initial, _ = other in
    let states = state_ids candidate_table in
    let up = access_word p access in
    let uq = access_word q access in
    let suffix = separating_word p q separate in
    ghost_ (check_reduction_def source candidate certificate);
    ghost_ (has_state_def candidate p);
    ghost_ (has_state_def candidate q);
    ghost_ (ids_member candidate_table p);
    ghost_ (ids_member candidate_table q);
    ghost_ (all_access_member candidate states access p);
    ghost_ (all_access_member candidate states access q);
    ghost_ (all_separated_member candidate states states separate p q);
    let u = () in
    match up, uq, suffix with
    | Some wp, Some wq, Some separating ->
      let first = append wp separating in
      let second = append wq separating in
      ghost_ (reached_def candidate wp);
      ghost_ (reached_def candidate wq);
      ghost_ (run_from_def candidate p separating);
      ghost_ (run_from_def candidate q separating);
      ghost_ (execute_after candidate candidate_initial wp separating);
      ghost_ (execute_after candidate candidate_initial wq separating);
      ghost_ (execute_after other other_initial wp separating);
      ghost_ (execute_after other other_initial wq separating);
      agreement first;
      agreement second;
      ghost_ (run_def candidate first);
      ghost_ (run_def candidate second);
      ghost_ (run_def other first);
      ghost_ (run_def other second);
      ghost_ (reached_def other wp);
      ghost_ (reached_def other wq);
      u
    | _ -> u

  let[@def] access_image (certificate : reduction_certificate)
      (other : machine) state =
    let _, access, _ = certificate in
    match access_word state access with
    | None -> None
    | Some word -> Some (reached other word)

  let (image_valid @ total) (source : machine) (candidate : machine)
      (certificate : reduction_certificate) (other : machine) state :
      {u : unit | if check_reduction source candidate certificate &&
        has_state candidate state && valid other then
        match access_image certificate other state with
        | None -> false
        | Some image -> has_state other image
        else true} =
    let _, access, _ = certificate in
    let _, table = candidate in
    let states = state_ids table in
    let word = access_word state access in
    ghost_ (check_reduction_def source candidate certificate);
    ghost_ (has_state_def candidate state);
    ghost_ (ids_member table state);
    ghost_ (all_access_member candidate states access state);
    ghost_ (access_image_def certificate other state);
    let u = () in
    match word with
    | None -> u
    | Some found ->
      let image = reached other found in
      ghost_ (reached_valid other found);
      ghost_ (has_state_def other image);
      u

  let (images_distinct @ total) (source : machine) (candidate : machine)
      (certificate : reduction_certificate) (other : machine)
      (relation : relation) (p : int) (q : int) :
      {u : unit | if check_reduction source candidate certificate &&
        check candidate other relation && has_state candidate p &&
        has_state candidate q && p <> q then
        match access_image certificate other p,
          access_image certificate other q with
        | Some left, Some right -> left <> right
        | _ -> false else true} =
    ghost_ (distinct_images source candidate certificate other relation p q);
    ghost_ (access_image_def certificate other p);
    ghost_ (access_image_def certificate other q);
    let u = () in u

  let[@def] image_number certificate other state =
    match access_image certificate other state with
    | None -> 0
    | Some image -> image

  let[@def] rec image_ids certificate other states =
    match states with
    | [] -> []
    | state :: rest ->
      image_number certificate other state :: image_ids certificate other rest

  let rec (image_ids_length @ total) :
      (certificate : reduction_certificate) -> (other : machine) ->
      (states : int list) ->
      {u : unit | big_length (image_ids certificate other states) ===
        big_length states} @ immutable contended =
    fun certificate other states ->
    let images = image_ids certificate other states in
    ghost_ (image_ids_def certificate other states);
    ghost_ (big_length_def images);
    ghost_ (big_length_def states);
    let u = () in
    match states with
    | [] -> u
    | _ :: rest ->
      ghost_ (image_ids_length certificate other rest);
      u

  let (image_number_valid @ total) (source : machine) (candidate : machine)
      (certificate : reduction_certificate) (other : machine) state :
      {u : unit | if check_reduction source candidate certificate &&
        has_state candidate state && valid other then
        has_state other (image_number certificate other state) else true} =
    ghost_ (image_valid source candidate certificate other state);
    ghost_ (image_number_def certificate other state);
    let u = () in u

  let (image_numbers_distinct @ total) (source : machine)
      (candidate : machine) (certificate : reduction_certificate)
      (other : machine) (relation : relation) (p : int) (q : int) :
      {u : unit | if check_reduction source candidate certificate &&
        check candidate other relation && has_state candidate p &&
        has_state candidate q && p <> q then
        image_number certificate other p <>
        image_number certificate other q else true} =
    ghost_ (images_distinct source candidate certificate other relation p q);
    ghost_ (image_number_def certificate other p);
    ghost_ (image_number_def certificate other q);
    let u = () in u

  let (image_numbers_distinct_semantic @ total) (source : machine)
      (candidate : machine) (certificate : reduction_certificate)
      (other : machine)
      (agreement : ((word : int list) ->
        {u : unit | run candidate word === run other word}) @ total)
      (p : int) (q : int) :
      {u : unit | if check_reduction source candidate certificate &&
        has_state candidate p && has_state candidate q && p <> q then
        image_number certificate other p <>
        image_number certificate other q else true} =
    ghost_ (distinct_images_semantic source candidate certificate other agreement p q);
    ghost_ (access_image_def certificate other p);
    ghost_ (access_image_def certificate other q);
    ghost_ (image_number_def certificate other p);
    ghost_ (image_number_def certificate other q);
    let u = () in u

  let rec (image_ids_included @ total) :
      (source : machine) -> (candidate : machine) ->
      (certificate : reduction_certificate) -> (other : machine) ->
      (states : int list) ->
      {u : unit | if check_reduction source candidate certificate &&
        valid other &&
        (let _, candidate_table = candidate in
         included states (state_ids candidate_table)) then
        let _, other_table = other in
        included (image_ids certificate other states)
          (state_ids other_table) else true}
        @ immutable contended =
    fun source candidate certificate other states ->
    let _, candidate_table = candidate in
    let _, other_table = other in
    let candidate_states = state_ids candidate_table in
    let other_states = state_ids other_table in
    let images = image_ids certificate other states in
    ghost_ (included_def states candidate_states);
    ghost_ (image_ids_def certificate other states);
    ghost_ (included_def images other_states);
    let u = () in
    match states with
    | [] -> u
    | state :: rest ->
      let image = image_number certificate other state in
      let tail_images = image_ids certificate other rest in
      ghost_ (included_def rest candidate_states);
      ghost_ (included_def tail_images other_states);
      ghost_ (has_letter_def state candidate_states);
      ghost_ (has_state_def candidate state);
      ghost_ (ids_member candidate_table state);
      ghost_ (image_number_valid source candidate certificate other state);
      ghost_ (has_state_def other image);
      ghost_ (ids_member other_table image);
      ghost_ (has_letter_def image other_states);
      ghost_ (image_ids_included source candidate certificate other rest);
      u

  let rec (image_not_member @ total) :
      (source : machine) -> (candidate : machine) ->
      (certificate : reduction_certificate) -> (other : machine) ->
      (relation : relation) -> (state : int) -> (states : int list) ->
      {u : unit | if check_reduction source candidate certificate &&
        check candidate other relation && has_state candidate state &&
        not (has_letter state states) &&
        (let _, table = candidate in included states (state_ids table)) then
        not (has_letter (image_number certificate other state)
          (image_ids certificate other states)) else true}
        @ immutable contended =
    fun source candidate certificate other relation state states ->
    let _, table = candidate in
    let candidate_states = state_ids table in
    let image = image_number certificate other state in
    let images = image_ids certificate other states in
    ghost_ (has_letter_def state states);
    ghost_ (included_def states candidate_states);
    ghost_ (image_ids_def certificate other states);
    ghost_ (has_letter_def image images);
    let u = () in
    match states with
    | [] -> u
    | head :: rest ->
      ghost_ (has_state_def candidate head);
      ghost_ (ids_member table head);
      ghost_ (image_numbers_distinct source candidate certificate other relation
        state head);
      ghost_ (image_not_member source candidate certificate other relation state rest);
      u

  let rec (image_ids_distinct @ total) :
      (source : machine) -> (candidate : machine) ->
      (certificate : reduction_certificate) -> (other : machine) ->
      (relation : relation) -> (states : int list) ->
      {u : unit | if check_reduction source candidate certificate &&
        check candidate other relation && distinct_ints states &&
        (let _, table = candidate in included states (state_ids table)) then
        distinct_ints (image_ids certificate other states) else true}
        @ immutable contended =
    fun source candidate certificate other relation states ->
    let images = image_ids certificate other states in
    let _, table = candidate in
    let candidate_states = state_ids table in
    ghost_ (distinct_ints_def states);
    ghost_ (included_def states candidate_states);
    ghost_ (image_ids_def certificate other states);
    ghost_ (distinct_ints_def images);
    let u = () in
    match states with
    | [] -> u
    | state :: rest ->
      ghost_ (has_state_def candidate state);
      ghost_ (ids_member table state);
      ghost_ (image_not_member source candidate certificate other relation state rest);
      ghost_ (image_ids_distinct source candidate certificate other relation rest);
      u

  let rec (image_not_member_semantic @ total) :
      (source : machine) -> (candidate : machine) ->
      (certificate : reduction_certificate) -> (other : machine) ->
      (agreement : ((word : int list) ->
        {u : unit | run candidate word === run other word})) @ total ->
      (state : int) -> (states : int list) ->
      {u : unit | if check_reduction source candidate certificate &&
        has_state candidate state && not (has_letter state states) &&
        (let _, table = candidate in included states (state_ids table)) then
        not (has_letter (image_number certificate other state)
          (image_ids certificate other states)) else true}
        @ immutable contended =
    fun source candidate certificate other agreement state states ->
    let _, table = candidate in
    let candidate_states = state_ids table in
    let image = image_number certificate other state in
    let images = image_ids certificate other states in
    ghost_ (has_letter_def state states);
    ghost_ (included_def states candidate_states);
    ghost_ (image_ids_def certificate other states);
    ghost_ (has_letter_def image images);
    let u = () in
    match states with
    | [] -> u
    | head :: rest ->
      ghost_ (has_state_def candidate head);
      ghost_ (ids_member table head);
      ghost_ (image_numbers_distinct_semantic source candidate certificate other
        agreement state head);
      ghost_ (image_not_member_semantic source candidate certificate other agreement
        state rest);
      u

  let rec (image_ids_distinct_semantic @ total) :
      (source : machine) -> (candidate : machine) ->
      (certificate : reduction_certificate) -> (other : machine) ->
      (agreement : ((word : int list) ->
        {u : unit | run candidate word === run other word})) @ total ->
      (states : int list) ->
      {u : unit | if check_reduction source candidate certificate &&
        distinct_ints states &&
        (let _, table = candidate in included states (state_ids table)) then
        distinct_ints (image_ids certificate other states) else true}
        @ immutable contended =
    fun source candidate certificate other agreement states ->
    let images = image_ids certificate other states in
    let _, table = candidate in
    let candidate_states = state_ids table in
    ghost_ (distinct_ints_def states);
    ghost_ (included_def states candidate_states);
    ghost_ (image_ids_def certificate other states);
    ghost_ (distinct_ints_def images);
    let u = () in
    match states with
    | [] -> u
    | state :: rest ->
      ghost_ (has_state_def candidate state);
      ghost_ (ids_member table state);
      ghost_ (image_not_member_semantic source candidate certificate other agreement
        state rest);
      ghost_ (image_ids_distinct_semantic source candidate certificate other agreement
        rest);
      u

  let (minimum_count_semantic @ total) (source : machine)
      (candidate : machine) (certificate : reduction_certificate)
      (other : machine)
      (agreement : ((word : int list) ->
        {u : unit | run candidate word === run other word}) @ total) :
      {u : unit | if check_reduction source candidate certificate &&
        valid other then
        Bigint.compare (state_size candidate) (state_size other) <= 0
        else true} =
    let _, candidate_table = candidate in
    let _, other_table = other in
    let candidate_states = state_ids candidate_table in
    let other_states = state_ids other_table in
    let images = image_ids certificate other candidate_states in
    ghost_ (check_reduction_def source candidate certificate);
    ghost_ (state_ids_unique candidate_table);
    ghost_ (self_included candidate_states);
    ghost_ (image_ids_length certificate other candidate_states);
    ghost_ (image_ids_included source candidate certificate other candidate_states);
    ghost_ (image_ids_distinct_semantic source candidate certificate other agreement
      candidate_states);
    ghost_ (included_bound images other_states);
    ghost_ (state_size_def candidate);
    ghost_ (state_size_def other);
    let u = () in u

  let (minimum_count_source_semantic @ total) (source : machine)
      (candidate : machine) (certificate : reduction_certificate)
      (other : machine)
      (agreement : ((word : int list) ->
        {u : unit | run source word === run other word}) @ total) :
      {u : unit | if check_reduction source candidate certificate &&
        valid other then
        Bigint.compare (state_size candidate) (state_size other) <= 0
        else true} =
    if check_reduction source candidate certificate then begin
      let (candidate_agreement @ total) (word : int list) :
          {u : unit | run candidate word === run other word} =
        ghost_ (reduction_preserves source candidate certificate word);
        agreement word;
        let u = () in u in
      ghost_ (minimum_count_semantic source candidate certificate other
        candidate_agreement);
      let u = () in u
    end else let u = () in u

  let (minimum_count @ total) (source : machine) (candidate : machine)
      (certificate : reduction_certificate) (other : machine)
      (relation : relation) :
      {u : unit | if check_reduction source candidate certificate &&
        check candidate other relation && valid other then
        Bigint.compare (state_size candidate) (state_size other) <= 0
        else true} =
    let _, candidate_table = candidate in
    let _, other_table = other in
    let candidate_states = state_ids candidate_table in
    let other_states = state_ids other_table in
    let images = image_ids certificate other candidate_states in
    ghost_ (check_reduction_def source candidate certificate);
    ghost_ (state_ids_unique candidate_table);
    ghost_ (self_included candidate_states);
    ghost_ (image_ids_length certificate other candidate_states);
    ghost_ (image_ids_included source candidate certificate other candidate_states);
    ghost_ (image_ids_distinct source candidate certificate other relation
      candidate_states);
    ghost_ (included_bound images other_states);
    ghost_ (state_size_def candidate);
    ghost_ (state_size_def other);
    let u = () in u

  let rec (list_size_properties @ total) :
      (xs : int list) ->
      {u : unit | 0 <= list_size xs && list_size xs <= 129 &&
        Bigint.compare (Bigint.of_int (list_size xs)) (big_length xs) <= 0 &&
        (if list_size xs < 129 then
          Bigint.of_int (list_size xs) === big_length xs else true)}
        @ immutable contended =
    fun xs ->
    ghost_ (list_size_def xs);
    ghost_ (big_length_def xs);
    let u = () in
    match xs with
    | [] -> u
    | _ :: rest ->
      ghost_ (list_size_properties rest);
      u

  let rec (append_big_length @ total) :
      (left : int list) -> (right : int list) ->
      {u : unit | big_length (append left right) ===
        Bigint.add (big_length left) (big_length right)}
        @ immutable contended =
    fun left right ->
    ghost_ (append_def left right);
    ghost_ (big_length_def left);
    let joined = append left right in
    let u = () in
    match left with
    | [] -> u
    | _ :: rest ->
      ghost_ (big_length_def joined);
      ghost_ (append_big_length rest right);
      u

  let (pair_labels_size @ total) (left : int list) (right : int list) :
      {u : unit | if list_size left <= 64 && list_size right <= 64 then
        list_size (append left right) <= 128 else true} =
    let joined = append left right in
    ghost_ (list_size_properties left);
    ghost_ (list_size_properties right);
    ghost_ (list_size_properties joined);
    ghost_ (append_big_length left right);
    let u = () in u

  let rec (bounded_labels_member @ total) :
      (machine : machine) -> (states : int list) -> (state : int) ->
      {u : unit | if bounded_labels_from machine states && has_letter state states then
        list_size (labels machine state) <= 64 else true}
        @ immutable contended =
    fun machine states state ->
    ghost_ (bounded_labels_from_def machine states);
    ghost_ (has_letter_def state states);
    let u = () in
    match states with
    | [] -> u
    | _ :: rest -> bounded_labels_member machine rest state; u

  let rec (bounded_labels_rebased @ total) :
      (source : machine) -> (initial : int) -> (states : int list) ->
      {u : unit | let _, table = source in
        bounded_labels_from (initial, table) states ===
          bounded_labels_from source states} @ immutable contended =
    fun source initial states ->
    let _, table = source in
    let rebased = initial, table in
    ghost_ (bounded_labels_from_def source states);
    ghost_ (bounded_labels_from_def rebased states);
    let u = () in
    match states with
    | [] -> u
    | state :: rest ->
      ghost_ (labels_def source state);
      ghost_ (labels_def rebased state);
      ghost_ (bounded_labels_rebased source initial rest);
      u

  let (labels_bounded_state @ total) (machine : machine) (state : int) :
      {u : unit | if labels_bounded machine && has_state machine state then
        list_size (labels machine state) <= 64 else true} =
    let _, table = machine in
    let states = state_ids table in
    ghost_ (labels_bounded_def machine);
    ghost_ (has_state_def machine state);
    ghost_ (ids_member table state);
    ghost_ (bounded_labels_member machine states state);
    let u = () in u

  let[@def] rec pair_member pair pairs =
    match pairs with
    | [] -> false
    | head :: rest -> same_pair pair head || pair_member pair rest

  let rec (pair_member_agrees @ total) :
      (pair : state_pair) -> (pairs : relation) ->
      {u : unit | pair_member pair pairs === related pair pairs}
        @ immutable contended =
    fun pair pairs ->
    ghost_ (pair_member_def pair pairs);
    ghost_ (related_def pair pairs);
    let u = () in
    match pairs with
    | [] -> u
    | _ :: rest ->
      ghost_ (pair_member_agrees pair rest);
      u

  let (related_cons @ total) (pair : int * int) (head : int * int)
      (rest : relation) :
      {u : unit | related pair (head :: rest) ===
        (pair === head || related pair rest)} =
    let extended = head :: rest in
    ghost_ (related_def pair extended);
    ghost_ (same_pair_correct pair head);
    let u = () in u

  let (related_weaken @ total) (pair : int * int) (head : int * int)
      (rest : relation) :
      {u : unit | if related pair rest then
        related pair (head :: rest) else true} =
    ghost_ (related_cons pair head rest);
    let u = () in u

  let[@def] rec relation_included before after =
    match before with
    | [] -> true
    | head :: rest -> related head after && relation_included rest after

  let rec (relation_included_weaken @ total) :
      (before : relation) -> (after : relation) ->
      (head : state_pair) ->
      {u : unit | if relation_included before after then
        relation_included before (head :: after) else true}
        @ immutable contended =
    fun before after head ->
    ghost_ (relation_included_def before after);
    let extended = head :: after in
    ghost_ (relation_included_def before extended);
    let u = () in
    match before with
    | [] -> u
    | pair :: rest ->
      ghost_ (related_weaken pair head after);
      ghost_ (relation_included_weaken rest after head);
      u

  let rec (relation_included_self @ total) :
      (relation : relation) ->
      {u : unit | relation_included relation relation}
        @ immutable contended =
    fun relation ->
    ghost_ (relation_included_def relation relation);
    let u = () in
    match relation with
    | [] -> u
    | head :: rest ->
      ghost_ (relation_included_self rest);
      ghost_ (relation_included_weaken rest rest head);
      ghost_ (related_cons head head rest);
      u

  let[@def] rec distinct_pairs pairs =
    match pairs with
    | [] -> true
    | pair :: rest -> not (related pair rest) && distinct_pairs rest

  let[@def] rec remove_pair pair pairs =
    match pairs with
    | [] -> []
    | head :: rest ->
      if same_pair pair head then rest else head :: remove_pair pair rest

  let rec (remove_pair_preserves_other @ total) :
      (pair : state_pair) -> (other : state_pair) -> (pairs : relation) ->
      {u : unit | if not (same_pair pair other) && related other pairs then
        related other (remove_pair pair pairs) else true}
        @ immutable contended =
    fun pair other pairs ->
    ghost_ (related_def other pairs);
    ghost_ (remove_pair_def pair pairs);
    ghost_ (same_pair_correct pair other);
    let u = () in
    match pairs with
    | [] -> u
    | head :: rest ->
      let filtered = remove_pair pair pairs in
      ghost_ (related_def other filtered);
      ghost_ (same_pair_correct pair head);
      ghost_ (same_pair_correct other head);
      ghost_ (remove_pair_preserves_other pair other rest);
      u

  let rec (remove_pair_size @ total) :
      (pair : state_pair) -> (pairs : relation) ->
      {u : unit | if related pair pairs then
        big_length pairs === Bigint.add 1Z (big_length (remove_pair pair pairs))
        else true} @ immutable contended =
    fun pair pairs ->
    ghost_ (related_def pair pairs);
    ghost_ (remove_pair_def pair pairs);
    ghost_ (big_length_def pairs);
    let u = () in
    match pairs with
    | [] -> u
    | _ :: rest ->
      let filtered = remove_pair pair pairs in
      let filtered_rest = remove_pair pair rest in
      ghost_ (big_length_def filtered);
      ghost_ (big_length_def rest);
      ghost_ (big_length_def filtered_rest);
      ghost_ (remove_pair_size pair rest);
      u

  let rec (relation_included_remove @ total) :
      (pair : state_pair) -> (before : relation) -> (after : relation) ->
      {u : unit | if not (related pair before) &&
        relation_included before after then
        relation_included before (remove_pair pair after) else true}
        @ immutable contended =
    fun pair before after ->
    ghost_ (related_def pair before);
    ghost_ (relation_included_def before after);
    let filtered = remove_pair pair after in
    ghost_ (relation_included_def before filtered);
    let u = () in
    match before with
    | [] -> u
    | head :: rest ->
      ghost_ (remove_pair_preserves_other pair head after);
      ghost_ (relation_included_remove pair rest after);
      u

  let rec (relation_length_nonnegative @ total) :
      (pairs : relation) ->
      {u : unit | Bigint.compare 0Z (big_length pairs) <= 0}
        @ immutable contended =
    fun pairs ->
    ghost_ (big_length_def pairs);
    let u = () in
    match pairs with
    | [] -> u
    | _ :: rest -> relation_length_nonnegative rest; u

  let rec (relation_included_bound @ total) :
      (before : relation) -> (after : relation) ->
      {u : unit | if distinct_pairs before && relation_included before after then
        Bigint.compare (big_length before) (big_length after) <= 0 else true}
        @ immutable contended =
    fun before after ->
    ghost_ (distinct_pairs_def before);
    ghost_ (relation_included_def before after);
    ghost_ (big_length_def before);
    let u = () in
    match before with
    | [] -> relation_length_nonnegative after; u
    | pair :: rest ->
      let filtered = remove_pair pair after in
      ghost_ (relation_included_remove pair rest after);
      ghost_ (remove_pair_size pair after);
      ghost_ (relation_included_bound rest filtered);
      u

  let[@def] rec product_row state others tail =
    match others with
    | [] -> tail
    | other :: rest -> (state, other) :: product_row state rest tail

  let[@def] rec state_product left right =
    match left with
    | [] -> []
    | state :: rest -> product_row state right (state_product rest right)

  let rec (product_row_length @ total) :
      (state : int) -> (others : int list) -> (tail : relation) ->
      {u : unit | big_length (product_row state others tail) ===
        Bigint.add (big_length others) (big_length tail)}
        @ immutable contended =
    fun state others tail ->
    ghost_ (product_row_def state others tail);
    ghost_ (big_length_def others);
    let pairs = product_row state others tail in
    let u = () in
    match others with
    | [] -> u
    | _ :: rest ->
      ghost_ (big_length_def pairs);
      ghost_ (product_row_length state rest tail);
      u

  let rec (state_product_length @ total) :
      (left : int list) -> (right : int list) ->
      {u : unit | big_length (state_product left right) ===
        Bigint.mul (big_length left) (big_length right)}
        @ immutable contended =
    fun left right ->
    ghost_ (state_product_def left right);
    ghost_ (big_length_def left);
    let pairs = state_product left right in
    let u = () in
    match left with
    | [] -> big_length_def pairs; u
    | state :: rest ->
      let tail = state_product rest right in
      ghost_ (product_row_length state right tail);
      ghost_ (state_product_length rest right);
      u

  let rec (product_row_member @ total) :
      (state : int) -> (others : int list) -> (tail : relation) ->
      (pair : state_pair) ->
      {u : unit | let p, q = pair in
        related pair (product_row state others tail) ===
          ((p = state && has_letter q others) || related pair tail)}
        @ immutable contended =
    fun state others tail pair ->
    let _, q = pair in
    ghost_ (product_row_def state others tail);
    ghost_ (has_letter_def q others);
    let u = () in
    match others with
    | [] -> u
    | other :: rest ->
      let head = state, other in
      let pairs = product_row state others tail in
      ghost_ (related_def pair pairs);
      ghost_ (same_pair_correct pair head);
      ghost_ (product_row_member state rest tail pair);
      u

  let rec (state_product_member @ total) :
      (left : int list) -> (right : int list) -> (pair : state_pair) ->
      {u : unit | let p, q = pair in
        related pair (state_product left right) ===
          (has_letter p left && has_letter q right)}
        @ immutable contended =
    fun left right pair ->
    let p, _ = pair in
    ghost_ (state_product_def left right);
    ghost_ (has_letter_def p left);
    let u = () in
    match left with
    | [] ->
      let pairs = state_product left right in
      ghost_ (related_def pair pairs);
      u
    | state :: rest ->
      let tail = state_product rest right in
      ghost_ (product_row_member state right tail pair);
      ghost_ (state_product_member rest right pair);
      u

  let[@def] rec pairs_valid left right pairs =
    match pairs with
    | [] -> true
    | (p, q) :: rest ->
      has_state left p && has_state right q && pairs_valid left right rest

  let rec (pairs_valid_included @ total) :
      (left : machine) -> (right : machine) -> (pairs : relation) ->
      {u : unit | let _, left_table = left in let _, right_table = right in
        if pairs_valid left right pairs then
          relation_included pairs
            (state_product (state_ids left_table) (state_ids right_table))
        else true} @ immutable contended =
    fun left right pairs ->
    let _, left_table = left in
    let _, right_table = right in
    let left_states = state_ids left_table in
    let right_states = state_ids right_table in
    let domain = state_product left_states right_states in
    ghost_ (pairs_valid_def left right pairs);
    ghost_ (relation_included_def pairs domain);
    let u = () in
    match pairs with
    | [] -> u
    | (p, q) :: rest ->
      let pair = p, q in
      ghost_ (has_state_def left p);
      ghost_ (has_state_def right q);
      ghost_ (ids_member left_table p);
      ghost_ (ids_member right_table q);
      ghost_ (state_product_member left_states right_states pair);
      ghost_ (pairs_valid_included left right rest);
      u

  let (product_count_bound @ total) (left : machine) (right : machine)
      (pairs : relation) :
      {u : unit | if pairs_valid left right pairs && distinct_pairs pairs then
        Bigint.compare (big_length pairs)
          (Bigint.mul (state_size left) (state_size right)) <= 0 else true} =
    let _, left_table = left in
    let _, right_table = right in
    let left_states = state_ids left_table in
    let right_states = state_ids right_table in
    let domain = state_product left_states right_states in
    ghost_ (pairs_valid_included left right pairs);
    ghost_ (relation_included_bound pairs domain);
    ghost_ (state_product_length left_states right_states);
    ghost_ (state_size_def left);
    ghost_ (state_size_def right);
    let u = () in u

  let rec (related_included @ total) :
      (pair : state_pair) -> (before : relation) ->
      (after : relation) ->
      {u : unit | if relation_included before after &&
        related pair before then related pair after else true}
        @ immutable contended =
    fun pair before after ->
    ghost_ (relation_included_def before after);
    ghost_ (related_def pair before);
    let u = () in
    match before with
    | [] -> u
    | head :: rest ->
      ghost_ (same_pair_correct pair head);
      ghost_ (related_included pair rest after);
      u

  let rec (relation_included_trans @ total) :
      (first : relation) -> (second : relation) ->
      (third : relation) ->
      {u : unit | if relation_included first second &&
        relation_included second third then
        relation_included first third else true}
        @ immutable contended =
    fun first second third ->
    ghost_ (relation_included_def first second);
    ghost_ (relation_included_def first third);
    let u = () in
    match first with
    | [] -> u
    | head :: rest ->
      ghost_ (related_included head second third);
      ghost_ (relation_included_trans rest second third);
      u

  let rec (labelled_closed_weaken @ total) :
      (left : machine) -> (right : machine) ->
      (p : int) -> (q : int) -> (letters : int list) ->
      (before : relation) -> (after : relation) ->
      {u : unit | if relation_included before after &&
        labelled_closed left right p q letters before then
        labelled_closed left right p q letters after else true}
        @ immutable contended =
    fun left right p q letters before after ->
    ghost_ (labelled_closed_def left right p q letters before);
    ghost_ (labelled_closed_def left right p q letters after);
    let u = () in
    match letters with
    | [] -> u
    | letter :: rest ->
      let pair = step left p letter, step right q letter in
      ghost_ (related_included pair before after);
      ghost_ (labelled_closed_weaken left right p q rest before after);
      u

  let (pair_closed_weaken @ total) (left : machine) (right : machine)
      (p : int) (q : int) (before : relation) (after : relation) :
      {u : unit | if relation_included before after &&
        pair_closed left right p q before then
        pair_closed left right p q after else true} =
    let default_pair = default left p, default right q in
    let letters = append (labels left p) (labels right q) in
    ghost_ (pair_closed_def left right p q before);
    ghost_ (pair_closed_def left right p q after);
    ghost_ (related_included default_pair before after);
    ghost_ (labelled_closed_weaken left right p q letters before after);
    let u = () in u

  let rec (all_closed_weaken @ total) :
      (left : machine) -> (right : machine) ->
      (before : relation) -> (after : relation) ->
      (pairs : relation) ->
      {u : unit | if relation_included before after &&
        all_closed left right before pairs then
        all_closed left right after pairs else true}
        @ immutable contended =
    fun left right before after pairs ->
    ghost_ (all_closed_def left right before pairs);
    ghost_ (all_closed_def left right after pairs);
    let u = () in
    match pairs with
    | [] -> u
    | (p, q) :: rest ->
      ghost_ (pair_closed_weaken left right p q before after);
      ghost_ (all_closed_weaken left right before after rest);
      u

  let[@def] rec pending_valid left right pending =
    match pending with
    | [] -> true
    | (p, q, word) :: rest ->
      reached left word = p && reached right word = q &&
      pending_valid left right rest

  let[@def] rec pending_member (pair : state_pair)
      (pending : (int * int * int list) list) =
    match pending with
    | [] -> false
    | (p, q, _) :: rest ->
      same_pair pair (p, q) || pending_member pair rest

  let[@def] rec all_seen_accounted seen processed pending =
    match seen with
    | [] -> true
    | pair :: rest ->
      (related pair processed || pending_member pair pending) &&
      all_seen_accounted rest processed pending

  let (pending_member_cons @ total) (pair : state_pair)
      (p : int) (q : int) (word : int list)
      (rest : (int * int * int list) list) :
      {u : unit | pending_member pair ((p, q, word) :: rest) ===
        (same_pair pair (p, q) || pending_member pair rest)} =
    let extended = (p, q, word) :: rest in
    ghost_ (pending_member_def pair extended);
    let u = () in u

  let (pending_member_empty @ total) (pair : state_pair) :
      {u : unit | pending_member pair [] === false} =
    let nil = [] in
    ghost_ (pending_member_def pair nil);
    let u = () in u

  let rec (accounted_pending_weaken @ total) :
      (seen : relation) -> (processed : relation) ->
      (pending : (int * int * int list) list) ->
      (p : int) -> (q : int) -> (word : int list) ->
      {u : unit | if all_seen_accounted seen processed pending then
        all_seen_accounted seen processed ((p, q, word) :: pending)
        else true} @ immutable contended =
    fun seen processed pending p q word ->
    ghost_ (all_seen_accounted_def seen processed pending);
    let extended = (p, q, word) :: pending in
    ghost_ (all_seen_accounted_def seen processed extended);
    let u = () in
    match seen with
    | [] -> u
    | pair :: rest ->
      ghost_ (pending_member_cons pair p q word pending);
      ghost_ (accounted_pending_weaken rest processed pending p q word);
      u

  let rec (accounted_pop @ total) :
      (seen : relation) -> (processed : relation) ->
      (p : int) -> (q : int) -> (word : int list) ->
      (rest : (int * int * int list) list) ->
      {u : unit | if all_seen_accounted seen processed
          ((p, q, word) :: rest) then
        all_seen_accounted seen ((p, q) :: processed) rest
        else true} @ immutable contended =
    fun seen processed p q word rest ->
    let pair = p, q in
    let pending = (p, q, word) :: rest in
    let updated_processed = pair :: processed in
    ghost_ (all_seen_accounted_def seen processed pending);
    ghost_ (all_seen_accounted_def seen updated_processed rest);
    let u = () in
    match seen with
    | [] -> u
    | current :: tail ->
      ghost_ (pending_member_cons current p q word rest);
      ghost_ (related_cons current pair processed);
      ghost_ (same_pair_correct current pair);
      ghost_ (accounted_pop tail processed p q word rest);
      u

  let rec (accounted_empty_included @ total) :
      (seen : relation) -> (processed : relation) ->
      {u : unit | if all_seen_accounted seen processed [] then
        relation_included seen processed else true}
        @ immutable contended =
    fun seen processed ->
    let nil = [] in
    ghost_ (all_seen_accounted_def seen processed nil);
    ghost_ (relation_included_def seen processed);
    let u = () in
    if all_seen_accounted seen processed nil then
      match seen with
      | [] -> u
      | pair :: rest ->
        ghost_ (pending_member_empty pair);
        ghost_ (all_seen_accounted_def rest processed nil);
        ghost_ (relation_included_def rest processed);
        ghost_ (accounted_empty_included rest processed);
        u
    else u

  let rec (closed_processed_covers_seen @ total) :
      (left : machine) -> (right : machine) ->
      (seen : relation) -> (processed : relation) ->
      (remaining : relation) ->
      {u : unit | if all_closed left right seen processed &&
        relation_included remaining processed then
        all_closed left right seen remaining else true}
        @ immutable contended =
    fun left right seen processed remaining ->
    ghost_ (all_closed_def left right seen remaining);
    ghost_ (relation_included_def remaining processed);
    let u = () in
    match remaining with
    | [] -> u
    | (p, q) :: rest ->
      ghost_ (all_closed_member left right seen processed p q);
      ghost_ (closed_processed_covers_seen left right seen processed rest);
      u

  let (pending_singleton @ total) (left : machine) (right : machine)
      (p : int) (q : int) (word : int list) :
      {u : unit | if reached left word = p && reached right word = q then
        pending_valid left right [p, q, word] else true} =
    let empty = [] in
    let singleton = [p, q, word] in
    ghost_ (pending_valid_def left right empty);
    ghost_ (pending_valid_def left right singleton);
    let u = () in u

  let (reached_push @ total) (machine : machine) (word : int list)
      (letter : int) :
      {u : unit | reached machine (append word [letter]) ===
        step machine (reached machine word) letter} =
    let initial, _ = machine in
    let suffix = [letter] in
    let joined = append word suffix in
    let state = drive machine initial word in
    let next = step machine state letter in
    let nil = [] in
    ghost_ (append_def word suffix);
    ghost_ (reached_def machine word);
    ghost_ (reached_def machine joined);
    ghost_ (drive_after machine initial word suffix);
    ghost_ (drive_def machine state suffix);
    ghost_ (drive_def machine next nil);
    let u = () in u

  let (reached_empty @ total) (machine : machine) :
      {u : unit | let initial, _ = machine in reached machine [] === initial} =
    let initial, _ = machine in
    let nil = [] in
    ghost_ (reached_def machine nil);
    ghost_ (drive_def machine initial nil);
    let u = () in u

  let (reached_empty_equal @ total) (machine : machine) :
      {u : unit | let initial, _ = machine in reached machine [] = initial} =
    let initial, _ = machine in
    let nil = [] in
    ghost_ (reached_def machine nil);
    ghost_ (drive_def machine initial nil);
    let u = () in u

  let (initial_pending_valid @ total) (left : machine) (right : machine) :
      {u : unit | let left_initial, _ = left in
        let right_initial, _ = right in
        pending_valid left right [left_initial, right_initial, []]} =
    let left_initial, _ = left in
    let right_initial, _ = right in
    let nil = [] in
    let pending = [left_initial, right_initial, nil] in
    ghost_ (reached_empty_equal left);
    ghost_ (reached_empty_equal right);
    ghost_ (pending_singleton left right left_initial right_initial nil);
    ghost_ (reached_def left nil);
    ghost_ (reached_def right nil);
    ghost_ (drive_def left left_initial nil);
    ghost_ (drive_def right right_initial nil);
    ghost_ (pending_singleton left right left_initial right_initial nil);
    ghost_ (reached_def left nil);
    ghost_ (reached_def right nil);
    ghost_ (drive_def left left_initial nil);
    ghost_ (drive_def right right_initial nil);
    ghost_ (pending_valid_def left right nil);
    ghost_ (pending_valid_def left right pending);
    let u = () in u

  let (initial_pending_valid_at @ total) (left : machine)
      (right : machine) (pending : (int * int * int list) list) :
      {u : unit | let left_initial, _ = left in
        let right_initial, _ = right in
        if pending === [left_initial, right_initial, []] then
          pending_valid left right pending else true} =
    ghost_ (initial_pending_valid left right);
    let u = () in u

  let[@def] push_pair (limit : int) (pair : int * int)
      (word : int list) (pending : (int * int * int list) list)
      (seen : relation) (count : int) =
    if pair_member pair seen then Some (pending, seen, count)
    else if count >= limit then None
    else
      let p, q = pair in
      Some ((p, q, word) :: pending, pair :: seen, count + 1)

  let rec (product_pending_length_nonnegative @ total) :
      (pending : (int * int * int list) list) ->
      {u : unit | Bigint.compare 0Z (big_length pending) <= 0}
        @ immutable contended =
    fun pending ->
    ghost_ (big_length_def pending);
    let u = () in
    match pending with
    | [] -> u
    | _ :: rest ->
      ghost_ (product_pending_length_nonnegative rest);
      u

  let (push_pair_counts @ total) (limit : int) (pair : state_pair)
      (word : int list) (pending : (int * int * int list) list)
      (seen : relation) (count : int) :
      {u : unit | if 0 <= count && count <= limit && limit <= 65_536 &&
        Bigint.of_int count === big_length seen then
        match push_pair limit pair word pending seen count with
        | None -> count = limit && not (pair_member pair seen)
        | Some (new_pending, new_seen, new_count) ->
          0 <= new_count && new_count <= limit &&
          Bigint.of_int new_count === big_length new_seen &&
          Bigint.add (Bigint.of_int new_count) (big_length pending) ===
            Bigint.add (Bigint.of_int count) (big_length new_pending)
        else true} =
    ghost_ (push_pair_def limit pair word pending seen count);
    let p, q = pair in
    let new_pending = (p, q, word) :: pending in
    let new_seen = pair :: seen in
    ghost_ (big_length_def new_pending);
    ghost_ (big_length_def new_seen);
    let u = () in u

  let (push_pair_distinct @ total) (limit : int) (pair : state_pair)
      (word : int list) (pending : (int * int * int list) list)
      (seen : relation) (count : int) :
      {u : unit | if distinct_pairs seen then
        match push_pair limit pair word pending seen count with
        | None -> true
        | Some (_, new_seen, _) -> distinct_pairs new_seen
        else true} =
    ghost_ (push_pair_def limit pair word pending seen count);
    ghost_ (pair_member_agrees pair seen);
    let extended = pair :: seen in
    ghost_ (distinct_pairs_def extended);
    let u = () in u

  let (push_pair_domain @ total) (left : machine) (right : machine)
      (limit : int) (pair : state_pair) (word : int list)
      (pending : (int * int * int list) list) (seen : relation) (count : int) :
      {u : unit | let p, q = pair in
        if pairs_valid left right seen && has_state left p && has_state right q then
        match push_pair limit pair word pending seen count with
        | None -> true
        | Some (_, new_seen, _) -> pairs_valid left right new_seen
        else true} =
    ghost_ (push_pair_def limit pair word pending seen count);
    let extended = pair :: seen in
    ghost_ (pairs_valid_def left right extended);
    let u = () in u

  let (product_fresh_bound @ total) (left : machine) (right : machine)
      (seen : relation) (pair : state_pair) :
      {u : unit | let p, q = pair in
        if pairs_valid left right seen && distinct_pairs seen &&
          not (pair_member pair seen) && has_state left p && has_state right q then
        Bigint.compare (Bigint.add 1Z (big_length seen))
          (Bigint.mul (state_size left) (state_size right)) <= 0 else true} =
    let extended = pair :: seen in
    ghost_ (pair_member_agrees pair seen);
    ghost_ (pairs_valid_def left right extended);
    ghost_ (distinct_pairs_def extended);
    ghost_ (product_count_bound left right extended);
    ghost_ (big_length_def extended);
    let u = () in u

  let (push_pair_capacity @ total) (left : machine) (right : machine)
      (limit : int) (pair : state_pair) (word : int list)
      (pending : (int * int * int list) list) (seen : relation) (count : int) :
      {u : unit | let p, q = pair in
        if pairs_valid left right seen && distinct_pairs seen &&
          has_state left p && has_state right q &&
          0 <= count && count <= limit && limit <= 65_536 &&
          Bigint.of_int count === big_length seen &&
          Bigint.compare (Bigint.mul (state_size left) (state_size right))
            (Bigint.of_int limit) <= 0 then
        match push_pair limit pair word pending seen count with
        | None -> false
        | Some _ -> true
        else true} =
    ghost_ (push_pair_counts limit pair word pending seen count);
    ghost_ (product_fresh_bound left right seen pair);
    let u = () in u

  let (push_pair_seen_included @ total) (limit : int)
      (pair : int * int) (word : int list)
      (pending : (int * int * int list) list)
      (seen : relation) (count : int) :
      {u : unit | match push_pair limit pair word pending seen count with
        | None -> true
        | Some (_, updated_seen, _) ->
          relation_included seen updated_seen} =
    ghost_ (push_pair_def limit pair word pending seen count);
    let u = () in
    if pair_member pair seen then begin
      ghost_ (relation_included_self seen);
      u
    end else if count >= limit then u
    else begin
      ghost_ (relation_included_self seen);
      ghost_ (relation_included_weaken seen seen pair);
      u
    end

  let (push_pair_related @ total) (limit : int)
      (pair : state_pair) (word : int list)
      (pending : (int * int * int list) list)
      (seen : relation) (count : int) :
      {u : unit | match push_pair limit pair word pending seen count with
        | None -> true
        | Some (_, updated_seen, _) -> related pair updated_seen} =
    ghost_ (push_pair_def limit pair word pending seen count);
    ghost_ (pair_member_agrees pair seen);
    let u = () in
    if pair_member pair seen then u
    else if count >= limit then u
    else begin
      ghost_ (related_cons pair pair seen);
      u
    end

  let (push_pair_accounted @ total) (limit : int)
      (pair : state_pair) (word : int list)
      (pending : (int * int * int list) list)
      (seen : relation) (processed : relation) (count : int) :
      {u : unit | if all_seen_accounted seen processed pending then
        match push_pair limit pair word pending seen count with
        | None -> true
        | Some (updated_pending, updated_seen, _) ->
          all_seen_accounted updated_seen processed updated_pending
        else true} =
    ghost_ (push_pair_def limit pair word pending seen count);
    let u = () in
    if pair_member pair seen then u
    else if count >= limit then u
    else begin
      let p, q = pair in
      let updated_pending = (p, q, word) :: pending in
      let updated_seen = pair :: seen in
      ghost_ (accounted_pending_weaken seen processed pending p q word);
      ghost_ (all_seen_accounted_def updated_seen processed updated_pending);
      ghost_ (pending_member_cons pair p q word pending);
      ghost_ (same_pair_correct pair pair);
      u
    end

  let (push_pair_valid @ total) (left : machine) (right : machine)
      (limit : int) (pair : int * int) (word : int list)
      (pending : (int * int * int list) list) (seen : relation)
      (count : int) :
      {u : unit | if pending_valid left right pending &&
        (let p, q = pair in reached left word = p && reached right word = q)
        then match push_pair limit pair word pending seen count with
          | None -> true
          | Some (pending, _, _) -> pending_valid left right pending
        else true} =
    ghost_ (push_pair_def limit pair word pending seen count);
    let p, q = pair in
    let new_pending = (p, q, word) :: pending in
    ghost_ (pending_valid_def left right new_pending);
    let u = () in u

  let[@def] rec (push_labels @ total) :
      (left : machine) -> (right : machine) ->
      (p : int) -> (q : int) -> (word : int list) ->
      (letters : int list) ->
      (pending : (int * int * int list) list) ->
      (seen : relation) -> (count : int) -> (limit : int) ->
      ((int * int * int list) list * relation * int) option
      @ immutable contended =
    fun left right p q word letters pending seen count limit ->
    match letters with
    | [] -> Some (pending, seen, count)
    | letter :: rest ->
      let pair = step left p letter, step right q letter in
      match push_pair limit pair (append word [letter]) pending seen count with
      | None -> None
      | Some (pending, seen, count) ->
        push_labels left right p q word rest pending seen count limit

  let rec (push_labels_counts @ total) :
      (left : machine) -> (right : machine) -> (p : int) -> (q : int) ->
      (word : int list) -> (letters : int list) ->
      (pending : (int * int * int list) list) -> (seen : relation) ->
      (count : int) -> (limit : int) ->
      {u : unit | if 0 <= count && count <= limit && limit <= 65_536 &&
        Bigint.of_int count === big_length seen then
        match push_labels left right p q word letters pending seen count limit with
        | None -> true
        | Some (new_pending, new_seen, new_count) ->
          0 <= new_count && new_count <= limit &&
          Bigint.of_int new_count === big_length new_seen &&
          Bigint.add (Bigint.of_int new_count) (big_length pending) ===
            Bigint.add (Bigint.of_int count) (big_length new_pending)
        else true} @ immutable contended =
    fun left right p q word letters pending seen count limit ->
    ghost_ (push_labels_def left right p q word letters pending seen count limit);
    let u = () in
    match letters with
    | [] -> u
    | letter :: rest ->
      let pair = step left p letter, step right q letter in
      let next_word = append word [letter] in
      ghost_ (push_pair_counts limit pair next_word pending seen count);
      (match push_pair limit pair next_word pending seen count with
       | None -> u
       | Some (new_pending, new_seen, new_count) ->
         ghost_ (push_labels_counts left right p q word rest
           new_pending new_seen new_count limit);
         u)

  let rec (push_labels_distinct @ total) :
      (left : machine) -> (right : machine) -> (p : int) -> (q : int) ->
      (word : int list) -> (letters : int list) ->
      (pending : (int * int * int list) list) -> (seen : relation) ->
      (count : int) -> (limit : int) ->
      {u : unit | if distinct_pairs seen then
        match push_labels left right p q word letters pending seen count limit with
        | None -> true
        | Some (_, new_seen, _) -> distinct_pairs new_seen
        else true} @ immutable contended =
    fun left right p q word letters pending seen count limit ->
    ghost_ (push_labels_def left right p q word letters pending seen count limit);
    let u = () in
    match letters with
    | [] -> u
    | letter :: rest ->
      let pair = step left p letter, step right q letter in
      let next_word = append word [letter] in
      ghost_ (push_pair_distinct limit pair next_word pending seen count);
      (match push_pair limit pair next_word pending seen count with
       | None -> u
       | Some (new_pending, new_seen, new_count) ->
         ghost_ (push_labels_distinct left right p q word rest
           new_pending new_seen new_count limit);
         u)

  let rec (push_labels_domain @ total) :
      (left : machine) -> (right : machine) -> (p : int) -> (q : int) ->
      (word : int list) -> (letters : int list) ->
      (pending : (int * int * int list) list) -> (seen : relation) ->
      (count : int) -> (limit : int) ->
      {u : unit | if valid left && valid right &&
        has_state left p && has_state right q && pairs_valid left right seen then
        match push_labels left right p q word letters pending seen count limit with
        | None -> true
        | Some (_, new_seen, _) -> pairs_valid left right new_seen
        else true} @ immutable contended =
    fun left right p q word letters pending seen count limit ->
    ghost_ (push_labels_def left right p q word letters pending seen count limit);
    let u = () in
    match letters with
    | [] -> u
    | letter :: rest ->
      let pair = step left p letter, step right q letter in
      let next_word = append word [letter] in
      ghost_ (step_valid left p letter);
      ghost_ (step_valid right q letter);
      ghost_ (push_pair_domain left right limit pair next_word pending seen count);
      (match push_pair limit pair next_word pending seen count with
       | None -> u
       | Some (new_pending, new_seen, new_count) ->
         ghost_ (push_labels_domain left right p q word rest
           new_pending new_seen new_count limit);
         u)

  let rec (push_labels_capacity @ total) :
      (left : machine) -> (right : machine) -> (p : int) -> (q : int) ->
      (word : int list) -> (letters : int list) ->
      (pending : (int * int * int list) list) -> (seen : relation) ->
      (count : int) -> (limit : int) ->
      {u : unit | if valid left && valid right &&
        has_state left p && has_state right q &&
        pairs_valid left right seen && distinct_pairs seen &&
        0 <= count && count <= limit && limit <= 65_536 &&
        Bigint.of_int count === big_length seen &&
        Bigint.compare (Bigint.mul (state_size left) (state_size right))
          (Bigint.of_int limit) <= 0 then
        match push_labels left right p q word letters pending seen count limit with
        | None -> false
        | Some _ -> true
        else true} @ immutable contended =
    fun left right p q word letters pending seen count limit ->
    ghost_ (push_labels_def left right p q word letters pending seen count limit);
    let u = () in
    match letters with
    | [] -> u
    | letter :: rest ->
      let pair = step left p letter, step right q letter in
      let next_word = append word [letter] in
      ghost_ (step_valid left p letter);
      ghost_ (step_valid right q letter);
      ghost_ (push_pair_capacity left right limit pair next_word pending seen count);
      ghost_ (push_pair_counts limit pair next_word pending seen count);
      ghost_ (push_pair_distinct limit pair next_word pending seen count);
      ghost_ (push_pair_domain left right limit pair next_word pending seen count);
      (match push_pair limit pair next_word pending seen count with
       | None -> u
       | Some (new_pending, new_seen, new_count) ->
         ghost_ (push_labels_capacity left right p q word rest
           new_pending new_seen new_count limit);
         u)

  let rec (push_labels_seen_included @ total) :
      (left : machine) -> (right : machine) ->
      (p : int) -> (q : int) -> (word : int list) ->
      (letters : int list) ->
      (pending : (int * int * int list) list) ->
      (seen : relation) -> (count : int) -> (limit : int) ->
      {u : unit | match push_labels left right p q word letters
          pending seen count limit with
        | None -> true
        | Some (_, updated_seen, _) ->
          relation_included seen updated_seen}
        @ immutable contended =
    fun left right p q word letters pending seen count limit ->
    ghost_ (push_labels_def left right p q word letters pending seen count limit);
    let u = () in
    match letters with
    | [] ->
      ghost_ (relation_included_self seen);
      u
    | letter :: rest ->
      let pair = step left p letter, step right q letter in
      let next_word = append word [letter] in
      ghost_ (push_pair_seen_included limit pair next_word pending seen count);
      (match push_pair limit pair next_word pending seen count with
       | None -> u
       | Some (updated_pending, updated_seen, updated_count) ->
         ghost_ (push_labels_seen_included left right p q word rest
           updated_pending updated_seen updated_count limit);
         (match push_labels left right p q word rest
             updated_pending updated_seen updated_count limit with
          | None -> u
          | Some (_, final_seen, _) ->
            ghost_ (relation_included_trans seen updated_seen final_seen);
            u))

  let rec (push_labels_accounted @ total) :
      (left : machine) -> (right : machine) ->
      (p : int) -> (q : int) -> (word : int list) ->
      (letters : int list) ->
      (pending : (int * int * int list) list) ->
      (seen : relation) -> (processed : relation) ->
      (count : int) -> (limit : int) ->
      {u : unit | if all_seen_accounted seen processed pending then
        match push_labels left right p q word letters
            pending seen count limit with
        | None -> true
        | Some (updated_pending, updated_seen, _) ->
          all_seen_accounted updated_seen processed updated_pending
        else true} @ immutable contended =
    fun left right p q word letters pending seen processed count limit ->
    ghost_ (push_labels_def left right p q word letters pending seen count limit);
    let u = () in
    match letters with
    | [] -> u
    | letter :: rest ->
      let pair = step left p letter, step right q letter in
      let next_word = append word [letter] in
      ghost_ (push_pair_accounted limit pair next_word
        pending seen processed count);
      (match push_pair limit pair next_word pending seen count with
       | None -> u
       | Some (updated_pending, updated_seen, updated_count) ->
         ghost_ (push_labels_accounted left right p q word rest
           updated_pending updated_seen processed updated_count limit);
         u)

  let rec (push_labels_closed @ total) :
      (left : machine) -> (right : machine) ->
      (p : int) -> (q : int) -> (word : int list) ->
      (letters : int list) ->
      (pending : (int * int * int list) list) ->
      (seen : relation) -> (count : int) -> (limit : int) ->
      {u : unit | match push_labels left right p q word letters
          pending seen count limit with
        | None -> true
        | Some (_, updated_seen, _) ->
          labelled_closed left right p q letters updated_seen}
        @ immutable contended =
    fun left right p q word letters pending seen count limit ->
    ghost_ (push_labels_def left right p q word letters pending seen count limit);
    let u = () in
    match letters with
    | [] ->
      ghost_ (labelled_closed_def left right p q letters seen);
      u
    | letter :: rest ->
      let pair = step left p letter, step right q letter in
      let next_word = append word [letter] in
      ghost_ (push_pair_related limit pair next_word pending seen count);
      (match push_pair limit pair next_word pending seen count with
       | None -> u
       | Some (updated_pending, updated_seen, updated_count) ->
         ghost_ (push_labels_seen_included left right p q word rest
           updated_pending updated_seen updated_count limit);
         ghost_ (push_labels_closed left right p q word rest
           updated_pending updated_seen updated_count limit);
         (match push_labels left right p q word rest
             updated_pending updated_seen updated_count limit with
          | None -> u
          | Some (_, final_seen, _) ->
            ghost_ (related_included pair updated_seen final_seen);
            ghost_ (labelled_closed_def left right p q letters final_seen);
            u))

  let rec (push_labels_valid @ total) :
      (left : machine) -> (right : machine) ->
      (p : int) -> (q : int) -> (word : int list) ->
      (letters : int list) ->
      (pending : (int * int * int list) list) ->
      (seen : relation) -> (count : int) -> (limit : int) ->
      {u : unit | if pending_valid left right pending &&
        reached left word = p && reached right word = q then
        match push_labels left right p q word letters pending seen count limit
        with
        | None -> true
        | Some (pending, _, _) -> pending_valid left right pending
        else true} @ immutable contended =
    fun left right p q word letters pending seen count limit ->
    ghost_ (push_labels_def left right p q word letters pending seen count limit);
    let u = () in
    match letters with
    | [] -> u
    | letter :: rest ->
      let pair = step left p letter, step right q letter in
      let next_word = append word [letter] in
      ghost_ (reached_push left word letter);
      ghost_ (reached_push right word letter);
      ghost_ (push_pair_valid left right limit pair next_word pending seen count);
      (match push_pair limit pair next_word pending seen count with
       | None -> u
       | Some (pending, seen, count) ->
         ghost_ (push_labels_valid left right p q word rest pending seen count limit);
         u)

  let[@def] rec (symbol_range @ total) :
      (start : int) -> (remaining : int) -> int list @ immutable contended =
    fun start remaining ->
    if remaining <= 0 then []
    else start :: symbol_range (start + 1) (remaining - 1)
  [@@decreases remaining]

  let rec (symbol_range_member @ total) :
      (start : int) -> (remaining : int) -> (symbol : int) ->
      {u : unit | if 0 <= start && start <= 129 &&
        0 <= remaining && remaining <= 129 && start + remaining <= 129 &&
        has_letter symbol (symbol_range start remaining) then
        start <= symbol && symbol < start + remaining else true}
        @ immutable contended =
    fun start remaining symbol ->
    ghost_ (symbol_range_def start remaining);
    let symbols = symbol_range start remaining in
    ghost_ (has_letter_def symbol symbols);
    let u = () in
    if remaining <= 0 then u
    else begin
      let next_start = start + 1 in
      let next_remaining = remaining - 1 in
      ghost_ (symbol_range_member next_start next_remaining symbol);
      u
    end
  [@@decreases remaining]

  let rec (symbol_range_properties @ total) :
      (start : int) -> (remaining : int) ->
      {u : unit | if 0 <= start && start <= 129 &&
        0 <= remaining && remaining <= 129 && start + remaining <= 129 then
        distinct_ints (symbol_range start remaining) &&
        big_length (symbol_range start remaining) === Bigint.of_int remaining
        else true} @ immutable contended =
    fun start remaining ->
    ghost_ (symbol_range_def start remaining);
    let symbols = symbol_range start remaining in
    ghost_ (distinct_ints_def symbols);
    ghost_ (big_length_def symbols);
    let u = () in
    if remaining <= 0 then u
    else begin
      let next_start = start + 1 in
      let next_remaining = remaining - 1 in
      ghost_ (symbol_range_member next_start next_remaining start);
      ghost_ (symbol_range_properties next_start next_remaining);
      u
    end
  [@@decreases remaining]

  let[@def] rec (missing_letter @ total) :
      (letters : int list) -> (candidate : int) -> (remaining : int) ->
      int option @ immutable contended =
    fun letters candidate remaining ->
    if remaining <= 0 then None
    else if has_letter candidate letters then
      missing_letter letters (candidate + 1) (remaining - 1)
    else Some candidate
  [@@decreases remaining]

  let rec (missing_letter_sound @ total) :
      (letters : int list) -> (candidate : int) -> (remaining : int) ->
      {u : unit | match missing_letter letters candidate remaining with
        | None -> true
        | Some outsider -> not (has_letter outsider letters)}
        @ immutable contended =
    fun letters candidate remaining ->
    ghost_ (missing_letter_def letters candidate remaining);
    let u = () in
    if remaining <= 0 then u
    else if has_letter candidate letters then begin
      let next_candidate = candidate + 1 in
      let next_remaining = remaining - 1 in
      ghost_ (missing_letter_sound letters next_candidate next_remaining);
      u
    end else u
  [@@decreases remaining]

  let rec (missing_letter_exhausted @ total) :
      (letters : int list) -> (candidate : int) -> (remaining : int) ->
      {u : unit | if missing_letter letters candidate remaining === None then
        included (symbol_range candidate remaining) letters else true}
        @ immutable contended =
    fun letters candidate remaining ->
    ghost_ (missing_letter_def letters candidate remaining);
    ghost_ (symbol_range_def candidate remaining);
    let symbols = symbol_range candidate remaining in
    ghost_ (included_def symbols letters);
    let u = () in
    if remaining <= 0 then u
    else if has_letter candidate letters then begin
      let next_candidate = candidate + 1 in
      let next_remaining = remaining - 1 in
      ghost_ (missing_letter_exhausted letters next_candidate next_remaining);
      u
    end else u
  [@@decreases remaining]

  let (missing_letter_complete @ total) (letters : int list)
      (candidate : int) (remaining : int) :
      {u : unit | if 0 <= candidate && candidate <= 129 &&
        0 <= remaining && remaining <= 129 && candidate + remaining <= 129 &&
        Bigint.compare (big_length letters) (Bigint.of_int remaining) < 0 then
        match missing_letter letters candidate remaining with
        | None -> false
        | Some _ -> true
        else true} =
    let symbols = symbol_range candidate remaining in
    ghost_ (symbol_range_properties candidate remaining);
    ghost_ (missing_letter_exhausted letters candidate remaining);
    ghost_ (included_bound symbols letters);
    let u = () in u

  let (missing_letter_budget @ total) (letters : int list) :
      {u : unit | if list_size letters <= 128 then
        match missing_letter letters 0 (list_size letters + 1) with
        | None -> false
        | Some _ -> true
        else true} =
    ghost_ (list_size_properties letters);
    let zero = 0 in
    let budget = list_size letters + 1 in
    ghost_ (missing_letter_complete letters zero budget);
    let u = () in u

  let (outside_pair_step @ total) (left : machine) (right : machine)
      (p : int) (q : int) (outsider : int) (letter : int) :
      {u : unit | let letters = append (labels left p) (labels right q) in
        if not (has_letter outsider letters) &&
          not (has_letter letter letters) then
          step left p outsider === step left p letter &&
          step right q outsider === step right q letter
        else true} =
    let left_labels = labels left p in
    let right_labels = labels right q in
    ghost_ (append_letter left_labels right_labels outsider);
    ghost_ (append_letter left_labels right_labels letter);
    ghost_ (step_outside left p outsider);
    ghost_ (step_outside right q outsider);
    ghost_ (step_outside left p letter);
    ghost_ (step_outside right q letter);
    let u = () in u

  let (outside_pair_default @ total) (left : machine)
      (right : machine) (p : int) (q : int) (outsider : int) :
      {u : unit | let letters =
          append (labels left p) (labels right q) in
        if not (has_letter outsider letters) then
          step left p outsider === default left p &&
          step right q outsider === default right q
        else true} =
    let left_labels = labels left p in
    let right_labels = labels right q in
    ghost_ (append_letter left_labels right_labels outsider);
    ghost_ (step_outside left p outsider);
    ghost_ (step_outside right q outsider);
    let u = () in u

  let (expanded_pair_closed @ total) (left : machine)
      (right : machine) (p : int) (q : int)
      (word : int list) (outsider : int)
      (pending : (int * int * int list) list)
      (seen : relation) (count : int) (limit : int) :
      {u : unit | let letters =
          append (labels left p) (labels right q) in
        if final left p = final right q &&
          not (has_letter outsider letters) then
          match push_labels left right p q word letters
              pending seen count limit with
          | None -> true
          | Some (updated_pending, updated_seen, updated_count) ->
            let pair = step left p outsider, step right q outsider in
            let next_word = append word [outsider] in
            (match push_pair limit pair next_word updated_pending
                updated_seen updated_count with
             | None -> true
             | Some (_, final_seen, _) ->
               pair_closed left right p q final_seen)
        else true} =
    let letters = append (labels left p) (labels right q) in
    ghost_ (push_labels_closed left right p q word letters
      pending seen count limit);
    let u = () in
    match push_labels left right p q word letters
        pending seen count limit with
    | None -> u
    | Some (updated_pending, updated_seen, updated_count) ->
      let pair = step left p outsider, step right q outsider in
      let next_word = append word [outsider] in
      ghost_ (push_pair_related limit pair next_word
        updated_pending updated_seen updated_count);
      ghost_ (push_pair_seen_included limit pair next_word
        updated_pending updated_seen updated_count);
      (match push_pair limit pair next_word
          updated_pending updated_seen updated_count with
       | None -> u
       | Some (_, final_seen, _) ->
         ghost_ (outside_pair_default left right p q outsider);
         ghost_ (labelled_closed_weaken left right p q letters
           updated_seen final_seen);
         ghost_ (pair_closed_def left right p q final_seen);
         u)

  let rec (search_product @ total) :
      (left : machine) -> (right : machine) ->
      (pending : (int * int * int list) list) ->
      (seen : relation) -> (processed : relation) @ ghost ->
      (count : int) -> (limit : int) ->
      (remaining : {fuel : int | 0 <= fuel && fuel <= 65_536 &&
        0 <= count && count <= limit && limit <= 65_536 &&
        Bigint.of_int count === big_length seen &&
        pending_valid left right pending && distinct_pairs seen &&
        (if valid left && valid right then pairs_valid left right seen else true) &&
        Bigint.add (Bigint.of_int fuel) (Bigint.of_int count) ===
          Bigint.add (Bigint.of_int limit) (big_length pending)}) @ ghost ->
      {decision : decision |
        (if valid left && valid right && labels_bounded left && labels_bounded right &&
          Bigint.compare (Bigint.mul (state_size left) (state_size right))
            (Bigint.of_int limit) <= 0 then
          match decision with Limit -> false | Equal _ | Different _ -> true
         else true) &&
        (match decision with
         | Equal relation -> distinct_pairs relation &&
           (if valid left && valid right then pairs_valid left right relation
            else true)
         | Different _ | Limit -> true) &&
        (match pending with
         | [] -> decision === Equal seen
         | _ :: _ -> true) &&
        let left_initial, _ = left in
        let right_initial, _ = right in
        if pending_valid left right pending &&
          all_seen_accounted seen processed pending &&
          all_closed left right seen processed &&
          related (left_initial, right_initial) seen then
        match decision with
        | Different word -> run left word <> run right word
        | Equal relation -> check left right relation
        | Limit -> true
        else true} @ immutable contended =
    fun left right pending seen processed count limit remaining ->
    match pending with
    | [] ->
      ghost_ (accounted_empty_included seen processed);
      ghost_ (closed_processed_covers_seen left right seen processed seen);
      ghost_ (check_def left right seen);
      let decision = Equal seen in decision
    | (p, q, word) :: rest ->
      ghost_ (big_length_def pending);
      ghost_ (product_pending_length_nonnegative rest);
      ghost_ (pending_valid_def left right pending);
      ghost_ (reached_valid left word);
      ghost_ (reached_valid right word);
      ghost_ (labels_bounded_state left p);
      ghost_ (labels_bounded_state right q);
      if final left p <> final right q then
        let left_initial, _ = left in
        let right_initial, _ = right in
        ghost_ (reached_def left word);
        ghost_ (reached_def right word);
        ghost_ (execute_reached left left_initial word);
        ghost_ (execute_reached right right_initial word);
        ghost_ (run_def left word);
        ghost_ (run_def right word);
        let decision = Different word in decision
      else
        let left_labels = labels left p in
        let right_labels = labels right q in
        if list_size left_labels > 64 || list_size right_labels > 64
        then let decision = Limit in decision
        else
          let letters = append left_labels right_labels in
          let zero = 0 in
          let missing_budget = list_size letters + 1 in
          ghost_ (pair_labels_size left_labels right_labels);
          ghost_ (missing_letter_budget letters);
          ghost_ (missing_letter_sound letters zero missing_budget);
          match missing_letter letters zero missing_budget with
          | None -> let decision = Limit in decision
          | Some outsider ->
            let new_processed = ghost_ ((p, q) :: processed) in
            ghost_ (accounted_pop seen processed p q word rest);
            ghost_ (push_labels_accounted left right p q word letters
              rest seen new_processed count limit);
            ghost_ (push_labels_counts left right p q word letters
              rest seen count limit);
            ghost_ (push_labels_distinct left right p q word letters
              rest seen count limit);
            ghost_ (push_labels_domain left right p q word letters
              rest seen count limit);
            ghost_ (push_labels_capacity left right p q word letters
              rest seen count limit);
            ghost_ (push_labels_valid left right p q word letters
              rest seen count limit);
            ghost_ (push_labels_seen_included left right p q word letters
              rest seen count limit);
            ghost_ (expanded_pair_closed left right p q word outsider
              rest seen count limit);
            match push_labels left right p q word letters
                rest seen count limit with
            | None -> let decision = Limit in decision
            | Some (updated_pending, updated_seen, updated_count) ->
              let pair = step left p outsider, step right q outsider in
              let next_word = append word [outsider] in
              ghost_ (reached_push left word outsider);
              ghost_ (reached_push right word outsider);
              ghost_ (push_pair_accounted limit pair next_word
                updated_pending updated_seen new_processed updated_count);
              ghost_ (push_pair_seen_included limit pair next_word
                updated_pending updated_seen updated_count);
              ghost_ (push_pair_valid left right limit pair next_word
                updated_pending updated_seen updated_count);
              ghost_ (push_pair_counts limit pair next_word
                updated_pending updated_seen updated_count);
              ghost_ (push_pair_distinct limit pair next_word
                updated_pending updated_seen updated_count);
              ghost_ (step_valid left p outsider);
              ghost_ (step_valid right q outsider);
              ghost_ (push_pair_domain left right limit pair next_word
                updated_pending updated_seen updated_count);
              ghost_ (push_pair_capacity left right limit pair next_word
                updated_pending updated_seen updated_count);
              match push_pair limit pair next_word
                  updated_pending updated_seen updated_count with
              | None -> let decision = Limit in decision
              | Some (next_pending, next_seen, next_count) ->
                ghost_ (relation_included_trans seen updated_seen next_seen);
                ghost_ (all_closed_weaken left right seen next_seen processed);
                ghost_ (all_closed_def left right next_seen new_processed);
                let left_initial, _ = left in
                let right_initial, _ = right in
                let initial_pair = left_initial, right_initial in
                ghost_ (related_included initial_pair seen next_seen);
                let next_remaining = ghost_ (remaining - 1) in
                let recursive_decision =
                  search_product left right next_pending next_seen new_processed
                    next_count limit (next_remaining) in
                (match recursive_decision with
                 | Limit -> let decision = Limit in decision
                 | Equal relation ->
                   let decision = Equal relation in decision
                 | Different word ->
                   let decision = Different word in decision)
  [@@decreases let fuel = remaining in fuel]

  let (candidate @ total) (left : machine) (right : machine) (limit : int) :
      {decision : decision |
        (if valid left && valid right && labels_bounded left && labels_bounded right &&
          0 < limit && limit <= 65_536 &&
          Bigint.compare (Bigint.mul (state_size left) (state_size right))
            (Bigint.of_int limit) <= 0 then
          match decision with Limit -> false | Equal _ | Different _ -> true
         else true) &&
        match decision with
        | Different word -> run left word <> run right word
        | Equal relation -> check left right relation
        | Limit -> true} =
    let left_initial, _ = left in
    let right_initial, _ = right in
    let pair = left_initial, right_initial in
    let count = if limit > 65_536 then 65_536 else limit in
    if count <= 0 then let decision = Limit in decision
    else
      let empty_word = [] in
      let empty_pending : (int * int * int list) list = [] in
      let empty_seen : relation = [] in
      let empty_processed = [] in
      let zero = 0 in
      let remaining = count in
      ghost_ (big_length_def empty_pending);
      ghost_ (big_length_def empty_seen);
      ghost_ (distinct_pairs_def empty_seen);
      ghost_ (pairs_valid_def left right empty_seen);
      ghost_ (pending_valid_def left right empty_pending);
      ghost_ (all_seen_accounted_def empty_seen empty_processed empty_pending);
      ghost_ (reached_empty_equal left);
      ghost_ (reached_empty_equal right);
      ghost_ (reached_valid left empty_word);
      ghost_ (reached_valid right empty_word);
      ghost_ (push_pair_valid left right count pair empty_word empty_pending
        empty_seen zero);
      ghost_ (push_pair_accounted count pair empty_word empty_pending empty_seen
        empty_processed zero);
      ghost_ (push_pair_related count pair empty_word empty_pending empty_seen zero);
      ghost_ (push_pair_counts count pair empty_word empty_pending empty_seen zero);
      ghost_ (push_pair_distinct count pair empty_word empty_pending empty_seen zero);
      ghost_ (push_pair_domain left right count pair empty_word empty_pending empty_seen zero);
      ghost_ (pair_member_def pair empty_seen);
      ghost_ (push_pair_def count pair empty_word empty_pending empty_seen zero);
      match push_pair count pair empty_word empty_pending empty_seen zero with
      | None -> let decision = Limit in decision
      | Some (pending, seen, used) ->
        ghost_ (big_length_def pending);
        ghost_ (big_length_def seen);
        ghost_ (all_closed_def left right seen empty_processed);
        let recursive_decision =
          search_product left right pending seen empty_processed
            used count (remaining) in
        match recursive_decision with
        | Limit -> let decision = Limit in decision
        | Equal relation ->
          let decision = Equal relation in decision
        | Different word ->
          let decision = Different word in decision

  let (compare_states @ total) (source : machine)
      (p : int) (q : int) (limit : int) :
      {decision : decision |
        (if valid source && has_state source p && has_state source q &&
          labels_bounded source && 0 < limit && limit <= 65_536 &&
          Bigint.compare (Bigint.mul (state_size source) (state_size source))
            (Bigint.of_int limit) <= 0 then
          match decision with Limit -> false | Equal _ | Different _ -> true
         else true) &&
        match decision with
        | Different word -> run_from source p word <> run_from source q word
        | Equal relation -> related (p, q) relation &&
          all_closed source source relation relation
        | Limit -> true} =
    let _, table = source in
    let left = p, table in
    let right = q, table in
    ghost_ (valid_def source);
    ghost_ (valid_def left);
    ghost_ (valid_def right);
    ghost_ (has_state_def source p);
    ghost_ (has_state_def source q);
    ghost_ (labels_bounded_def source);
    ghost_ (labels_bounded_def left);
    ghost_ (labels_bounded_def right);
    let states = state_ids table in
    ghost_ (bounded_labels_rebased source p states);
    ghost_ (bounded_labels_rebased source q states);
    ghost_ (state_size_def source);
    ghost_ (state_size_def left);
    ghost_ (state_size_def right);
    let decision = candidate left right limit in
    match decision with
    | Limit -> let result = Limit in result
    | Equal relation ->
      ghost_ (check_def left right relation);
      ghost_ (all_closed_rebased source p q relation relation);
      let result = Equal relation in result
    | Different word ->
      ghost_ (run_rebased source p word);
      ghost_ (run_rebased source q word);
      let result = Different word in result

  let (diagnose_comparison @ total) (left : machine) (right : machine) (limit : int) :
      {decision : decision | valid_decision left right decision &&
        (if valid left && valid right && labels_bounded left && labels_bounded right &&
          0 < limit && limit <= 65_536 &&
          Bigint.compare (Bigint.mul (state_size left) (state_size right))
            (Bigint.of_int limit) <= 0 then
          match decision with Limit -> false | Equal _ | Different _ -> true
         else true)} =
    let proposal = candidate left right limit in
    match proposal with
    | Limit ->
      let decision = Limit in
      ghost_ (valid_decision_def left right decision);
      decision

    | Different word ->
      let decision = Different word in
      ghost_ (valid_decision_def left right decision);
      decision
    | Equal relation ->
      let decision = Equal relation in
      ghost_ (valid_decision_def left right decision);
      decision

  let[@def] rec access_member state (entries : (int * int list) list) =
    match entries with
    | [] -> false
    | (candidate, _) :: rest ->
      candidate = state || access_member state rest

  let[@def] rec access_valid source (entries : (int * int list) list) =
    match entries with
    | [] -> true
    | (state, word) :: rest ->
      reached source word = state && access_valid source rest

  let (access_valid_singleton @ total) (source : machine)
      (state : int) (word : int list) :
      {u : unit | access_valid source [state, word] ===
        (reached source word = state)} =
    let empty = [] in
    let singleton = [state, word] in
    ghost_ (access_valid_def source singleton);
    ghost_ (access_valid_def source empty);
    let u = () in u

  let[@def] rec reach_labels_closed source seen state letters =
    match letters with
    | [] -> true
    | letter :: rest ->
      access_member (step source state letter) seen &&
      reach_labels_closed source seen state rest

  let[@def] reach_state_closed source seen state =
    access_member (default source state) seen &&
    reach_labels_closed source seen state (labels source state)

  let[@def] rec (all_reach_closed @ total) :
      machine -> (int * int list) list -> (int * int list) list ->
      bool @ immutable contended =
    fun source seen processed ->
    match processed with
    | [] -> true
    | (state, _) :: rest ->
      reach_state_closed source seen state &&
      all_reach_closed source seen rest

  let (all_reach_closed_empty @ total) (source : machine)
      (seen : (int * int list) list) :
      {u : unit | all_reach_closed source seen []} =
    let empty = [] in
    ghost_ (all_reach_closed_def source seen empty);
    let u = () in u

  let rec (reach_labels_closed_member @ total) :
      (source : machine) -> (seen : (int * int list) list) ->
      (state : int) -> (letters : int list) -> (letter : int) ->
      {u : unit | if reach_labels_closed source seen state letters &&
        has_letter letter letters then
        access_member (step source state letter) seen else true}
        @ immutable contended =
    fun source seen state letters letter ->
    ghost_ (reach_labels_closed_def source seen state letters);
    ghost_ (has_letter_def letter letters);
    let u = () in
    match letters with
    | [] -> u
    | _ :: rest ->
      ghost_ (reach_labels_closed_member source seen state rest letter);
      u

  let (reach_state_closed_step @ total) (source : machine)
      (seen : (int * int list) list) (state : int) (letter : int) :
      {u : unit | if reach_state_closed source seen state then
        access_member (step source state letter) seen else true} =
    let letters = labels source state in
    ghost_ (reach_state_closed_def source seen state);
    let u = () in
    if has_letter letter letters then begin
      ghost_ (reach_labels_closed_member source seen state letters letter);
      u
    end else begin
      ghost_ (step_outside source state letter);
      u
    end

  let rec (all_reach_closed_member @ total) :
      (source : machine) -> (seen : (int * int list) list) ->
      (processed : (int * int list) list) -> (state : int) ->
      {u : unit | if all_reach_closed source seen processed &&
        access_member state processed then
        reach_state_closed source seen state else true}
        @ immutable contended =
    fun source seen processed state ->
    ghost_ (all_reach_closed_def source seen processed);
    ghost_ (access_member_def state processed);
    let u = () in
    match processed with
    | [] -> u
    | _ :: rest ->
      ghost_ (all_reach_closed_member source seen rest state);
      u

  let rec (drive_reach_closed @ total) :
      (source : machine) -> (seen : (int * int list) list) ->
      (state : int) -> (word : int list) ->
      {u : unit | if all_reach_closed source seen seen &&
        access_member state seen then
        access_member (drive source state word) seen else true}
        @ immutable contended =
    fun source seen state word ->
    ghost_ (drive_def source state word);
    let u = () in
    match word with
    | [] -> u
    | letter :: rest ->
      ghost_ (all_reach_closed_member source seen seen state);
      ghost_ (reach_state_closed_step source seen state letter);
      let target = step source state letter in
      ghost_ (drive_reach_closed source seen target rest);
      u

  let (reachable_word_member @ total) (source : machine)
      (seen : (int * int list) list) (word : int list) :
      {u : unit | let initial, _ = source in
        if all_reach_closed source seen seen &&
          access_member initial seen then
          access_member (reached source word) seen else true} =
    let initial, _ = source in
    ghost_ (drive_reach_closed source seen initial word);
    ghost_ (reached_def source word);
    let u = () in u

  let[@def] rec states_of_entries entries =
    match entries with
    | [] -> []
    | (state, _) :: rest -> state :: states_of_entries rest

  let rec (states_of_entries_member @ total) :
      (entries : (int * int list) list) -> (state : int) ->
      {u : unit | has_letter state (states_of_entries entries) ===
        access_member state entries} @ immutable contended =
    fun entries state ->
    ghost_ (states_of_entries_def entries);
    ghost_ (access_member_def state entries);
    let states = states_of_entries entries in
    ghost_ (has_letter_def state states);
    let u = () in
    match entries with
    | [] -> u
    | _ :: rest ->
      ghost_ (states_of_entries_member rest state);
      u

  let rec (states_of_entries_length @ total) :
      (entries : (int * int list) list) ->
      {u : unit | big_length (states_of_entries entries) === big_length entries}
        @ immutable contended =
    fun entries ->
    ghost_ (states_of_entries_def entries);
    ghost_ (big_length_def entries);
    let states = states_of_entries entries in
    ghost_ (big_length_def states);
    let u = () in
    match entries with
    | [] -> u
    | _ :: rest ->
      ghost_ (states_of_entries_length rest);
      u

  let rec (access_states_included @ total) :
      (source : machine) -> (entries : (int * int list) list) ->
      {u : unit | let _, table = source in
        if valid source && access_valid source entries then
          included (states_of_entries entries) (state_ids table)
        else true} @ immutable contended =
    fun source entries ->
    let _, table = source in
    let states = states_of_entries entries in
    let domain = state_ids table in
    ghost_ (states_of_entries_def entries);
    ghost_ (access_valid_def source entries);
    ghost_ (included_def states domain);
    let u = () in
    match entries with
    | [] -> u
    | (state, word) :: rest ->
      ghost_ (reached_valid source word);
      ghost_ (has_state_def source state);
      ghost_ (ids_member table state);
      ghost_ (access_states_included source rest);
      u

  let (access_count_bound @ total) (source : machine)
      (entries : (int * int list) list) :
      {u : unit | if valid source && access_valid source entries &&
        distinct_ints (states_of_entries entries) then
        Bigint.compare (big_length entries) (state_size source) <= 0
        else true} =
    let _, table = source in
    let states = states_of_entries entries in
    let domain = state_ids table in
    ghost_ (access_states_included source entries);
    ghost_ (states_of_entries_length entries);
    ghost_ (included_bound states domain);
    ghost_ (state_size_def source);
    let u = () in u

  let (access_fresh_bound @ total) (source : machine)
      (entries : (int * int list) list) (state : int) :
      {u : unit | if valid source && access_valid source entries &&
        distinct_ints (states_of_entries entries) &&
        not (access_member state entries) && has_state source state then
        Bigint.compare (Bigint.add 1Z (big_length entries))
          (state_size source) <= 0 else true} =
    let _, table = source in
    let states = states_of_entries entries in
    let domain = state_ids table in
    let extended = state :: states in
    ghost_ (access_states_included source entries);
    ghost_ (states_of_entries_length entries);
    ghost_ (states_of_entries_member entries state);
    ghost_ (has_state_def source state);
    ghost_ (ids_member table state);
    ghost_ (included_def extended domain);
    ghost_ (distinct_ints_def extended);
    ghost_ (included_bound extended domain);
    ghost_ (big_length_def extended);
    ghost_ (state_size_def source);
    let u = () in u

  let (reachable_states_step @ total) (source : machine)
      (entries : (int * int list) list) (state : int)
      (letter : int) :
      {u : unit | if all_reach_closed source entries entries &&
        has_letter state (states_of_entries entries) then
        has_letter (step source state letter)
          (states_of_entries entries) else true} =
    ghost_ (states_of_entries_member entries state);
    ghost_ (all_reach_closed_member source entries entries state);
    ghost_ (reach_state_closed_step source entries state letter);
    let target = step source state letter in
    ghost_ (states_of_entries_member entries target);
    let u = () in u

  let (reachable_states_default @ total) (source : machine)
      (entries : (int * int list) list) (state : int) :
      {u : unit | if all_reach_closed source entries entries &&
        has_letter state (states_of_entries entries) then
        has_letter (default source state)
          (states_of_entries entries) else true} =
    ghost_ (states_of_entries_member entries state);
    ghost_ (all_reach_closed_member source entries entries state);
    ghost_ (reach_state_closed_def source entries state);
    let target = default source state in
    ghost_ (states_of_entries_member entries target);
    let u = () in u

  let (access_member_cons @ total) (state : int) (head : int)
      (word : int list) (rest : (int * int list) list) :
      {u : unit | access_member state ((head, word) :: rest) ===
        (state = head || access_member state rest)} =
    let extended = (head, word) :: rest in
    ghost_ (access_member_def state extended);
    let u = () in u

  let rec (access_word_complete @ total) :
      (state : int) -> (access : (int * int list) list) ->
      {u : unit | if access_member state access then
        match access_word state access with
        | None -> false
        | Some _ -> true else true} @ immutable contended =
    fun state access ->
    ghost_ (access_member_def state access);
    ghost_ (access_word_def state access);
    let u = () in
    match access with
    | [] -> u
    | _ :: rest ->
      ghost_ (access_word_complete state rest);
      u

  let (access_member_weaken @ total) (state : int) (head : int)
      (word : int list) (rest : (int * int list) list) :
      {u : unit | if access_member state rest then
        access_member state ((head, word) :: rest) else true} =
    ghost_ (access_member_cons state head word rest);
    let u = () in u

  let[@def] rec access_included before after =
    match before with
    | [] -> true
    | (state, _) :: rest ->
      access_member state after && access_included rest after

  let rec (access_included_prepend @ total) :
      (before : (int * int list) list) ->
      (after : (int * int list) list) ->
      (head : int) -> (word : int list) ->
      {u : unit | if access_included before after then
        access_included before ((head, word) :: after) else true}
        @ immutable contended =
    fun before after head word ->
    ghost_ (access_included_def before after);
    let extended = (head, word) :: after in
    ghost_ (access_included_def before extended);
    let u = () in
    match before with
    | [] -> u
    | (state, _) :: rest ->
      ghost_ (access_member_weaken state head word after);
      ghost_ (access_included_prepend rest after head word);
      u

  let rec (access_included_self @ total) :
      (entries : (int * int list) list) ->
      {u : unit | access_included entries entries}
        @ immutable contended =
    fun entries ->
    ghost_ (access_included_def entries entries);
    let u = () in
    match entries with
    | [] -> u
    | (state, word) :: rest ->
      ghost_ (access_included_self rest);
      ghost_ (access_included_prepend rest rest state word);
      ghost_ (access_member_cons state state word rest);
      u

  let rec (access_member_included @ total) :
      (state : int) -> (before : (int * int list) list) ->
      (after : (int * int list) list) ->
      {u : unit | if access_included before after &&
        access_member state before then
        access_member state after else true}
        @ immutable contended =
    fun state before after ->
    ghost_ (access_included_def before after);
    ghost_ (access_member_def state before);
    let u = () in
    match before with
    | [] -> u
    | _ :: rest ->
      ghost_ (access_member_included state rest after);
      u

  let rec (access_included_trans @ total) :
      (first : (int * int list) list) ->
      (second : (int * int list) list) ->
      (third : (int * int list) list) ->
      {u : unit | if access_included first second &&
        access_included second third then
        access_included first third else true}
        @ immutable contended =
    fun first second third ->
    ghost_ (access_included_def first second);
    ghost_ (access_included_def first third);
    let u = () in
    match first with
    | [] -> u
    | (state, _) :: rest ->
      ghost_ (access_member_included state second third);
      ghost_ (access_included_trans rest second third);
      u

  let rec (reach_labels_closed_weaken @ total) :
      (source : machine) ->
      (before : (int * int list) list) ->
      (after : (int * int list) list) ->
      (state : int) -> (letters : int list) ->
      {u : unit | if access_included before after &&
        reach_labels_closed source before state letters then
        reach_labels_closed source after state letters else true}
        @ immutable contended =
    fun source before after state letters ->
    ghost_ (reach_labels_closed_def source before state letters);
    ghost_ (reach_labels_closed_def source after state letters);
    let u = () in
    match letters with
    | [] -> u
    | letter :: rest ->
      let target = step source state letter in
      ghost_ (access_member_included target before after);
      ghost_ (reach_labels_closed_weaken source before after state rest);
      u

  let (reach_state_closed_weaken @ total) (source : machine)
      (before : (int * int list) list)
      (after : (int * int list) list) (state : int) :
      {u : unit | if access_included before after &&
        reach_state_closed source before state then
        reach_state_closed source after state else true} =
    let fallback = default source state in
    let letters = labels source state in
    ghost_ (reach_state_closed_def source before state);
    ghost_ (reach_state_closed_def source after state);
    ghost_ (access_member_included fallback before after);
    ghost_ (reach_labels_closed_weaken source before after state letters);
    let u = () in u

  let rec (all_reach_closed_weaken @ total) :
      (source : machine) ->
      (before : (int * int list) list) ->
      (after : (int * int list) list) ->
      (processed : (int * int list) list) ->
      {u : unit | if access_included before after &&
        all_reach_closed source before processed then
        all_reach_closed source after processed else true}
        @ immutable contended =
    fun source before after processed ->
    ghost_ (all_reach_closed_def source before processed);
    ghost_ (all_reach_closed_def source after processed);
    let u = () in
    match processed with
    | [] -> u
    | (state, _) :: rest ->
      ghost_ (reach_state_closed_weaken source before after state);
      ghost_ (all_reach_closed_weaken source before after rest);
      u

  let[@def] rec (reach_accounted @ total) :
      (int * int list) list -> (int * int list) list ->
      (int * int list) list -> bool @ immutable contended =
    fun seen processed pending ->
    match seen with
    | [] -> true
    | (state, _) :: rest ->
      (access_member state processed || access_member state pending) &&
      reach_accounted rest processed pending

  let (reach_accounted_empty @ total)
      (processed : (int * int list) list)
      (pending : (int * int list) list) :
      {u : unit | reach_accounted [] processed pending} =
    let empty = [] in
    ghost_ (reach_accounted_def empty processed pending);
    let u = () in u

  let (reach_accounted_singleton @ total) (state : int)
      (word : int list) :
      {u : unit | reach_accounted [state, word] [] [state, word]} =
    let empty = [] in
    let singleton = [state, word] in
    ghost_ (access_member_cons state state word empty);
    ghost_ (reach_accounted_empty empty singleton);
    ghost_ (reach_accounted_def singleton empty singleton);
    let u = () in u

  let rec (reach_accounted_pending_weaken @ total) :
      (seen : (int * int list) list) ->
      (processed : (int * int list) list) ->
      (pending : (int * int list) list) ->
      (state : int) -> (word : int list) ->
      {u : unit | if reach_accounted seen processed pending then
        reach_accounted seen processed ((state, word) :: pending)
        else true} @ immutable contended =
    fun seen processed pending state word ->
    ghost_ (reach_accounted_def seen processed pending);
    let extended = (state, word) :: pending in
    ghost_ (reach_accounted_def seen processed extended);
    let u = () in
    match seen with
    | [] -> u
    | (current, _) :: rest ->
      ghost_ (access_member_weaken current state word pending);
      ghost_ (reach_accounted_pending_weaken rest processed pending state word);
      u

  let rec (reach_accounted_pop @ total) :
      (seen : (int * int list) list) ->
      (processed : (int * int list) list) ->
      (state : int) -> (word : int list) ->
      (rest : (int * int list) list) ->
      {u : unit | if reach_accounted seen processed
          ((state, word) :: rest) then
        reach_accounted seen ((state, word) :: processed) rest
        else true} @ immutable contended =
    fun seen processed state word rest ->
    let pending = (state, word) :: rest in
    let new_processed = (state, word) :: processed in
    ghost_ (reach_accounted_def seen processed pending);
    ghost_ (reach_accounted_def seen new_processed rest);
    let u = () in
    match seen with
    | [] -> u
    | (current, _) :: tail ->
      ghost_ (access_member_cons current state word rest);
      ghost_ (access_member_cons current state word processed);
      ghost_ (reach_accounted_pop tail processed state word rest);
      u

  let rec (reach_accounted_empty_included @ total) :
      (seen : (int * int list) list) ->
      (processed : (int * int list) list) ->
      {u : unit | if reach_accounted seen processed [] then
        access_included seen processed else true}
        @ immutable contended =
    fun seen processed ->
    let nil : (int * int list) list = [] in
    ghost_ (reach_accounted_def seen processed nil);
    ghost_ (access_included_def seen processed);
    let u = () in
    match seen with
    | [] -> u
    | (state, _) :: rest ->
      ghost_ (access_member_def state nil);
      ghost_ (reach_accounted_empty_included rest processed);
      u

  let rec (reach_closed_processed_covers_seen @ total) :
      (source : machine) -> (seen : (int * int list) list) ->
      (processed : (int * int list) list) ->
      (remaining : (int * int list) list) ->
      {u : unit | if all_reach_closed source seen processed &&
        access_included remaining processed then
        all_reach_closed source seen remaining else true}
        @ immutable contended =
    fun source seen processed remaining ->
    ghost_ (all_reach_closed_def source seen remaining);
    ghost_ (access_included_def remaining processed);
    let u = () in
    match remaining with
    | [] -> u
    | (state, _) :: rest ->
      ghost_ (all_reach_closed_member source seen processed state);
      ghost_ (reach_closed_processed_covers_seen source seen processed rest);
      u

  let[@def] push_state limit state word pending seen count =
    if access_member state seen then Some (pending, seen, count)
    else if count >= limit then None
    else Some ((state, word) :: pending,
      (state, word) :: seen, count + 1)

  let rec (access_length_nonnegative @ total) :
      (entries : (int * int list) list) ->
      {u : unit | Bigint.compare 0Z (big_length entries) <= 0}
        @ immutable contended =
    fun entries ->
    ghost_ (big_length_def entries);
    let u = () in
    match entries with
    | [] -> u
    | _ :: rest ->
      ghost_ (access_length_nonnegative rest);
      u

  let (push_state_distinct @ total) (limit : int) (state : int)
      (word : int list) (pending : (int * int list) list)
      (seen : (int * int list) list) (count : int) :
      {u : unit | if distinct_ints (states_of_entries seen) then
        match push_state limit state word pending seen count with
        | None -> true
        | Some (_, new_seen, _) -> distinct_ints (states_of_entries new_seen)
        else true} =
    ghost_ (push_state_def limit state word pending seen count);
    ghost_ (states_of_entries_member seen state);
    let extended = (state, word) :: seen in
    ghost_ (states_of_entries_def extended);
    let states = states_of_entries extended in
    ghost_ (distinct_ints_def states);
    let u = () in u

  let (push_state_counts @ total) (limit : int) (state : int)
      (word : int list) (pending : (int * int list) list)
      (seen : (int * int list) list) (count : int) :
      {u : unit | if 0 <= count && count <= limit && limit <= 65_536 &&
        Bigint.of_int count === big_length seen then
        match push_state limit state word pending seen count with
        | None -> count = limit && not (access_member state seen)
        | Some (new_pending, new_seen, new_count) ->
          0 <= new_count && new_count <= limit &&
          Bigint.of_int new_count === big_length new_seen &&
          Bigint.add (Bigint.of_int new_count) (big_length pending) ===
            Bigint.add (Bigint.of_int count) (big_length new_pending)
        else true} =
    ghost_ (push_state_def limit state word pending seen count);
    let new_pending = (state, word) :: pending in
    let new_seen = (state, word) :: seen in
    ghost_ (big_length_def new_pending);
    ghost_ (big_length_def new_seen);
    let u = () in u

  let (push_state_capacity @ total) (source : machine) (limit : int)
      (state : int) (word : int list) (pending : (int * int list) list)
      (seen : (int * int list) list) (count : int) :
      {u : unit | if valid source && access_valid source seen &&
        distinct_ints (states_of_entries seen) && has_state source state &&
        0 <= count && count <= limit && limit <= 65_536 &&
        Bigint.of_int count === big_length seen &&
        Bigint.compare (state_size source) (Bigint.of_int limit) <= 0 then
        match push_state limit state word pending seen count with
        | None -> false
        | Some _ -> true
        else true} =
    ghost_ (push_state_counts limit state word pending seen count);
    ghost_ (access_fresh_bound source seen state);
    let u = () in u

  let (push_state_valid @ total) (source : machine) (limit : int)
      (state : int) (word : int list)
      (pending : (int * int list) list)
      (seen : (int * int list) list) (count : int) :
      {u : unit | if access_valid source pending &&
        access_valid source seen && reached source word = state then
        match push_state limit state word pending seen count with
        | None -> true
        | Some (new_pending, new_seen, _) ->
          access_valid source new_pending && access_valid source new_seen
        else true} =
    ghost_ (push_state_def limit state word pending seen count);
    let u = () in
    if access_member state seen then u
    else if count >= limit then u
    else begin
      let entry = state, word in
      let new_pending = entry :: pending in
      let new_seen = entry :: seen in
      ghost_ (access_valid_def source new_pending);
      ghost_ (access_valid_def source new_seen);
      u
    end

  let (push_state_seen_included @ total) (limit : int)
      (state : int) (word : int list)
      (pending : (int * int list) list)
      (seen : (int * int list) list) (count : int) :
      {u : unit | match push_state limit state word pending seen count with
        | None -> true
        | Some (_, new_seen, _) -> access_included seen new_seen} =
    ghost_ (push_state_def limit state word pending seen count);
    let u = () in
    if access_member state seen then begin
      ghost_ (access_included_self seen);
      u
    end else if count >= limit then u
    else begin
      ghost_ (access_included_self seen);
      ghost_ (access_included_prepend seen seen state word);
      u
    end

  let (push_state_target_member @ total) (limit : int)
      (state : int) (word : int list)
      (pending : (int * int list) list)
      (seen : (int * int list) list) (count : int) :
      {u : unit | match push_state limit state word pending seen count with
        | None -> true
        | Some (_, new_seen, _) -> access_member state new_seen} =
    ghost_ (push_state_def limit state word pending seen count);
    let u = () in
    if access_member state seen then u
    else if count >= limit then u
    else begin
      ghost_ (access_member_cons state state word seen);
      u
    end

  let (push_state_accounted @ total) (limit : int)
      (state : int) (word : int list)
      (pending : (int * int list) list)
      (seen : (int * int list) list)
      (processed : (int * int list) list) (count : int) :
      {u : unit | if reach_accounted seen processed pending then
        match push_state limit state word pending seen count with
        | None -> true
        | Some (new_pending, new_seen, _) ->
          reach_accounted new_seen processed new_pending
        else true} =
    ghost_ (push_state_def limit state word pending seen count);
    let u = () in
    if access_member state seen then u
    else if count >= limit then u
    else begin
      let new_pending = (state, word) :: pending in
      let new_seen = (state, word) :: seen in
      ghost_ (reach_accounted_pending_weaken seen processed
        pending state word);
      ghost_ (reach_accounted_def new_seen processed new_pending);
      ghost_ (access_member_cons state state word pending);
      u
    end

  let[@def] rec (expand_reachable_labels @ total) :
      (source : machine) -> (state : int) -> (word : int list) ->
      (letters : int list) -> (pending : (int * int list) list) ->
      (seen : (int * int list) list) -> (count : int) ->
      (limit : int) ->
      ((int * int list) list * (int * int list) list * int) option
        @ immutable contended =
    fun source state word letters pending seen count limit ->
    match letters with
    | [] -> Some (pending, seen, count)
    | letter :: rest ->
      let target = step source state letter in
      let next_word = append word [letter] in
      match push_state limit target next_word pending seen count with
      | None -> None
      | Some (new_pending, new_seen, new_count) ->
        expand_reachable_labels source state word rest
          new_pending new_seen new_count limit

  let rec (expand_reachable_labels_distinct @ total) :
      (source : machine) -> (state : int) -> (word : int list) ->
      (letters : int list) -> (pending : (int * int list) list) ->
      (seen : (int * int list) list) -> (count : int) -> (limit : int) ->
      {u : unit | if distinct_ints (states_of_entries seen) then
        match expand_reachable_labels source state word letters
          pending seen count limit with
        | None -> true
        | Some (_, new_seen, _) -> distinct_ints (states_of_entries new_seen)
        else true} @ immutable contended =
    fun source state word letters pending seen count limit ->
    ghost_ (expand_reachable_labels_def source state word letters pending seen count limit);
    let u = () in
    match letters with
    | [] -> u
    | letter :: rest ->
      let target = step source state letter in
      let next_word = append word [letter] in
      ghost_ (push_state_distinct limit target next_word pending seen count);
      (match push_state limit target next_word pending seen count with
       | None -> u
       | Some (new_pending, new_seen, new_count) ->
         ghost_ (expand_reachable_labels_distinct source state word rest
           new_pending new_seen new_count limit);
         u)

  let rec (expand_reachable_labels_counts @ total) :
      (source : machine) -> (state : int) -> (word : int list) ->
      (letters : int list) -> (pending : (int * int list) list) ->
      (seen : (int * int list) list) -> (count : int) -> (limit : int) ->
      {u : unit | if 0 <= count && count <= limit && limit <= 65_536 &&
        Bigint.of_int count === big_length seen then
        match expand_reachable_labels source state word letters
          pending seen count limit with
        | None -> true
        | Some (new_pending, new_seen, new_count) ->
          0 <= new_count && new_count <= limit &&
          Bigint.of_int new_count === big_length new_seen &&
          Bigint.add (Bigint.of_int new_count) (big_length pending) ===
            Bigint.add (Bigint.of_int count) (big_length new_pending)
        else true} @ immutable contended =
    fun source state word letters pending seen count limit ->
    ghost_ (expand_reachable_labels_def source state word letters
      pending seen count limit);
    let u = () in
    match letters with
    | [] -> u
    | letter :: rest ->
      let target = step source state letter in
      let next_word = append word [letter] in
      ghost_ (push_state_counts limit target next_word pending seen count);
      (match push_state limit target next_word pending seen count with
       | None -> u
       | Some (new_pending, new_seen, new_count) ->
         ghost_ (expand_reachable_labels_counts source state word rest
           new_pending new_seen new_count limit);
         u)

  let rec (expand_reachable_labels_capacity @ total) :
      (source : machine) -> (state : int) -> (word : int list) ->
      (letters : int list) -> (pending : (int * int list) list) ->
      (seen : (int * int list) list) -> (count : int) -> (limit : int) ->
      {u : unit | if valid source && access_valid source pending &&
        access_valid source seen && reached source word = state &&
        distinct_ints (states_of_entries seen) &&
        0 <= count && count <= limit && limit <= 65_536 &&
        Bigint.of_int count === big_length seen &&
        Bigint.compare (state_size source) (Bigint.of_int limit) <= 0 then
        match expand_reachable_labels source state word letters
          pending seen count limit with
        | None -> false
        | Some _ -> true
        else true} @ immutable contended =
    fun source state word letters pending seen count limit ->
    ghost_ (expand_reachable_labels_def source state word letters pending seen count limit);
    let u = () in
    match letters with
    | [] -> u
    | letter :: rest ->
      let target = step source state letter in
      let next_word = append word [letter] in
      ghost_ (reached_valid source word);
      ghost_ (step_valid source state letter);
      ghost_ (reached_push source word letter);
      ghost_ (push_state_capacity source limit target next_word pending seen count);
      ghost_ (push_state_counts limit target next_word pending seen count);
      ghost_ (push_state_distinct limit target next_word pending seen count);
      ghost_ (push_state_valid source limit target next_word pending seen count);
      (match push_state limit target next_word pending seen count with
       | None -> u
       | Some (new_pending, new_seen, new_count) ->
         ghost_ (expand_reachable_labels_capacity source state word rest
           new_pending new_seen new_count limit);
         u)

  let rec (expand_reachable_labels_seen_included @ total) :
      (source : machine) -> (state : int) -> (word : int list) ->
      (letters : int list) -> (pending : (int * int list) list) ->
      (seen : (int * int list) list) -> (count : int) ->
      (limit : int) ->
      {u : unit | match expand_reachable_labels source state word letters
          pending seen count limit with
        | None -> true
        | Some (_, new_seen, _) -> access_included seen new_seen}
        @ immutable contended =
    fun source state word letters pending seen count limit ->
    ghost_ (expand_reachable_labels_def source state word letters
      pending seen count limit);
    let u = () in
    match letters with
    | [] ->
      ghost_ (access_included_self seen);
      u
    | letter :: rest ->
      let target = step source state letter in
      let next_word = append word [letter] in
      ghost_ (push_state_seen_included limit target next_word pending seen count);
      (match push_state limit target next_word pending seen count with
       | None -> u
       | Some (new_pending, new_seen, new_count) ->
         ghost_ (expand_reachable_labels_seen_included source state word rest
           new_pending new_seen new_count limit);
         (match expand_reachable_labels source state word rest
             new_pending new_seen new_count limit with
          | None -> u
          | Some (_, final_seen, _) ->
            ghost_ (access_included_trans seen new_seen final_seen);
            u))

  let rec (expand_reachable_labels_accounted @ total) :
      (source : machine) -> (state : int) -> (word : int list) ->
      (letters : int list) -> (pending : (int * int list) list) ->
      (seen : (int * int list) list) ->
      (processed : (int * int list) list) ->
      (count : int) -> (limit : int) ->
      {u : unit | if reach_accounted seen processed pending then
        match expand_reachable_labels source state word letters
            pending seen count limit with
        | None -> true
        | Some (new_pending, new_seen, _) ->
          reach_accounted new_seen processed new_pending
        else true} @ immutable contended =
    fun source state word letters pending seen processed count limit ->
    ghost_ (expand_reachable_labels_def source state word letters
      pending seen count limit);
    let u = () in
    match letters with
    | [] -> u
    | letter :: rest ->
      let target = step source state letter in
      let next_word = append word [letter] in
      ghost_ (push_state_accounted limit target next_word
        pending seen processed count);
      (match push_state limit target next_word pending seen count with
       | None -> u
       | Some (new_pending, new_seen, new_count) ->
         ghost_ (expand_reachable_labels_accounted source state word rest
           new_pending new_seen processed new_count limit);
         u)

  let rec (expand_reachable_labels_closed @ total) :
      (source : machine) -> (state : int) -> (word : int list) ->
      (letters : int list) -> (pending : (int * int list) list) ->
      (seen : (int * int list) list) -> (count : int) ->
      (limit : int) ->
      {u : unit | match expand_reachable_labels source state word letters
          pending seen count limit with
        | None -> true
        | Some (_, new_seen, _) ->
          reach_labels_closed source new_seen state letters}
        @ immutable contended =
    fun source state word letters pending seen count limit ->
    ghost_ (expand_reachable_labels_def source state word letters
      pending seen count limit);
    let u = () in
    match letters with
    | [] ->
      ghost_ (reach_labels_closed_def source seen state letters);
      u
    | letter :: rest ->
      let target = step source state letter in
      let next_word = append word [letter] in
      ghost_ (push_state_target_member limit target next_word pending seen count);
      (match push_state limit target next_word pending seen count with
       | None -> u
       | Some (new_pending, new_seen, new_count) ->
         ghost_ (expand_reachable_labels_seen_included source state word rest
           new_pending new_seen new_count limit);
         ghost_ (expand_reachable_labels_closed source state word rest
           new_pending new_seen new_count limit);
         (match expand_reachable_labels source state word rest
             new_pending new_seen new_count limit with
          | None -> u
          | Some (_, final_seen, _) ->
            ghost_ (access_member_included target new_seen final_seen);
            ghost_ (reach_labels_closed_def source final_seen state letters);
            u))

  let rec (expand_reachable_labels_valid @ total) :
      (source : machine) -> (state : int) -> (word : int list) ->
      (letters : int list) -> (pending : (int * int list) list) ->
      (seen : (int * int list) list) -> (count : int) ->
      (limit : int) ->
      {u : unit | if access_valid source pending &&
        access_valid source seen && reached source word = state then
        match expand_reachable_labels source state word letters
            pending seen count limit with
        | None -> true
        | Some (new_pending, new_seen, _) ->
          access_valid source new_pending && access_valid source new_seen
        else true} @ immutable contended =
    fun source state word letters pending seen count limit ->
    ghost_ (expand_reachable_labels_def source state word letters
      pending seen count limit);
    let u = () in
    match letters with
    | [] -> u
    | letter :: rest ->
      let target = step source state letter in
      let next_word = append word [letter] in
      ghost_ (reached_push source word letter);
      ghost_ (push_state_valid source limit target next_word pending seen count);
      (match push_state limit target next_word pending seen count with
       | None -> u
       | Some (new_pending, new_seen, new_count) ->
         ghost_ (expand_reachable_labels_valid source state word rest
           new_pending new_seen new_count limit);
         u)

  let (expanded_reach_state_closed @ total) (source : machine)
      (state : int) (word : int list) (outsider : int)
      (pending : (int * int list) list)
      (seen : (int * int list) list)
      (count : int) (limit : int) :
      {u : unit | let letters = labels source state in
        if not (has_letter outsider letters) then
          match expand_reachable_labels source state word letters
              pending seen count limit with
          | None -> true
          | Some (updated_pending, updated_seen, updated_count) ->
            let target = step source state outsider in
            let next_word = append word [outsider] in
            (match push_state limit target next_word updated_pending
                updated_seen updated_count with
             | None -> true
             | Some (_, final_seen, _) ->
               reach_state_closed source final_seen state)
        else true} =
    let letters = labels source state in
    ghost_ (expand_reachable_labels_closed source state word letters
      pending seen count limit);
    let u = () in
    match expand_reachable_labels source state word letters
        pending seen count limit with
    | None -> u
    | Some (updated_pending, updated_seen, updated_count) ->
      let target = step source state outsider in
      let next_word = append word [outsider] in
      ghost_ (push_state_seen_included limit target next_word
        updated_pending updated_seen updated_count);
      ghost_ (push_state_target_member limit target next_word
        updated_pending updated_seen updated_count);
      (match push_state limit target next_word
          updated_pending updated_seen updated_count with
       | None -> u
       | Some (_, final_seen, _) ->
         ghost_ (step_outside source state outsider);
         ghost_ (reach_labels_closed_weaken source updated_seen
           final_seen state letters);
         ghost_ (reach_state_closed_def source final_seen state);
         u)

  let rec (reachable_search @ total) :
      (source : machine) -> (pending : (int * int list) list) ->
      (seen : (int * int list) list) ->
      (processed : (int * int list) list) @ ghost ->
      (count : int) -> (limit : int) ->
      (remaining : {fuel : int | 0 <= fuel && fuel <= 65_536 &&
        0 <= count && count <= limit && limit <= 65_536 &&
        Bigint.of_int count === big_length seen &&
        distinct_ints (states_of_entries seen) &&
        Bigint.add (Bigint.of_int fuel) (Bigint.of_int count) ===
          Bigint.add (Bigint.of_int limit) (big_length pending)}) @ ghost ->
      {result : (int * int list) list option |
        (if valid source && labels_bounded source &&
          access_valid source pending && access_valid source seen &&
          Bigint.compare (state_size source) (Bigint.of_int limit) <= 0 then
          match result with None -> false | Some _ -> true
         else true) &&
        (match result with
         | None -> true
         | Some entries -> distinct_ints (states_of_entries entries)) &&
        (match pending with
         | [] -> result === Some seen
         | _ :: _ -> true) &&
        let initial, _ = source in
        if access_valid source pending && access_valid source seen &&
          reach_accounted seen processed pending &&
          all_reach_closed source seen processed &&
          access_member initial seen then
          match result with
          | None -> true
          | Some entries -> access_valid source entries &&
            all_reach_closed source entries entries &&
            access_member initial entries
        else true} @ immutable contended =
    fun source pending seen processed count limit remaining ->
    match pending with
    | [] ->
      ghost_ (reach_accounted_empty_included seen processed);
      ghost_ (reach_closed_processed_covers_seen source seen processed seen);
      let result = Some seen in result
    | (state, word) :: rest ->
      ghost_ (big_length_def pending);
      ghost_ (access_length_nonnegative rest);
      ghost_ (access_valid_def source pending);
      ghost_ (reached_valid source word);
      ghost_ (labels_bounded_state source state);
      let letters = labels source state in
      if list_size letters > 64 then
        let result = None in result
      else
        let zero = 0 in
        let missing_budget = list_size letters + 1 in
        ghost_ (missing_letter_budget letters);
        ghost_ (missing_letter_sound letters zero missing_budget);
        match missing_letter letters zero missing_budget with
        | None -> let result = None in result
        | Some outsider ->
          let new_processed = ghost_ ((state, word) :: processed) in
          ghost_ (reach_accounted_pop seen processed state word rest);
          ghost_ (expand_reachable_labels_accounted source state word letters
            rest seen new_processed count limit);
          ghost_ (expand_reachable_labels_counts source state word letters
            rest seen count limit);
          ghost_ (expand_reachable_labels_distinct source state word letters
            rest seen count limit);
          ghost_ (expand_reachable_labels_seen_included source state word letters
            rest seen count limit);
          ghost_ (expanded_reach_state_closed source state word outsider
            rest seen count limit);
          ghost_ (expand_reachable_labels_valid source state word letters
            rest seen count limit);
          ghost_ (expand_reachable_labels_capacity source state word letters
            rest seen count limit);
          (match expand_reachable_labels source state word letters
              rest seen count limit with
           | None -> let result = None in result
           | Some (updated_pending, updated_seen, updated_count) ->
             let target = step source state outsider in
             let next_word = append word [outsider] in
             ghost_ (reached_push source word outsider);
             ghost_ (push_state_accounted limit target next_word
               updated_pending updated_seen new_processed updated_count);
             ghost_ (push_state_seen_included limit target next_word
               updated_pending updated_seen updated_count);
             ghost_ (push_state_valid source limit target next_word
               updated_pending updated_seen updated_count);
             ghost_ (push_state_counts limit target next_word
               updated_pending updated_seen updated_count);
             ghost_ (push_state_distinct limit target next_word
               updated_pending updated_seen updated_count);
             ghost_ (step_valid source state outsider);
             ghost_ (push_state_capacity source limit target next_word
               updated_pending updated_seen updated_count);
             (match push_state limit target next_word
                 updated_pending updated_seen updated_count with
              | None -> let result = None in result
              | Some (next_pending, next_seen, next_count) ->
                ghost_ (access_included_trans seen updated_seen next_seen);
                ghost_ (all_reach_closed_weaken source seen next_seen processed);
                ghost_ (all_reach_closed_def source next_seen new_processed);
                let initial, _ = source in
                ghost_ (access_member_included initial seen next_seen);
                let next_remaining = ghost_ (remaining - 1) in
                let result = reachable_search source
                  next_pending next_seen new_processed next_count limit
                  (next_remaining) in
                (match result with
                 | None -> let result = None in result
                 | Some entries ->
                   let result = Some entries in result)))
  [@@decreases let fuel = remaining in fuel]

  let (reachable_initial_access @ total) (source : machine) :
      {u : unit | let initial, _ = source in
        access_valid source [initial, []]} =
    let initial, _ = source in
    let empty_word = [] in
    ghost_ (reached_empty_equal source);
    ghost_ (access_valid_singleton source initial empty_word);
    let u = () in u

  let rec (find_access @ total) :
      (source : machine) -> (state : int) ->
      (entries : (int * int list) list) ->
      {result : int list option |
        if access_valid source entries && access_member state entries then
          match result with
          | None -> false
          | Some word -> reached source word === state
        else true} @ immutable contended =
    fun source state entries ->
    ghost_ (access_valid_def source entries);
    ghost_ (access_member_def state entries);
    match entries with
    | [] -> let result = None in result
    | (candidate, word) :: rest ->
      if state = candidate then
        let result = Some word in result
      else
        let result = find_access source state rest in
        result

  let (access_member_self @ total) (state : int) (word : int list) :
      {u : unit | access_member state [state, word]} =
    let singleton = [state, word] in
    ghost_ (access_member_def state singleton);
    let u = () in u

  let (reachable_search_initial @ total) (source : machine)
      (limit : int) :
      {result : (int * int list) list option |
        (if valid source && labels_bounded source && 0 < limit && limit <= 65_536 &&
          Bigint.compare (state_size source) (Bigint.of_int limit) <= 0 then
          match result with None -> false | Some _ -> true
         else true) &&
        let initial, _ = source in
        match result with
        | None -> true
        | Some entries ->
          access_valid source entries &&
          all_reach_closed source entries entries &&
          access_member initial entries &&
          distinct_ints (states_of_entries entries) &&
          (if valid source then
            Bigint.compare (big_length entries) (state_size source) <= 0
           else true)} =
    if limit <= 0 || limit > 65_536 then
      let result = None in result
    else
    let initial, _ = source in
    let empty_word = [] in
    let empty_entries : (int * int list) list = [] in
    let zero = 0 in
    ghost_ (big_length_def empty_entries);
    ghost_ (states_of_entries_def empty_entries);
    let empty_states = states_of_entries empty_entries in
    ghost_ (distinct_ints_def empty_states);
    ghost_ (access_valid_def source empty_entries);
    ghost_ (reach_accounted_empty empty_entries empty_entries);
    ghost_ (reached_empty_equal source);
    ghost_ (reached_valid source empty_word);
    ghost_ (push_state_valid source limit initial empty_word empty_entries
      empty_entries zero);
    ghost_ (push_state_accounted limit initial empty_word empty_entries
      empty_entries empty_entries zero);
    ghost_ (push_state_target_member limit initial empty_word empty_entries
      empty_entries zero);
    ghost_ (push_state_counts limit initial empty_word empty_entries
      empty_entries zero);
    ghost_ (push_state_distinct limit initial empty_word empty_entries empty_entries zero);
    ghost_ (push_state_capacity source limit initial empty_word empty_entries empty_entries zero);
    match push_state limit initial empty_word empty_entries empty_entries
        zero with
    | None -> let result = None in result
    | Some (pending, seen, count) ->
      ghost_ (all_reach_closed_empty source seen);
      let result = reachable_search source pending seen empty_entries
        count limit (limit) in
      (match result with
       | None -> let result = None in result
       | Some entries ->
         ghost_ (access_count_bound source entries);
         let result = Some entries in result)

  let rec (append_word @ total) :
      (left : int list) @ total -> (right : int list) @ total ->
      {result : int list | result === append left right}
        @ total immutable contended =
    fun left right ->
    ghost_ (append_def left right);
    match left with
    | [] -> right
    | letter :: rest ->
      let tail = append_word rest right in
      let result = letter :: tail in result

  module Access_trace : sig
    type t : value mod immutable
    val entries : t -> (int * int list) list @@ total
    val make : (values : (int * int list) list) @ total ->
      {result : t | entries result === values} @ total @@ total
  end = struct
    type t = (int * int list) list
    let[@def] entries values = values
    let (make @ total) (values : (int * int list) list @ total) :
        {result : t | entries result === values} @ total =
      ghost_ (entries_def values);
      values
  end

  type state_search = {
    pending_states : int list @@ total;
    seen_states : int list @@ total;
    search_count : int @@ total;
    pending_access : Access_trace.t Ghost.t @@ total;
    seen_access : Access_trace.t Ghost.t @@ total;
  }

  let[@def] state_search_valid (search : state_search @ total) = ghost_ (
    search.pending_states === states_of_entries (Access_trace.entries search.pending_access.ghost) &&
    search.seen_states === states_of_entries (Access_trace.entries search.seen_access.ghost))

  let[@def] state_search_view (search : state_search @ total) = ghost_ (
    Access_trace.entries search.pending_access.ghost,
    Access_trace.entries search.seen_access.ghost, search.search_count)

  let (push_search_state @ total) (limit : int) (state : int)
      (word : int list @ ghost) (before : state_search @ total) :
      {result : state_search option |
        if state_search_valid before then
          let pending, seen, count = state_search_view before in
          match result with
          | None -> push_state limit state word pending seen count === None
          | Some after -> state_search_valid after &&
            push_state limit state word pending seen count ===
              Some (state_search_view after)
        else true} @ total =
    let pending = ghost_ (Access_trace.entries before.pending_access.ghost) in
    let seen = ghost_ (Access_trace.entries before.seen_access.ghost) in
    let count = before.search_count in
    ghost_ (state_search_valid_def before);
    ghost_ (state_search_view_def before);
    ghost_ (states_of_entries_member seen state);
    ghost_ (push_state_def limit state word pending seen count);
    if has_letter state before.seen_states then
      let result = Some before in result
    else if count >= limit then
      let result = None in result
    else
      let pending_access = ghost_ ((state, word) :: pending) in
      let seen_access = ghost_ ((state, word) :: seen) in
      let pending_trace = ghost_ (Access_trace.make pending_access) in
      let seen_trace = ghost_ (Access_trace.make seen_access) in
      let after = {
        pending_states = state :: before.pending_states;
        seen_states = state :: before.seen_states;
        search_count = count + 1;
        pending_access = { ghost = pending_trace };
        seen_access = { ghost = seen_trace };
      } in
      ghost_ (states_of_entries_def pending_access);
      ghost_ (states_of_entries_def seen_access);
      ghost_ (state_search_valid_def after);
      ghost_ (state_search_view_def after);
      let result = Some after in result

  let rec (expand_search_labels @ total) :
      (source : machine) -> (state : int) -> (word : int list) @ ghost ->
      (letters : int list) -> (before : state_search) @ total -> (limit : int) ->
      {result : state_search option |
        if state_search_valid before then
          let pending, seen, count = state_search_view before in
          match result with
          | None -> expand_reachable_labels source state word letters
              pending seen count limit === None
          | Some after -> state_search_valid after &&
            expand_reachable_labels source state word letters
              pending seen count limit === Some (state_search_view after)
        else true} @ total immutable contended =
    fun source state word letters before limit ->
    let pending = ghost_ (Access_trace.entries before.pending_access.ghost) in
    let seen = ghost_ (Access_trace.entries before.seen_access.ghost) in
    let count = before.search_count in
    ghost_ (state_search_view_def before);
    ghost_ (expand_reachable_labels_def source state word letters
      pending seen count limit);
    match letters with
    | [] -> let result = Some before in result
    | letter :: rest ->
      let target = step source state letter in
      let suffix = ghost_ [letter] in
      let next_word = ghost_ (append_word word suffix) in
      let pushed = push_search_state limit target next_word before in
      match pushed with
      | None -> let result = None in result
      | Some updated ->
        ghost_ (state_search_view_def updated);
        let result = expand_search_labels source state word rest
          updated limit in
        result

  let rec (reachable_states_loop @ total) :
      (source : machine) -> (before : state_search) @ total ->
      (processed : (int * int list) list) @ ghost -> (limit : int) ->
      (remaining : {fuel : int |
        let pending, seen, count = state_search_view before in
        state_search_valid before &&
        0 <= fuel && fuel <= 65_536 &&
        0 <= count && count <= limit && limit <= 65_536 &&
        Bigint.of_int count === big_length seen &&
        distinct_ints (states_of_entries seen) &&
        Bigint.add (Bigint.of_int fuel) (Bigint.of_int count) ===
          Bigint.add (Bigint.of_int limit) (big_length pending)}) @ ghost ->
      {result : state_search option |
        let pending, seen, _ = state_search_view before in
        (if valid source && labels_bounded source &&
          access_valid source pending && access_valid source seen &&
          Bigint.compare (state_size source) (Bigint.of_int limit) <= 0 then
          match result with None -> false | Some _ -> true
         else true) &&
        (match result with
         | None -> true
         | Some after -> state_search_valid after &&
           let _, entries, _ = state_search_view after in
           distinct_ints (states_of_entries entries)) &&
        let initial, _ = source in
        if access_valid source pending && access_valid source seen &&
          reach_accounted seen processed pending &&
          all_reach_closed source seen processed &&
          access_member initial seen then
          match result with
          | None -> true
          | Some after -> let _, entries, _ = state_search_view after in
            access_valid source entries &&
            all_reach_closed source entries entries &&
            access_member initial entries
        else true} @ total immutable contended =
    fun source before processed limit remaining ->
    let pending = ghost_ (Access_trace.entries before.pending_access.ghost) in
    let seen = ghost_ (Access_trace.entries before.seen_access.ghost) in
    let count = before.search_count in
    ghost_ (state_search_valid_def before);
    ghost_ (state_search_view_def before);
    ghost_ (states_of_entries_def pending);
    match before.pending_states with
    | [] ->
      ghost_ (reach_accounted_empty_included seen processed);
      ghost_ (reach_closed_processed_covers_seen source seen processed seen);
      let result = Some before in result
    | state :: rest_states ->
      let word = ghost_ (
        match pending with [] -> [] | (_, word) :: _ -> word) in
      let rest = ghost_ (
        match pending with [] -> [] | _ :: rest -> rest) in
      let rest_trace = ghost_ (Access_trace.make rest) in
      let popped = { before with pending_states = rest_states;
        pending_access = { ghost = rest_trace } } in
      ghost_ (state_search_valid_def popped);
      ghost_ (state_search_view_def popped);
      ghost_ (big_length_def pending);
      ghost_ (access_length_nonnegative rest);
      ghost_ (access_valid_def source pending);
      ghost_ (reached_valid source word);
      ghost_ (labels_bounded_state source state);
      let letters = labels source state in
      if list_size letters > 64 then
        let result = None in result
      else
        let zero = 0 in
        let missing_budget = list_size letters + 1 in
        ghost_ (missing_letter_budget letters);
        ghost_ (missing_letter_sound letters zero missing_budget);
        match missing_letter letters zero missing_budget with
        | None -> let result = None in result
        | Some outsider ->
          let new_processed = ghost_ ((state, word) :: processed) in
          ghost_ (reach_accounted_pop seen processed state word rest);
          ghost_ (expand_reachable_labels_accounted source state word letters
            rest seen new_processed count limit);
          ghost_ (expand_reachable_labels_counts source state word letters
            rest seen count limit);
          ghost_ (expand_reachable_labels_distinct source state word letters
            rest seen count limit);
          ghost_ (expand_reachable_labels_seen_included source state word letters
            rest seen count limit);
          ghost_ (expanded_reach_state_closed source state word outsider
            rest seen count limit);
          ghost_ (expand_reachable_labels_valid source state word letters
            rest seen count limit);
          ghost_ (expand_reachable_labels_capacity source state word letters
            rest seen count limit);
          let expanded = expand_search_labels source state word letters
            popped limit in
          (match expanded with
           | None -> let result = None in result
           | Some updated ->
             let updated_pending = ghost_ (Access_trace.entries updated.pending_access.ghost) in
             let updated_seen = ghost_ (Access_trace.entries updated.seen_access.ghost) in
             let updated_count = updated.search_count in
             ghost_ (state_search_view_def updated);
             let target = step source state outsider in
             let suffix = ghost_ [outsider] in
             let next_word = ghost_ (append_word word suffix) in
             ghost_ (reached_push source word outsider);
             ghost_ (push_state_accounted limit target next_word
               updated_pending updated_seen new_processed updated_count);
             ghost_ (push_state_seen_included limit target next_word
               updated_pending updated_seen updated_count);
             ghost_ (push_state_valid source limit target next_word
               updated_pending updated_seen updated_count);
             ghost_ (push_state_counts limit target next_word
               updated_pending updated_seen updated_count);
             ghost_ (push_state_distinct limit target next_word
               updated_pending updated_seen updated_count);
             ghost_ (step_valid source state outsider);
             ghost_ (push_state_capacity source limit target next_word
               updated_pending updated_seen updated_count);
             let pushed = push_search_state limit target next_word updated in
             (match pushed with
              | None -> let result = None in result
              | Some next ->
                let next_seen = ghost_ (Access_trace.entries next.seen_access.ghost) in
                ghost_ (state_search_valid_def next);
                ghost_ (state_search_view_def next);
                ghost_ (access_included_trans seen updated_seen next_seen);
                ghost_ (all_reach_closed_weaken source seen next_seen processed);
                ghost_ (all_reach_closed_def source next_seen new_processed);
                let initial, _ = source in
                ghost_ (access_member_included initial seen next_seen);
                let next_remaining = ghost_ (remaining - 1) in
                let result = reachable_states_loop source next new_processed
                  limit (next_remaining) in
                (match result with
                 | None -> let result = None in result
                 | Some entries ->
                   let result = Some entries in result)))
  [@@decreases let fuel = remaining in fuel]

  let (reachable_states_initial @ total) (source : machine) (limit : int) :
      {result : state_search option |
        (if valid source && labels_bounded source && 0 < limit && limit <= 65_536 &&
          Bigint.compare (state_size source) (Bigint.of_int limit) <= 0 then
          match result with None -> false | Some _ -> true
         else true) &&
        let initial, _ = source in
        match result with
        | None -> true
        | Some after -> state_search_valid after &&
          let _, entries, _ = state_search_view after in
          access_valid source entries &&
          all_reach_closed source entries entries &&
          access_member initial entries &&
          distinct_ints (states_of_entries entries) &&
          (if valid source then
            Bigint.compare (big_length entries) (state_size source) <= 0
           else true)} @ total =
    if limit <= 0 || limit > 65_536 then
      let result = None in result
    else
    let initial, _ = source in
    let empty_word = ghost_ [] in
    let empty_entries = ghost_ ([] : (int * int list) list) in
    let empty_states : int list = [] in
    let zero = 0 in
    let empty_trace = ghost_ (Access_trace.make empty_entries) in
    let start = {
      pending_states = empty_states; seen_states = empty_states;
      search_count = zero;
      pending_access = { ghost = empty_trace };
      seen_access = { ghost = empty_trace };
    } in
    ghost_ (big_length_def empty_entries);
    ghost_ (states_of_entries_def empty_entries);
    ghost_ (state_search_valid_def start);
    ghost_ (state_search_view_def start);
    ghost_ (distinct_ints_def empty_states);
    ghost_ (access_valid_def source empty_entries);
    ghost_ (reach_accounted_empty empty_entries empty_entries);
    ghost_ (reached_empty_equal source);
    ghost_ (reached_valid source empty_word);
    ghost_ (push_state_valid source limit initial empty_word empty_entries
      empty_entries zero);
    ghost_ (push_state_accounted limit initial empty_word empty_entries
      empty_entries empty_entries zero);
    ghost_ (push_state_target_member limit initial empty_word empty_entries
      empty_entries zero);
    ghost_ (push_state_counts limit initial empty_word empty_entries
      empty_entries zero);
    ghost_ (push_state_distinct limit initial empty_word empty_entries empty_entries zero);
    ghost_ (push_state_capacity source limit initial empty_word empty_entries empty_entries zero);
    let pushed = push_search_state limit initial empty_word start in
    match pushed with
    | None -> let result = None in result
    | Some before ->
      let seen = ghost_ (Access_trace.entries before.seen_access.ghost) in
      ghost_ (state_search_view_def before);
      ghost_ (all_reach_closed_empty source seen);
      let result = reachable_states_loop source before empty_entries
        limit (limit) in
      (match result with
       | None -> let result = None in result
       | Some after ->
         let entries = ghost_ (Access_trace.entries after.seen_access.ghost) in
         ghost_ (state_search_view_def after);
         ghost_ (access_count_bound source entries);
         let result = Some after in result)

  let[@def] rec partition_class partition state =
    match partition with
    | [] -> min_int
    | (candidate, class_id) :: rest ->
      if equal_int candidate state then class_id
      else partition_class rest state

  let[@def] rec same_ints left right =
    match left with
    | [] ->
      (match right with [] -> true | _ :: _ -> false)
    | head_left :: rest_left ->
      (match right with
       | [] -> false
       | head_right :: rest_right ->
         equal_int head_left head_right &&
         same_ints rest_left rest_right)

  let rec (same_ints_correct @ total) :
      (left : int list) -> (right : int list) ->
      {u : unit | same_ints left right === (left === right)}
        @ immutable contended =
    fun left right ->
    ghost_ (same_ints_def left right);
    let u = () in
    match left with
    | [] ->
      (match right with [] | _ :: _ -> u)
    | _ :: rest_left ->
      (match right with
       | [] -> u
       | _ :: rest_right ->
         ghost_ (same_ints_correct rest_left rest_right);
         u)

  let[@def] rec successor_classes source partition alphabet state =
    match alphabet with
    | [] -> []
    | letter :: rest ->
      partition_class partition (step source state letter) ::
      successor_classes source partition rest state

  let rec (successor_classes_equal_member @ total) :
      (source : machine) -> (partition : (int * int) list) ->
      (alphabet : int list) -> (left : int) -> (right : int) ->
      (letter : int) ->
      {u : unit | if successor_classes source partition alphabet left ===
          successor_classes source partition alphabet right &&
        has_letter letter alphabet then
        partition_class partition (step source left letter) ===
          partition_class partition (step source right letter)
        else true} @ immutable contended =
    fun source partition alphabet left right letter ->
    ghost_ (successor_classes_def source partition alphabet left);
    ghost_ (successor_classes_def source partition alphabet right);
    ghost_ (has_letter_def letter alphabet);
    let u = () in
    match alphabet with
    | [] -> u
    | head :: rest ->
      if equal_int letter head then u
      else begin
        ghost_ (successor_classes_equal_member source partition rest
          left right letter);
        u
      end

  let[@def] same_partition_signature source partition alphabet left right =
    equal_int (partition_class partition left)
      (partition_class partition right) &&
    equal_int (partition_class partition (default source left))
      (partition_class partition (default source right)) &&
    same_ints
      (successor_classes source partition alphabet left)
      (successor_classes source partition alphabet right)

  let (same_partition_signature_correct @ total) (source : machine)
      (partition : (int * int) list) (alphabet : int list)
      (left : int) (right : int) :
      {u : unit | same_partition_signature source partition alphabet
          left right ===
        (equal_int (partition_class partition left)
           (partition_class partition right) &&
         equal_int (partition_class partition (default source left))
           (partition_class partition (default source right)) &&
         successor_classes source partition alphabet left ===
           successor_classes source partition alphabet right)} =
    let left_successors =
      successor_classes source partition alphabet left in
    let right_successors =
      successor_classes source partition alphabet right in
    ghost_ (same_ints_correct left_successors right_successors);
    ghost_ (same_partition_signature_def source partition alphabet left right);
    let u = () in u

  let (same_partition_signature_refl @ total) (source : machine)
      (partition : (int * int) list) (alphabet : int list)
      (state : int) :
      {u : unit | same_partition_signature source partition alphabet
        state state} =
    ghost_ (same_partition_signature_correct source partition alphabet state state);
    let u = () in u

  let (same_partition_signature_symm @ total) (source : machine)
      (partition : (int * int) list) (alphabet : int list)
      (left : int) (right : int) :
      {u : unit | if same_partition_signature source partition alphabet
          left right then
        same_partition_signature source partition alphabet right left
        else true} =
    ghost_ (same_partition_signature_correct source partition alphabet left right);
    ghost_ (same_partition_signature_correct source partition alphabet right left);
    let u = () in u

  let (same_partition_signature_trans @ total) (source : machine)
      (partition : (int * int) list) (alphabet : int list)
      (first : int) (second : int) (third : int) :
      {u : unit | if same_partition_signature source partition alphabet
          first second &&
        same_partition_signature source partition alphabet second third then
        same_partition_signature source partition alphabet first third
        else true} =
    ghost_ (same_partition_signature_correct source partition alphabet first second);
    ghost_ (same_partition_signature_correct source partition alphabet second third);
    ghost_ (same_partition_signature_correct source partition alphabet first third);
    let u = () in u

  let[@def] rec first_partition_match source partition alphabet
      state states =
    match states with
    | [] -> min_int
    | head :: rest ->
      if same_partition_signature source partition alphabet state head
      then head
      else first_partition_match source partition alphabet state rest

  let rec (first_partition_match_equiv @ total) :
      (source : machine) -> (partition : (int * int) list) ->
      (alphabet : int list) -> (left : int) -> (right : int) ->
      (states : int list) ->
      {u : unit | if same_partition_signature source partition alphabet
          left right then
        first_partition_match source partition alphabet left states ===
          first_partition_match source partition alphabet right states
        else true} @ immutable contended =
    fun source partition alphabet left right states ->
    ghost_ (first_partition_match_def source partition alphabet left states);
    ghost_ (first_partition_match_def source partition alphabet right states);
    let u = () in
    match states with
    | [] -> u
    | head :: rest ->
      if same_partition_signature source partition alphabet left head
      then begin
        ghost_ (same_partition_signature_symm source partition alphabet left right);
        ghost_ (same_partition_signature_trans source partition alphabet
          right left head);
        u
      end else if same_partition_signature source partition alphabet
          right head then begin
        ghost_ (same_partition_signature_trans source partition alphabet
          left right head);
        u
      end else begin
        ghost_ (first_partition_match_equiv source partition alphabet
          left right rest);
        u
      end

  let rec (first_partition_match_sound @ total) :
      (source : machine) -> (partition : (int * int) list) ->
      (alphabet : int list) -> (state : int) ->
      (states : int list) ->
      {u : unit | if has_letter state states then
        same_partition_signature source partition alphabet state
          (first_partition_match source partition alphabet state states)
        else true} @ immutable contended =
    fun source partition alphabet state states ->
    ghost_ (first_partition_match_def source partition alphabet state states);
    ghost_ (has_letter_def state states);
    let u = () in
    match states with
    | [] -> u
    | head :: rest ->
      ghost_ (same_partition_signature_refl source partition alphabet head);
      if same_partition_signature source partition alphabet state head
      then u
      else begin
        ghost_ (first_partition_match_sound source partition alphabet state rest);
        u
      end

  let rec (first_partition_match_member @ total) :
      (source : machine) -> (partition : (int * int) list) ->
      (alphabet : int list) -> (state : int) ->
      (states : int list) ->
      {u : unit | if has_letter state states then
        has_letter
          (first_partition_match source partition alphabet state states)
          states else true} @ immutable contended =
    fun source partition alphabet state states ->
    ghost_ (first_partition_match_def source partition alphabet state states);
    ghost_ (has_letter_def state states);
    let u = () in
    match states with
    | [] -> u
    | head :: rest ->
      let representative =
        first_partition_match source partition alphabet state states in
      ghost_ (has_letter_def representative states);
      ghost_ (same_partition_signature_refl source partition alphabet head);
      if same_partition_signature source partition alphabet state head
      then u
      else begin
        ghost_ (first_partition_match_member source partition alphabet state rest);
        u
      end

  let (first_partition_match_exact @ total) (source : machine)
      (partition : (int * int) list) (alphabet : int list)
      (states : int list) (left : int) (right : int) :
      {u : unit | if has_letter left states && has_letter right states then
        (equal_int
          (first_partition_match source partition alphabet left states)
          (first_partition_match source partition alphabet right states))
        === same_partition_signature source partition alphabet left right
        else true} =
    let left_match =
      first_partition_match source partition alphabet left states in
    let right_match =
      first_partition_match source partition alphabet right states in
    ghost_ (first_partition_match_sound source partition alphabet left states);
    ghost_ (first_partition_match_sound source partition alphabet right states);
    ghost_ (first_partition_match_equiv source partition alphabet left right states);
    ghost_ (same_partition_signature_symm source partition alphabet
      right right_match);
    ghost_ (same_partition_signature_trans source partition alphabet
      left left_match right);
    ghost_ (same_partition_signature_trans source partition alphabet
      left right_match right);
    let u = () in u

  let[@def] rec refine_partition_rows source partition alphabet
      states remaining =
    match remaining with
    | [] -> []
    | state :: rest ->
      (state, first_partition_match source partition alphabet state states)
      :: refine_partition_rows source partition alphabet states rest

  let rec (refine_partition_rows_lookup @ total) :
      (source : machine) -> (partition : (int * int) list) ->
      (alphabet : int list) -> (states : int list) ->
      (remaining : int list) -> (state : int) ->
      {u : unit | if has_letter state remaining then
        partition_class (refine_partition_rows source partition
          alphabet states remaining) state ===
        first_partition_match source partition alphabet state states
        else true} @ immutable contended =
    fun source partition alphabet states remaining state ->
    ghost_ (refine_partition_rows_def source partition alphabet states remaining);
    ghost_ (has_letter_def state remaining);
    let u = () in
    match remaining with
    | [] -> u
    | head :: rest ->
      let tail = refine_partition_rows source partition alphabet states rest in
      let class_id =
        first_partition_match source partition alphabet head states in
      let rows = (head, class_id) :: tail in
      ghost_ (partition_class_def rows state);
      if equal_int head state then u
      else begin
        ghost_ (refine_partition_rows_lookup source partition alphabet
          states rest state);
        u
      end

  let[@def] refine_partition source partition alphabet states =
    refine_partition_rows source partition alphabet states states

  let (refine_partition_exact @ total) (source : machine)
      (partition : (int * int) list) (alphabet : int list)
      (states : int list) (left : int) (right : int) :
      {u : unit | if has_letter left states && has_letter right states then
        equal_int
          (partition_class (refine_partition source partition
            alphabet states) left)
          (partition_class (refine_partition source partition
            alphabet states) right)
        === same_partition_signature source partition alphabet
          left right else true} =
    ghost_ (refine_partition_def source partition alphabet states);
    ghost_ (refine_partition_rows_lookup source partition alphabet
      states states left);
    ghost_ (refine_partition_rows_lookup source partition alphabet
      states states right);
    ghost_ (first_partition_match_exact source partition alphabet
      states left right);
    let u = () in u

  let (refine_partition_representative @ total) (source : machine)
      (partition : (int * int) list) (alphabet : int list)
      (states : int list) (state : int) :
      {u : unit | if has_letter state states then
        has_letter
          (partition_class
            (refine_partition source partition alphabet states) state)
          states else true} =
    ghost_ (refine_partition_def source partition alphabet states);
    ghost_ (refine_partition_rows_lookup source partition alphabet
      states states state);
    ghost_ (first_partition_match_member source partition alphabet state states);
    let u = () in u

  let (refine_partition_idempotent @ total) (source : machine)
      (partition : (int * int) list) (alphabet : int list)
      (states : int list) (state : int) :
      {u : unit | if has_letter state states then
        let refined = refine_partition source partition alphabet states in
        let representative = partition_class refined state in
        partition_class refined representative === representative
        else true} =
    let refined = refine_partition source partition alphabet states in
    let representative = partition_class refined state in
    ghost_ (refine_partition_representative source partition alphabet
      states state);
    ghost_ (refine_partition_def source partition alphabet states);
    ghost_ (refine_partition_rows_lookup source partition alphabet
      states states state);
    ghost_ (refine_partition_rows_lookup source partition alphabet
      states states representative);
    ghost_ (first_partition_match_sound source partition alphabet state states);
    ghost_ (first_partition_match_equiv source partition alphabet
      state representative states);
    let u = () in u

  let (refine_partition_refines @ total) (source : machine)
      (partition : (int * int) list) (alphabet : int list)
      (states : int list) (left : int) (right : int) :
      {u : unit | if has_letter left states && has_letter right states &&
        equal_int
          (partition_class (refine_partition source partition
            alphabet states) left)
          (partition_class (refine_partition source partition
            alphabet states) right) then
        partition_class partition left === partition_class partition right
        else true} =
    ghost_ (refine_partition_exact source partition alphabet states left right);
    ghost_ (same_partition_signature_correct source partition alphabet left right);
    let u = () in u

  let (refine_partition_default @ total) (source : machine)
      (partition : (int * int) list) (alphabet : int list)
      (states : int list) (left : int) (right : int) :
      {u : unit | if has_letter left states && has_letter right states &&
        equal_int
          (partition_class (refine_partition source partition
            alphabet states) left)
          (partition_class (refine_partition source partition
            alphabet states) right) then
        partition_class partition (default source left) ===
          partition_class partition (default source right)
        else true} =
    ghost_ (refine_partition_exact source partition alphabet states left right);
    ghost_ (same_partition_signature_correct source partition alphabet left right);
    let u = () in u

  let (refine_partition_label @ total) (source : machine)
      (partition : (int * int) list) (alphabet : int list)
      (states : int list) (left : int) (right : int)
      (letter : int) :
      {u : unit | if has_letter left states && has_letter right states &&
        has_letter letter alphabet &&
        equal_int
          (partition_class (refine_partition source partition
            alphabet states) left)
          (partition_class (refine_partition source partition
            alphabet states) right) then
        partition_class partition (step source left letter) ===
          partition_class partition (step source right letter)
        else true} =
    ghost_ (refine_partition_exact source partition alphabet states left right);
    ghost_ (same_partition_signature_correct source partition alphabet left right);
    ghost_ (successor_classes_equal_member source partition alphabet
      left right letter);
    let u = () in u

  let[@def] rec initial_partition source states =
    match states with
    | [] -> []
    | state :: rest ->
      (state, if final source state then 1 else 0) ::
      initial_partition source rest

  let rec (initial_partition_lookup @ total) :
      (source : machine) -> (states : int list) -> (state : int) ->
      {u : unit | if has_letter state states then
        partition_class (initial_partition source states) state ===
          (if final source state then 1 else 0)
        else true} @ immutable contended =
    fun source states state ->
    ghost_ (initial_partition_def source states);
    ghost_ (has_letter_def state states);
    let u = () in
    match states with
    | [] -> u
    | head :: rest ->
      let tail = initial_partition source rest in
      let rows = (head, if final source head then 1 else 0) :: tail in
      ghost_ (partition_class_def rows state);
      if equal_int head state then u
      else begin
        ghost_ (initial_partition_lookup source rest state);
        u
      end

  let[@def] rec partition_respects partition states pairs =
    match pairs with
    | [] -> true
    | (left, right) :: rest ->
      (if has_letter left states && has_letter right states then
        partition_class partition left = partition_class partition right
       else true) && partition_respects partition states rest

  let rec (partition_respects_member @ total) :
      (partition : relation) -> (states : int list) ->
      (pairs : relation) -> (left : int) -> (right : int) ->
      {u : unit | if partition_respects partition states pairs &&
        related (left, right) pairs && has_letter left states &&
        has_letter right states then
        partition_class partition left === partition_class partition right
        else true} @ immutable contended =
    fun partition states pairs left right ->
    ghost_ (partition_respects_def partition states pairs);
    let pair = left, right in
    ghost_ (related_def pair pairs);
    let u = () in
    match pairs with
    | [] -> u
    | head :: rest ->
      ghost_ (same_pair_correct pair head);
      ghost_ (partition_respects_member partition states rest left right);
      u

  let rec (initial_partition_respects @ total) :
      (source : machine) -> (states : int list) ->
      (relation : relation) -> (pairs : relation) ->
      {u : unit | if all_closed source source relation pairs then
        partition_respects (initial_partition source states) states pairs
        else true} @ immutable contended =
    fun source states relation pairs ->
    let partition = initial_partition source states in
    ghost_ (partition_respects_def partition states pairs);
    ghost_ (all_closed_def source source relation pairs);
    let u = () in
    match pairs with
    | [] -> u
    | (left, right) :: rest ->
      ghost_ (pair_closed_def source source left right relation);
      ghost_ (initial_partition_lookup source states left);
      ghost_ (initial_partition_lookup source states right);
      ghost_ (initial_partition_respects source states relation rest);
      u

  let rec (related_successor_classes @ total) :
      (source : machine) -> (entries : (int * int list) list) ->
      (partition : relation) -> (relation : relation) ->
      (alphabet : int list) -> (left : int) -> (right : int) ->
      {u : unit | let states = states_of_entries entries in
        if all_reach_closed source entries entries &&
          partition_respects partition states relation &&
          pair_closed source source left right relation &&
          access_member left entries && access_member right entries then
          successor_classes source partition alphabet left ===
            successor_classes source partition alphabet right
        else true} @ immutable contended =
    fun source entries partition relation alphabet left right ->
    let states = states_of_entries entries in
    ghost_ (successor_classes_def source partition alphabet left);
    ghost_ (successor_classes_def source partition alphabet right);
    let u = () in
    match alphabet with
    | [] -> u
    | letter :: rest ->
      let next_left = step source left letter in
      let next_right = step source right letter in
      ghost_ (all_reach_closed_member source entries entries left);
      ghost_ (all_reach_closed_member source entries entries right);
      ghost_ (reach_state_closed_step source entries left letter);
      ghost_ (reach_state_closed_step source entries right letter);
      ghost_ (states_of_entries_member entries next_left);
      ghost_ (states_of_entries_member entries next_right);
      ghost_ (closed_step source source relation left right letter);
      ghost_ (partition_respects_member partition states relation next_left next_right);
      ghost_ (related_successor_classes source entries partition relation rest
        left right);
      u

  let rec (refine_partition_respects @ total) :
      (source : machine) -> (entries : (int * int list) list) ->
      (partition : relation) -> (relation : relation) ->
      (pairs : relation) -> (alphabet : int list) ->
      {u : unit | let states = states_of_entries entries in
        if all_reach_closed source entries entries &&
          partition_respects partition states relation &&
          all_closed source source relation pairs &&
          relation_included pairs relation then
          partition_respects
            (refine_partition source partition alphabet states) states pairs
        else true} @ immutable contended =
    fun source entries partition relation pairs alphabet ->
    let states = states_of_entries entries in
    let next = refine_partition source partition alphabet states in
    ghost_ (partition_respects_def next states pairs);
    ghost_ (all_closed_def source source relation pairs);
    ghost_ (relation_included_def pairs relation);
    let u = () in
    match pairs with
    | [] -> u
    | (left, right) :: rest ->
      ghost_ (states_of_entries_member entries left);
      ghost_ (states_of_entries_member entries right);
      ghost_ (all_reach_closed_member source entries entries left);
      ghost_ (all_reach_closed_member source entries entries right);
      ghost_ (reach_state_closed_def source entries left);
      ghost_ (reach_state_closed_def source entries right);
      let default_left = default source left in
      let default_right = default source right in
      ghost_ (states_of_entries_member entries default_left);
      ghost_ (states_of_entries_member entries default_right);
      ghost_ (pair_closed_def source source left right relation);
      ghost_ (partition_respects_member partition states relation left right);
      ghost_ (partition_respects_member partition states relation
        default_left default_right);
      ghost_ (related_successor_classes source entries partition relation alphabet
        left right);
      ghost_ (same_partition_signature_correct source partition alphabet left right);
      ghost_ (refine_partition_exact source partition alphabet states left right);
      ghost_ (refine_partition_respects source entries partition relation rest alphabet);
      u

  let (distinguish_classes @ total) (source : machine)
      (states : int list) (partition : relation)
      (respect : ((relation : relation) ->
        {u : unit | if all_closed source source relation relation then
          partition_respects partition states relation else true}) @ total)
      (p : int) (q : int) (limit : int) :
      {result : int list option |
        (match result with
         | None -> true
         | Some word -> run_from source p word <> run_from source q word) &&
        (if valid source && has_state source p && has_state source q &&
          labels_bounded source && has_letter p states && has_letter q states &&
          partition_class partition p <> partition_class partition q &&
          0 < limit && limit <= 65_536 &&
          Bigint.compare (Bigint.mul (state_size source) (state_size source))
            (Bigint.of_int limit) <= 0 then
          match result with None -> false | Some _ -> true else true)} =
    let decision = compare_states source p q limit in
    match decision with
    | Limit -> let result = None in result
    | Equal relation ->
      ghost_ (respect relation);
      ghost_ (partition_respects_member partition states relation p q);
      let result = None in result
    | Different word -> let result = Some word in result

  let (initial_partition_final @ total) (source : machine)
      (states : int list) (left : int) (right : int) :
      {u : unit | if has_letter left states &&
        has_letter right states &&
        equal_int (partition_class (initial_partition source states) left)
          (partition_class (initial_partition source states) right) then
        final source left === final source right else true} =
    ghost_ (initial_partition_lookup source states left);
    ghost_ (initial_partition_lookup source states right);
    let u = () in u

  let[@def] rec accepting_row source partition state others =
    match others with
    | [] -> true
    | other :: rest ->
      (if equal_int (partition_class partition state)
          (partition_class partition other) then
        final source state = final source other else true) &&
      accepting_row source partition state rest

  let[@def] rec accepting_rows source partition states remaining =
    match remaining with
    | [] -> true
    | state :: rest ->
      accepting_row source partition state states &&
      accepting_rows source partition states rest

  let[@def] accepting_partition source partition states =
    accepting_rows source partition states states

  let rec (accepting_row_member @ total) :
      (source : machine) -> (partition : (int * int) list) ->
      (state : int) -> (others : int list) -> (other : int) ->
      {u : unit | if accepting_row source partition state others &&
        has_letter other others &&
        equal_int (partition_class partition state)
          (partition_class partition other) then
        final source state === final source other else true}
        @ immutable contended =
    fun source partition state others other ->
    ghost_ (accepting_row_def source partition state others);
    ghost_ (has_letter_def other others);
    let u = () in
    match others with
    | [] -> u
    | _ :: rest ->
      ghost_ (accepting_row_member source partition state rest other);
      u

  let rec (accepting_rows_member @ total) :
      (source : machine) -> (partition : (int * int) list) ->
      (states : int list) -> (remaining : int list) ->
      (state : int) ->
      {u : unit | if accepting_rows source partition states remaining &&
        has_letter state remaining then
        accepting_row source partition state states else true}
        @ immutable contended =
    fun source partition states remaining state ->
    ghost_ (accepting_rows_def source partition states remaining);
    ghost_ (has_letter_def state remaining);
    let u = () in
    match remaining with
    | [] -> u
    | _ :: rest ->
      ghost_ (accepting_rows_member source partition states rest state);
      u

  let (accepting_partition_pair @ total) (source : machine)
      (partition : (int * int) list) (states : int list)
      (left : int) (right : int) :
      {u : unit | if accepting_partition source partition states &&
        has_letter left states && has_letter right states &&
        equal_int (partition_class partition left)
          (partition_class partition right) then
        final source left === final source right else true} =
    ghost_ (accepting_partition_def source partition states);
    ghost_ (accepting_rows_member source partition states states left);
    ghost_ (accepting_row_member source partition left states right);
    let u = () in u

  let[@def] rec letters_in subset states =
    match subset with
    | [] -> true
    | state :: rest ->
      has_letter state states && letters_in rest states

  let (letters_in_tail @ total) :
      (subset : int list) -> (states : int list) ->
      {u : unit | if letters_in subset states then
        match subset with
        | [] -> true
        | _ :: rest -> letters_in rest states
        else true} @ immutable contended =
    fun subset states ->
    ghost_ (letters_in_def subset states);
    let u = () in u

  let rec (letters_in_weaken @ total) :
      (subset : int list) -> (states : int list) -> (head : int) ->
      {u : unit | if letters_in subset states then
        letters_in subset (head :: states) else true}
        @ immutable contended =
    fun subset states head ->
    let extended = head :: states in
    ghost_ (letters_in_def subset states);
    ghost_ (letters_in_def subset extended);
    let u = () in
    match subset with
    | [] -> u
    | state :: rest ->
      ghost_ (has_letter_def state extended);
      ghost_ (letters_in_weaken rest states head);
      u

  let rec (letters_in_refl @ total) :
      (states : int list) ->
      {u : unit | letters_in states states}
        @ immutable contended =
    fun states ->
    ghost_ (letters_in_def states states);
    let u = () in
    match states with
    | [] -> u
    | head :: rest ->
      ghost_ (has_letter_def head states);
      ghost_ (letters_in_refl rest);
      ghost_ (letters_in_weaken rest rest head);
      u

  let rec (initial_accepting_row @ total) :
      (source : machine) -> (states : int list) ->
      (left : int) -> (others : int list) ->
      {u : unit | if has_letter left states &&
        letters_in others states then
        accepting_row source (initial_partition source states)
          left others else true} @ immutable contended =
    fun source states left others ->
    let initial = initial_partition source states in
    ghost_ (accepting_row_def source initial left others);
    ghost_ (letters_in_def others states);
    let u = () in
    match others with
    | [] -> u
    | right :: rest ->
      ghost_ (initial_partition_final source states left right);
      ghost_ (letters_in_tail others states);
      ghost_ (initial_accepting_row source states left rest);
      u

  let rec (initial_accepting_rows @ total) :
      (source : machine) -> (states : int list) ->
      (remaining : int list) ->
      {u : unit | if letters_in remaining states then
        accepting_rows source
        (initial_partition source states) states remaining else true}
        @ immutable contended =
    fun source states remaining ->
    let initial = initial_partition source states in
    ghost_ (accepting_rows_def source initial states remaining);
    ghost_ (letters_in_def remaining states);
    let u = () in
    match remaining with
    | [] -> u
    | state :: rest ->
      ghost_ (letters_in_refl states);
      ghost_ (initial_accepting_row source states state states);
      ghost_ (letters_in_tail remaining states);
      ghost_ (initial_accepting_rows source states rest);
      u

  let (initial_accepting_partition @ total) (source : machine)
      (states : int list) :
      {u : unit | accepting_partition source
        (initial_partition source states) states} =
    ghost_ (letters_in_refl states);
    ghost_ (initial_accepting_rows source states states);
    let initial = initial_partition source states in
    ghost_ (accepting_partition_def source initial states);
    let u = () in u

  let rec (refine_accepting_row @ total) :
      (source : machine) -> (partition : (int * int) list) ->
      (alphabet : int list) -> (states : int list) ->
      (left : int) -> (others : int list) ->
      {u : unit | if accepting_partition source partition states &&
        has_letter left states && letters_in others states then
        accepting_row source
          (refine_partition source partition alphabet states)
          left others else true} @ immutable contended =
    fun source partition alphabet states left others ->
    let next = refine_partition source partition alphabet states in
    ghost_ (accepting_row_def source next left others);
    ghost_ (letters_in_def others states);
    let u = () in
    match others with
    | [] -> u
    | right :: rest ->
      ghost_ (refine_partition_refines source partition alphabet states left right);
      ghost_ (accepting_partition_pair source partition states left right);
      ghost_ (letters_in_tail others states);
      ghost_ (refine_accepting_row source partition alphabet states left rest);
      u

  let rec (refine_accepting_rows @ total) :
      (source : machine) -> (partition : (int * int) list) ->
      (alphabet : int list) -> (states : int list) ->
      (remaining : int list) ->
      {u : unit | if accepting_partition source partition states &&
        letters_in remaining states then
        accepting_rows source
          (refine_partition source partition alphabet states)
          states remaining else true} @ immutable contended =
    fun source partition alphabet states remaining ->
    let next = refine_partition source partition alphabet states in
    ghost_ (accepting_rows_def source next states remaining);
    ghost_ (letters_in_def remaining states);
    let u = () in
    match remaining with
    | [] -> u
    | state :: rest ->
      ghost_ (letters_in_refl states);
      ghost_ (refine_accepting_row source partition alphabet states state states);
      ghost_ (letters_in_tail remaining states);
      ghost_ (refine_accepting_rows source partition alphabet states rest);
      u

  let (refine_accepting_partition @ total) (source : machine)
      (partition : (int * int) list) (alphabet : int list)
      (states : int list) :
      {u : unit | if accepting_partition source partition states then
        accepting_partition source
          (refine_partition source partition alphabet states) states
        else true} =
    ghost_ (letters_in_refl states);
    ghost_ (refine_accepting_rows source partition alphabet states states);
    let next = refine_partition source partition alphabet states in
    ghost_ (accepting_partition_def source next states);
    let u = () in u

  let[@def] insert_letter letter alphabet =
    if has_letter letter alphabet then alphabet else letter :: alphabet

  let (insert_letter_member @ total) (letter : int)
      (alphabet : int list) (query : int) :
      {u : unit | has_letter query (insert_letter letter alphabet) ===
        (equal_int query letter || has_letter query alphabet)} =
    ghost_ (insert_letter_def letter alphabet);
    let inserted = insert_letter letter alphabet in
    ghost_ (has_letter_def query inserted);
    let u = () in u

  let (insert_letter_distinct @ total) (letter : int)
      (alphabet : int list) :
      {u : unit | if distinct_ints alphabet then
        distinct_ints (insert_letter letter alphabet) else true} =
    ghost_ (insert_letter_def letter alphabet);
    let inserted = insert_letter letter alphabet in
    ghost_ (distinct_ints_def inserted);
    let u = () in u

  let[@def] rec (quotient_access @ total) partition entries =
    match entries with
    | [] -> []
    | (state, word) :: rest ->
      (partition_class partition state, word) ::
      quotient_access partition rest

  let[@def] rec (collect_classes @ total) :
      (int * int) list -> int list -> int list @ immutable contended =
    fun partition states ->
    match states with
    | [] -> []
    | state :: rest ->
      insert_letter (partition_class partition state)
        (collect_classes partition rest)

  let rec (collect_classes_member @ total) :
      (partition : (int * int) list) -> (states : int list) ->
      (state : int) ->
      {u : unit | if has_letter state states then
        has_letter (partition_class partition state)
          (collect_classes partition states)
        else true} @ immutable contended =
    fun partition states state ->
    ghost_ (collect_classes_def partition states);
    ghost_ (has_letter_def state states);
    let u = () in
    match states with
    | [] -> u
    | head :: rest ->
      let tail = collect_classes partition rest in
      let head_class = partition_class partition head in
      let state_class = partition_class partition state in
      ghost_ (insert_letter_member head_class tail state_class);
      ghost_ (collect_classes_member partition rest state);
      u

  let rec (collect_classes_distinct @ total) :
      (partition : (int * int) list) -> (states : int list) ->
      {u : unit | distinct_ints (collect_classes partition states)}
        @ immutable contended =
    fun partition states ->
    ghost_ (collect_classes_def partition states);
    let u = () in
    match states with
    | [] ->
      let empty = collect_classes partition states in
      ghost_ (distinct_ints_def empty);
      u
    | state :: rest ->
      ghost_ (collect_classes_distinct partition rest);
      let tail = collect_classes partition rest in
      let class_id = partition_class partition state in
      ghost_ (insert_letter_distinct class_id tail);
      u

  let rec (collect_classes_representatives @ total) :
      (source : machine) -> (previous : (int * int) list) ->
      (alphabet : int list) -> (states : int list) ->
      (remaining : int list) -> (class_id : int) ->
      {u : unit | let stable =
          refine_partition source previous alphabet states in
        if letters_in remaining states &&
          has_letter class_id (collect_classes stable remaining) then
          has_letter class_id states else true} @ immutable contended =
    fun source previous alphabet states remaining class_id ->
    let stable = refine_partition source previous alphabet states in
    ghost_ (collect_classes_def stable remaining);
    ghost_ (letters_in_def remaining states);
    let u = () in
    match remaining with
    | [] ->
      let empty_classes = collect_classes stable remaining in
      ghost_ (has_letter_def class_id empty_classes);
      u
    | head :: rest ->
      let head_class = partition_class stable head in
      let tail = collect_classes stable rest in
      ghost_ (insert_letter_member head_class tail class_id);
      ghost_ (refine_partition_representative source previous alphabet
        states head);
      ghost_ (letters_in_tail remaining states);
      ghost_ (collect_classes_representatives source previous alphabet
        states rest class_id);
      u

  let rec (collect_classes_fixed @ total) :
      (source : machine) -> (previous : (int * int) list) ->
      (alphabet : int list) -> (states : int list) ->
      (remaining : int list) -> (class_id : int) ->
      {u : unit | let stable =
          refine_partition source previous alphabet states in
        if letters_in remaining states &&
          has_letter class_id (collect_classes stable remaining) then
          partition_class stable class_id === class_id else true}
        @ immutable contended =
    fun source previous alphabet states remaining class_id ->
    let stable = refine_partition source previous alphabet states in
    ghost_ (collect_classes_def stable remaining);
    ghost_ (letters_in_def remaining states);
    let u = () in
    match remaining with
    | [] ->
      let empty_classes = collect_classes stable remaining in
      ghost_ (has_letter_def class_id empty_classes);
      u
    | head :: rest ->
      let head_class = partition_class stable head in
      let tail = collect_classes stable rest in
      ghost_ (insert_letter_member head_class tail class_id);
      ghost_ (refine_partition_idempotent source previous alphabet states head);
      ghost_ (letters_in_tail remaining states);
      ghost_ (collect_classes_fixed source previous alphabet states rest class_id);
      u

  let rec (quotient_access_member @ total) :
      (partition : (int * int) list) ->
      (entries : (int * int list) list) -> (class_id : int) ->
      {u : unit | access_member class_id
          (quotient_access partition entries) ===
        has_letter class_id
          (collect_classes partition (states_of_entries entries))}
        @ immutable contended =
    fun partition entries class_id ->
    ghost_ (quotient_access_def partition entries);
    ghost_ (states_of_entries_def entries);
    let access = quotient_access partition entries in
    let states = states_of_entries entries in
    let classes = collect_classes partition states in
    ghost_ (access_member_def class_id access);
    ghost_ (collect_classes_def partition states);
    let u = () in
    match entries with
    | [] ->
      ghost_ (has_letter_def class_id classes);
      u
    | (state, _) :: rest ->
      let head_class = partition_class partition state in
      let tail_states = states_of_entries rest in
      let tail_classes = collect_classes partition tail_states in
      ghost_ (insert_letter_member head_class tail_classes class_id);
      ghost_ (quotient_access_member partition rest class_id);
      u

  let rec (quotient_access_word_source @ total) :
      (source : machine) -> (partition : (int * int) list) ->
      (entries : (int * int list) list) -> (class_id : int) ->
      {u : unit | if access_valid source entries then
        match access_word class_id (quotient_access partition entries) with
        | None -> true
        | Some word ->
          partition_class partition (reached source word) === class_id
        else true} @ immutable contended =
    fun source partition entries class_id ->
    ghost_ (quotient_access_def partition entries);
    ghost_ (access_valid_def source entries);
    let access = quotient_access partition entries in
    ghost_ (access_word_def class_id access);
    let u = () in
    match entries with
    | [] -> u
    | (state, _) :: rest ->
      ghost_ (quotient_access_word_source source partition rest class_id);
      u

  let[@def] rec quotient_edges source partition representative alphabet =
    match alphabet with
    | [] -> []
    | letter :: rest ->
      (letter, partition_class partition
        (step source representative letter)) ::
      quotient_edges source partition representative rest

  let rec (quotient_edges_labels @ total) :
      (source : machine) -> (partition : (int * int) list) ->
      (representative : int) -> (alphabet : int list) ->
      (letter : int) ->
      {u : unit | has_label letter
        (quotient_edges source partition representative alphabet) ===
        has_letter letter alphabet} @ immutable contended =
    fun source partition representative alphabet letter ->
    ghost_ (quotient_edges_def source partition representative alphabet);
    ghost_ (has_letter_def letter alphabet);
    let edges = quotient_edges source partition representative alphabet in
    ghost_ (has_label_def letter edges);
    let u = () in
    match alphabet with
    | [] -> u
    | _ :: rest ->
      ghost_ (quotient_edges_labels source partition representative rest letter);
      u

  let rec (quotient_edges_unique @ total) :
      (source : machine) -> (partition : (int * int) list) ->
      (representative : int) -> (alphabet : int list) ->
      {u : unit | if distinct_ints alphabet then
        unique_labels (quotient_edges source partition
          representative alphabet) else true} @ immutable contended =
    fun source partition representative alphabet ->
    ghost_ (quotient_edges_def source partition representative alphabet);
    ghost_ (distinct_ints_def alphabet);
    let edges = quotient_edges source partition representative alphabet in
    ghost_ (unique_labels_def edges);
    let u = () in
    match alphabet with
    | [] -> u
    | letter :: rest ->
      ghost_ (quotient_edges_labels source partition representative rest letter);
      ghost_ (quotient_edges_unique source partition representative rest);
      u

  let rec (quotient_edges_step @ total) :
      (source : machine) -> (partition : (int * int) list) ->
      (representative : int) -> (alphabet : int list) ->
      (fallback : int) -> (letter : int) ->
      {u : unit | edge_step
        (quotient_edges source partition representative alphabet)
        fallback letter ===
        (if has_letter letter alphabet then
           partition_class partition (step source representative letter)
         else fallback)} @ immutable contended =
    fun source partition representative alphabet fallback letter ->
    ghost_ (quotient_edges_def source partition representative alphabet);
    ghost_ (has_letter_def letter alphabet);
    let edges = quotient_edges source partition representative alphabet in
    ghost_ (edge_step_def edges fallback letter);
    let u = () in
    match alphabet with
    | [] -> u
    | head :: rest ->
      if equal_int letter head then u
      else begin
        ghost_ (quotient_edges_step source partition representative
          rest fallback letter);
        u
      end

  let[@def] rec quotient_table source partition alphabet classes =
    match classes with
    | [] -> []
    | representative :: rest ->
      (representative, final source representative,
        (quotient_edges source partition representative alphabet,
         partition_class partition (default source representative))) ::
      quotient_table source partition alphabet rest

  let rec (quotient_state_ids @ total) :
      (source : machine) -> (partition : (int * int) list) ->
      (alphabet : int list) -> (classes : int list) ->
      {u : unit | state_ids
        (quotient_table source partition alphabet classes) === classes}
        @ immutable contended =
    fun source partition alphabet classes ->
    ghost_ (quotient_table_def source partition alphabet classes);
    let table = quotient_table source partition alphabet classes in
    ghost_ (state_ids_def table);
    let u = () in
    match classes with
    | [] -> u
    | _ :: rest ->
      ghost_ (quotient_state_ids source partition alphabet rest);
      u

  let rec (quotient_table_keys @ total) :
      (source : machine) -> (partition : (int * int) list) ->
      (alphabet : int list) -> (classes : int list) -> (key : int) ->
      {u : unit | has_key key
        (quotient_table source partition alphabet classes) ===
        has_letter key classes} @ immutable contended =
    fun source partition alphabet classes key ->
    ghost_ (quotient_table_def source partition alphabet classes);
    ghost_ (has_letter_def key classes);
    let table = quotient_table source partition alphabet classes in
    ghost_ (has_key_def key table);
    let u = () in
    match classes with
    | [] -> u
    | _ :: rest ->
      ghost_ (quotient_table_keys source partition alphabet rest key);
      u

  let rec (quotient_edges_targets @ total) :
      (source : machine) -> (entries : (int * int list) list) ->
      (partition : (int * int) list) -> (representative : int) ->
      (alphabet : int list) -> (remaining : int list) ->
      {u : unit | let states = states_of_entries entries in
        let classes = collect_classes partition states in
        let table = quotient_table source partition alphabet classes in
        if all_reach_closed source entries entries &&
          access_member representative entries then
          targets_valid table
            (quotient_edges source partition representative remaining)
        else true} @ immutable contended =
    fun source entries partition representative alphabet remaining ->
    let states = states_of_entries entries in
    let classes = collect_classes partition states in
    let table = quotient_table source partition alphabet classes in
    let edges = quotient_edges source partition representative remaining in
    ghost_ (quotient_edges_def source partition representative remaining);
    ghost_ (targets_valid_def table edges);
    let u = () in
    match remaining with
    | [] -> u
    | letter :: rest ->
      let target = step source representative letter in
      let target_class = partition_class partition target in
      ghost_ (all_reach_closed_member source entries entries representative);
      ghost_ (reach_state_closed_step source entries representative letter);
      ghost_ (states_of_entries_member entries target);
      ghost_ (collect_classes_member partition states target);
      ghost_ (quotient_table_keys source partition alphabet classes target_class);
      ghost_ (quotient_edges_targets source entries partition representative
        alphabet rest);
      u

  let rec (quotient_table_unique @ total) :
      (source : machine) -> (partition : (int * int) list) ->
      (alphabet : int list) -> (classes : int list) ->
      {u : unit | if distinct_ints classes then
        unique_keys (quotient_table source partition alphabet classes)
        else true} @ immutable contended =
    fun source partition alphabet classes ->
    ghost_ (quotient_table_def source partition alphabet classes);
    ghost_ (distinct_ints_def classes);
    let table = quotient_table source partition alphabet classes in
    ghost_ (unique_keys_def table);
    let u = () in
    match classes with
    | [] -> u
    | class_id :: rest ->
      ghost_ (quotient_table_keys source partition alphabet rest class_id);
      ghost_ (quotient_table_unique source partition alphabet rest);
      u

  let rec (quotient_table_view @ total) :
      (source : machine) -> (partition : (int * int) list) ->
      (alphabet : int list) -> (classes : int list) ->
      (representative : int) ->
      {u : unit | if has_letter representative classes then
        view (quotient_table source partition alphabet classes)
          representative ===
        (final source representative,
         (quotient_edges source partition representative alphabet,
          partition_class partition (default source representative)))
        else true} @ immutable contended =
    fun source partition alphabet classes representative ->
    ghost_ (quotient_table_def source partition alphabet classes);
    ghost_ (has_letter_def representative classes);
    let table = quotient_table source partition alphabet classes in
    ghost_ (view_def table representative);
    let u = () in
    match classes with
    | [] -> u
    | _ :: rest ->
      ghost_ (quotient_table_view source partition alphabet rest representative);
      u

  let[@def] rec add_letters letters alphabet =
    match letters with
    | [] -> alphabet
    | letter :: rest ->
      insert_letter letter (add_letters rest alphabet)

  let rec (add_letters_member @ total) :
      (letters : int list) -> (alphabet : int list) -> (query : int) ->
      {u : unit | has_letter query (add_letters letters alphabet) ===
        (has_letter query letters || has_letter query alphabet)}
        @ immutable contended =
    fun letters alphabet query ->
    ghost_ (add_letters_def letters alphabet);
    ghost_ (has_letter_def query letters);
    let u = () in
    match letters with
    | [] -> u
    | letter :: rest ->
      ghost_ (add_letters_member rest alphabet query);
      let tail_alphabet = add_letters rest alphabet in
      ghost_ (insert_letter_member letter tail_alphabet query);
      u

  let rec (add_letters_distinct @ total) :
      (letters : int list) -> (alphabet : int list) ->
      {u : unit | if distinct_ints alphabet then
        distinct_ints (add_letters letters alphabet) else true}
        @ immutable contended =
    fun letters alphabet ->
    ghost_ (add_letters_def letters alphabet);
    let u = () in
    match letters with
    | [] -> u
    | letter :: rest ->
      ghost_ (add_letters_distinct rest alphabet);
      let tail = add_letters rest alphabet in
      ghost_ (insert_letter_distinct letter tail);
      u

  let[@def] rec collect_alphabet source states =
    match states with
    | [] -> []
    | state :: rest ->
      add_letters (labels source state) (collect_alphabet source rest)

  let rec (collect_alphabet_member @ total) :
      (source : machine) -> (states : int list) ->
      (state : int) -> (letter : int) ->
      {u : unit | if has_letter state states &&
        has_letter letter (labels source state) then
        has_letter letter (collect_alphabet source states)
        else true} @ immutable contended =
    fun source states state letter ->
    ghost_ (collect_alphabet_def source states);
    ghost_ (has_letter_def state states);
    let u = () in
    match states with
    | [] -> u
    | head :: rest ->
      ghost_ (collect_alphabet_member source rest state letter);
      let head_labels = labels source head in
      let tail_alphabet = collect_alphabet source rest in
      ghost_ (add_letters_member head_labels tail_alphabet letter);
      u

  let rec (collect_alphabet_distinct @ total) :
      (source : machine) -> (states : int list) ->
      {u : unit | distinct_ints (collect_alphabet source states)}
        @ immutable contended =
    fun source states ->
    ghost_ (collect_alphabet_def source states);
    let u = () in
    match states with
    | [] ->
      let alphabet = collect_alphabet source states in
      ghost_ (distinct_ints_def alphabet);
      u
    | state :: rest ->
      ghost_ (collect_alphabet_distinct source rest);
      let tail = collect_alphabet source rest in
      let row_labels = labels source state in
      ghost_ (add_letters_distinct row_labels tail);
      u

  let (quotient_row_step @ total) (source : machine)
      (partition : (int * int) list) (states : int list)
      (representative : int) (letter : int) :
      {u : unit | let alphabet = collect_alphabet source states in
        let row =
          (quotient_edges source partition representative alphabet,
           partition_class partition (default source representative)) in
        if has_letter representative states then
          row_step row letter ===
            partition_class partition
              (step source representative letter)
        else true} =
    let alphabet = collect_alphabet source states in
    let fallback =
      partition_class partition (default source representative) in
    let edges = quotient_edges source partition representative alphabet in
    let row = edges, fallback in
    ghost_ (row_step_def row letter);
    ghost_ (quotient_edges_step source partition representative alphabet
      fallback letter);
    let u = () in
    if has_letter letter alphabet then u
    else begin
      ghost_ (collect_alphabet_member source states representative letter);
      ghost_ (step_outside source representative letter);
      u
    end

  let[@def] quotient_raw source partition states classes initial =
    partition_class partition initial,
    quotient_table source partition
      (collect_alphabet source states) classes

  let[@def] rec (quotient_relation @ total) partition states =
    match states with
    | [] -> []
    | state :: rest ->
      (state, partition_class partition state) ::
      quotient_relation partition rest

  let rec (quotient_related @ total) :
      (partition : (int * int) list) -> (states : int list) ->
      (state : int) ->
      {u : unit | if has_letter state states then
        related (state, partition_class partition state)
          (quotient_relation partition states) else true}
        @ immutable contended =
    fun partition states state ->
    ghost_ (quotient_relation_def partition states);
    ghost_ (has_letter_def state states);
    let relation = quotient_relation partition states in
    let pair = state, partition_class partition state in
    ghost_ (related_def pair relation);
    let u = () in
    match states with
    | [] -> u
    | head :: rest ->
      let head_pair = head, partition_class partition head in
      ghost_ (same_pair_def pair head_pair);
      ghost_ (quotient_related partition rest state);
      u

  let (quotient_final @ total) (source : machine)
      (partition : (int * int) list) (states : int list)
      (classes : int list) (initial : int) (representative : int) :
      {u : unit | if has_letter representative classes then
        final (quotient_raw source partition states classes initial)
          representative === final source representative else true} =
    let alphabet = collect_alphabet source states in
    let raw = quotient_raw source partition states classes initial in
    ghost_ (quotient_raw_def source partition states classes initial);
    ghost_ (quotient_table_view source partition alphabet classes representative);
    ghost_ (final_def raw representative);
    let u = () in u

  let (quotient_step @ total) (source : machine)
      (partition : (int * int) list) (states : int list)
      (classes : int list) (initial : int)
      (representative : int) (letter : int) :
      {u : unit | if has_letter representative classes &&
        has_letter representative states then
        step (quotient_raw source partition states classes initial)
          representative letter ===
        partition_class partition (step source representative letter)
        else true} =
    let alphabet = collect_alphabet source states in
    let raw = quotient_raw source partition states classes initial in
    ghost_ (quotient_raw_def source partition states classes initial);
    ghost_ (quotient_table_view source partition alphabet classes representative);
    ghost_ (step_def raw representative letter);
    ghost_ (quotient_row_step source partition states representative letter);
    let u = () in u

  let (quotient_default @ total) (source : machine)
      (partition : (int * int) list) (states : int list)
      (classes : int list) (initial : int) (representative : int) :
      {u : unit | if has_letter representative classes then
        default (quotient_raw source partition states classes initial)
          representative ===
        partition_class partition (default source representative)
        else true} =
    let alphabet = collect_alphabet source states in
    let raw = quotient_raw source partition states classes initial in
    ghost_ (quotient_raw_def source partition states classes initial);
    ghost_ (quotient_table_view source partition alphabet classes representative);
    ghost_ (default_def raw representative);
    let u = () in u

  let[@def] rec partition_stable_row old_partition new_partition
      state others =
    match others with
    | [] -> true
    | other :: rest ->
      (if equal_int (partition_class old_partition state)
          (partition_class old_partition other) then
        equal_int (partition_class new_partition state)
          (partition_class new_partition other)
      else true) &&
      partition_stable_row old_partition new_partition state rest

  let rec (partition_stable_row_member @ total) :
      (old_partition : (int * int) list) ->
      (new_partition : (int * int) list) ->
      (state : int) -> (others : int list) -> (other : int) ->
      {u : unit | if partition_stable_row old_partition new_partition
          state others && has_letter other others &&
        equal_int (partition_class old_partition state)
          (partition_class old_partition other) then
        partition_class new_partition state ===
          partition_class new_partition other else true}
        @ immutable contended =
    fun old_partition new_partition state others other ->
    ghost_ (partition_stable_row_def old_partition new_partition state others);
    ghost_ (has_letter_def other others);
    let u = () in
    match others with
    | [] -> u
    | _ :: rest ->
      ghost_ (partition_stable_row_member old_partition new_partition
        state rest other);
      u

  let[@def] rec partition_stable old_partition new_partition states =
    match states with
    | [] -> true
    | state :: rest ->
      partition_stable_row old_partition new_partition state rest &&
      partition_stable old_partition new_partition rest

  let rec (partition_stable_pair @ total) :
      (old_partition : (int * int) list) ->
      (new_partition : (int * int) list) ->
      (states : int list) -> (left : int) -> (right : int) ->
      {u : unit | if partition_stable old_partition new_partition states &&
        has_letter left states && has_letter right states &&
        equal_int (partition_class old_partition left)
          (partition_class old_partition right) then
        partition_class new_partition left ===
          partition_class new_partition right else true}
        @ immutable contended =
    fun old_partition new_partition states left right ->
    ghost_ (partition_stable_def old_partition new_partition states);
    ghost_ (has_letter_def left states);
    ghost_ (has_letter_def right states);
    let u = () in
    match states with
    | [] -> u
    | head :: rest ->
      if equal_int left head then begin
        if equal_int right head then u
        else begin
          ghost_ (partition_stable_row_member old_partition new_partition
            head rest right);
          u
        end
      end else if equal_int right head then begin
        ghost_ (partition_stable_row_member old_partition new_partition
          head rest left);
        u
      end else begin
        ghost_ (partition_stable_pair old_partition new_partition rest left right);
        u
      end

  let[@def] rec same_class_row_size partition state others =
    match others with
    | [] -> 0Z
    | other :: rest ->
      Bigint.add
        (if equal_int (partition_class partition state)
            (partition_class partition other) then 1Z else 0Z)
        (same_class_row_size partition state rest)

  let[@def] rec same_class_pairs partition states =
    match states with
    | [] -> 0Z
    | state :: rest ->
      Bigint.add (same_class_row_size partition state rest)
        (same_class_pairs partition rest)

  let rec (same_class_row_nonnegative @ total) :
      (partition : (int * int) list) -> (state : int) -> (others : int list) ->
      {u : unit | Bigint.compare 0Z (same_class_row_size partition state others) <= 0}
        @ immutable contended =
    fun partition state others ->
    ghost_ (same_class_row_size_def partition state others);
    let u = () in
    match others with
    | [] -> u
    | _ :: rest -> same_class_row_nonnegative partition state rest; u

  let rec (same_class_pairs_nonnegative @ total) :
      (partition : (int * int) list) -> (states : int list) ->
      {u : unit | Bigint.compare 0Z (same_class_pairs partition states) <= 0}
        @ immutable contended =
    fun partition states ->
    ghost_ (same_class_pairs_def partition states);
    let u = () in
    match states with
    | [] -> u
    | state :: rest ->
      ghost_ (same_class_row_nonnegative partition state rest);
      ghost_ (same_class_pairs_nonnegative partition rest);
      u

  let rec (same_class_row_progress @ total) :
      (previous : (int * int) list) -> (next : (int * int) list) ->
      (state : int) -> (others : int list) ->
      {u : unit | if partition_stable_row next previous state others then
        Bigint.compare (same_class_row_size next state others)
          (same_class_row_size previous state others) <= 0 &&
        (if not (partition_stable_row previous next state others) then
          Bigint.compare (same_class_row_size next state others)
            (same_class_row_size previous state others) < 0 else true)
        else true} @ immutable contended =
    fun previous next state others ->
    ghost_ (partition_stable_row_def previous next state others);
    ghost_ (partition_stable_row_def next previous state others);
    ghost_ (same_class_row_size_def previous state others);
    ghost_ (same_class_row_size_def next state others);
    let u = () in
    match others with
    | [] -> u
    | _ :: rest -> same_class_row_progress previous next state rest; u

  let rec (same_class_pairs_progress @ total) :
      (previous : (int * int) list) -> (next : (int * int) list) ->
      (states : int list) ->
      {u : unit | if partition_stable next previous states then
        Bigint.compare (same_class_pairs next states)
          (same_class_pairs previous states) <= 0 &&
        (if not (partition_stable previous next states) then
          Bigint.compare (same_class_pairs next states)
            (same_class_pairs previous states) < 0 else true)
        else true} @ immutable contended =
    fun previous next states ->
    ghost_ (partition_stable_def previous next states);
    ghost_ (partition_stable_def next previous states);
    ghost_ (same_class_pairs_def previous states);
    ghost_ (same_class_pairs_def next states);
    let u = () in
    match states with
    | [] -> u
    | state :: rest ->
      ghost_ (same_class_row_progress previous next state rest);
      ghost_ (same_class_pairs_progress previous next rest);
      u

  let rec (refinement_stable_row @ total) :
      (source : machine) -> (partition : (int * int) list) ->
      (alphabet : int list) -> (states : int list) ->
      (state : int) -> (others : int list) ->
      {u : unit | if has_letter state states && letters_in others states then
        partition_stable_row (refine_partition source partition alphabet states)
          partition state others else true} @ immutable contended =
    fun source partition alphabet states state others ->
    let next = refine_partition source partition alphabet states in
    ghost_ (partition_stable_row_def next partition state others);
    ghost_ (letters_in_def others states);
    let u = () in
    match others with
    | [] -> u
    | other :: rest ->
      ghost_ (refine_partition_refines source partition alphabet states state other);
      ghost_ (refinement_stable_row source partition alphabet states state rest);
      u

  let rec (refinement_stable_rows @ total) :
      (source : machine) -> (partition : (int * int) list) ->
      (alphabet : int list) -> (states : int list) -> (remaining : int list) ->
      {u : unit | if letters_in remaining states then
        partition_stable (refine_partition source partition alphabet states)
          partition remaining else true} @ immutable contended =
    fun source partition alphabet states remaining ->
    let next = refine_partition source partition alphabet states in
    ghost_ (partition_stable_def next partition remaining);
    ghost_ (letters_in_def remaining states);
    let u = () in
    match remaining with
    | [] -> u
    | state :: rest ->
      ghost_ (refinement_stable_row source partition alphabet states state rest);
      ghost_ (refinement_stable_rows source partition alphabet states rest);
      u

  let (partition_stable_signature @ total) (source : machine)
      (partition : (int * int) list) (alphabet : int list)
      (states : int list) (left : int) (right : int) :
      {u : unit | if partition_stable partition
          (refine_partition source partition alphabet states) states &&
        has_letter left states && has_letter right states &&
        equal_int (partition_class partition left)
          (partition_class partition right) then
        same_partition_signature source partition alphabet left right
        else true} =
    let next = refine_partition source partition alphabet states in
    ghost_ (partition_stable_pair partition next states left right);
    ghost_ (refine_partition_exact source partition alphabet states left right);
    let u = () in u

  let (partition_stable_default @ total) (source : machine)
      (partition : (int * int) list) (alphabet : int list)
      (states : int list) (left : int) (right : int) :
      {u : unit | if partition_stable partition
          (refine_partition source partition alphabet states) states &&
        has_letter left states && has_letter right states &&
        equal_int (partition_class partition left)
          (partition_class partition right) then
        partition_class partition (default source left) ===
          partition_class partition (default source right)
        else true} =
    ghost_ (partition_stable_signature source partition alphabet
      states left right);
    ghost_ (same_partition_signature_correct source partition alphabet left right);
    let u = () in u

  let (partition_stable_label @ total) (source : machine)
      (partition : (int * int) list) (alphabet : int list)
      (states : int list) (left : int) (right : int)
      (letter : int) :
      {u : unit | if partition_stable partition
          (refine_partition source partition alphabet states) states &&
        has_letter left states && has_letter right states &&
        has_letter letter alphabet &&
        equal_int (partition_class partition left)
          (partition_class partition right) then
        partition_class partition (step source left letter) ===
          partition_class partition (step source right letter)
        else true} =
    ghost_ (partition_stable_signature source partition alphabet
      states left right);
    ghost_ (same_partition_signature_correct source partition alphabet left right);
    ghost_ (successor_classes_equal_member source partition alphabet
      left right letter);
    let u = () in u

  let (partition_stable_step @ total) (source : machine)
      (partition : (int * int) list) (states : int list)
      (left : int) (right : int) (letter : int) :
      {u : unit | let alphabet = collect_alphabet source states in
        if partition_stable partition
          (refine_partition source partition alphabet states) states &&
        has_letter left states && has_letter right states &&
        equal_int (partition_class partition left)
          (partition_class partition right) then
        partition_class partition (step source left letter) ===
          partition_class partition (step source right letter)
        else true} =
    let alphabet = collect_alphabet source states in
    let u = () in
    if has_letter letter alphabet then begin
      ghost_ (partition_stable_label source partition alphabet
        states left right letter);
      u
    end else begin
      ghost_ (collect_alphabet_member source states left letter);
      ghost_ (collect_alphabet_member source states right letter);
      ghost_ (step_outside source left letter);
      ghost_ (step_outside source right letter);
      ghost_ (partition_stable_default source partition alphabet
        states left right);
      u
    end

  let (stable_partition_step @ total) (source : machine)
      (entries : (int * int list) list)
      (previous : (int * int) list) (stable : (int * int) list)
      (left : int) (right : int) (letter : int) :
      {u : unit | let states = states_of_entries entries in
        let alphabet = collect_alphabet source states in
        if all_reach_closed source entries entries &&
          partition_stable previous stable states &&
          stable === refine_partition source previous alphabet states &&
          has_letter left states && has_letter right states &&
          equal_int (partition_class stable left)
            (partition_class stable right) then
          partition_class stable (step source left letter) ===
            partition_class stable (step source right letter)
        else true} =
    let states = states_of_entries entries in
    let alphabet = collect_alphabet source states in
    ghost_ (refine_partition_refines source previous alphabet states left right);
    ghost_ (partition_stable_step source previous states left right letter);
    ghost_ (reachable_states_step source entries left letter);
    ghost_ (reachable_states_step source entries right letter);
    let left_target = step source left letter in
    let right_target = step source right letter in
    ghost_ (partition_stable_pair previous stable states
      left_target right_target);
    let u = () in u

  let (stable_partition_default @ total) (source : machine)
      (entries : (int * int list) list)
      (previous : (int * int) list) (stable : (int * int) list)
      (left : int) (right : int) :
      {u : unit | let states = states_of_entries entries in
        let alphabet = collect_alphabet source states in
        if all_reach_closed source entries entries &&
          partition_stable previous stable states &&
          stable === refine_partition source previous alphabet states &&
          has_letter left states && has_letter right states &&
          equal_int (partition_class stable left)
            (partition_class stable right) then
          partition_class stable (default source left) ===
            partition_class stable (default source right)
        else true} =
    let states = states_of_entries entries in
    let alphabet = collect_alphabet source states in
    ghost_ (refine_partition_refines source previous alphabet states left right);
    ghost_ (partition_stable_default source previous alphabet states left right);
    ghost_ (reachable_states_default source entries left);
    ghost_ (reachable_states_default source entries right);
    let left_target = default source left in
    let right_target = default source right in
    ghost_ (partition_stable_pair previous stable states
      left_target right_target);
    let u = () in u

  let rec (stable_partition_language @ total) :
      (source : machine) -> (entries : (int * int list) list) ->
      (previous : (int * int) list) ->
      (stable : (int * int) list) ->
      (left : int) -> (right : int) -> (word : int list) ->
      {u : unit | let states = states_of_entries entries in
        let alphabet = collect_alphabet source states in
        if all_reach_closed source entries entries &&
          accepting_partition source stable states &&
          partition_stable previous stable states &&
          stable === refine_partition source previous alphabet states &&
          has_letter left states && has_letter right states &&
          equal_int (partition_class stable left)
            (partition_class stable right) then
          run_from source left word === run_from source right word
        else true} @ immutable contended =
    fun source entries previous stable left right word ->
    let states = states_of_entries entries in
    let u = () in
    match word with
    | [] ->
      ghost_ (accepting_partition_pair source stable states left right);
      ghost_ (run_from_empty source left);
      ghost_ (run_from_empty source right);
      u
    | letter :: suffix ->
      ghost_ (stable_partition_step source entries previous stable
        left right letter);
      ghost_ (reachable_states_step source entries left letter);
      ghost_ (reachable_states_step source entries right letter);
      let left_target = step source left letter in
      let right_target = step source right letter in
      ghost_ (stable_partition_language source entries previous stable
        left_target right_target suffix);
      ghost_ (run_from_letter source left letter suffix);
      ghost_ (run_from_letter source right letter suffix);
      u

  let (quotient_one_step @ total) (source : machine)
      (entries : (int * int list) list)
      (previous : (int * int) list) (stable : (int * int) list)
      (initial : int) (state : int) (letter : int) :
      {u : unit | let states = states_of_entries entries in
        let alphabet = collect_alphabet source states in
        let classes = collect_classes stable states in
        let quotient = quotient_raw source stable states classes initial in
        let representative = partition_class stable state in
        if all_reach_closed source entries entries &&
          accepting_partition source stable states &&
          partition_stable previous stable states &&
          stable === refine_partition source previous alphabet states &&
          has_letter state states then
          final quotient representative === final source state &&
          step quotient representative letter ===
            partition_class stable (step source state letter)
        else true} =
    let states = states_of_entries entries in
    let alphabet = collect_alphabet source states in
    let classes = collect_classes stable states in
    let representative = partition_class stable state in
    ghost_ (collect_classes_member stable states state);
    ghost_ (letters_in_refl states);
    ghost_ (collect_classes_representatives source previous alphabet
      states states representative);
    ghost_ (refine_partition_idempotent source previous alphabet states state);
    ghost_ (accepting_partition_pair source stable states state representative);
    ghost_ (stable_partition_step source entries previous stable
      state representative letter);
    ghost_ (quotient_final source stable states classes initial representative);
    ghost_ (quotient_step source stable states classes initial
      representative letter);
    let u = () in u

  let rec (quotient_labels_closed @ total) :
      (source : machine) -> (entries : (int * int list) list) ->
      (previous : (int * int) list) ->
      (stable : (int * int) list) -> (initial : int) ->
      (state : int) -> (letters : int list) ->
      {u : unit | let states = states_of_entries entries in
        let alphabet = collect_alphabet source states in
        let classes = collect_classes stable states in
        let quotient = quotient_raw source stable states classes initial in
        let representative = partition_class stable state in
        let relation = quotient_relation stable states in
        if all_reach_closed source entries entries &&
          accepting_partition source stable states &&
          partition_stable previous stable states &&
          stable === refine_partition source previous alphabet states &&
          has_letter state states then
          labelled_closed source quotient state representative
            letters relation else true} @ immutable contended =
    fun source entries previous stable initial state letters ->
    let states = states_of_entries entries in
    let classes = collect_classes stable states in
    let quotient = quotient_raw source stable states classes initial in
    let representative = partition_class stable state in
    let relation = quotient_relation stable states in
    ghost_ (labelled_closed_def source quotient state representative
      letters relation);
    let u = () in
    match letters with
    | [] -> u
    | letter :: rest ->
      ghost_ (quotient_one_step source entries previous stable
        initial state letter);
      ghost_ (reachable_states_step source entries state letter);
      let target = step source state letter in
      ghost_ (quotient_related stable states target);
      ghost_ (quotient_labels_closed source entries previous stable
        initial state rest);
      u

  let (quotient_pair_closed @ total) (source : machine)
      (entries : (int * int list) list)
      (previous : (int * int) list) (stable : (int * int) list)
      (initial : int) (state : int) :
      {u : unit | let states = states_of_entries entries in
        let alphabet = collect_alphabet source states in
        let classes = collect_classes stable states in
        let quotient = quotient_raw source stable states classes initial in
        let representative = partition_class stable state in
        let relation = quotient_relation stable states in
        if all_reach_closed source entries entries &&
          accepting_partition source stable states &&
          partition_stable previous stable states &&
          stable === refine_partition source previous alphabet states &&
          has_letter state states then
          pair_closed source quotient state representative relation
        else true} =
    let states = states_of_entries entries in
    let alphabet = collect_alphabet source states in
    let classes = collect_classes stable states in
    let quotient = quotient_raw source stable states classes initial in
    let representative = partition_class stable state in
    let relation = quotient_relation stable states in
    let source_labels = labels source state in
    let quotient_labels = labels quotient representative in
    let letters = append source_labels quotient_labels in
    ghost_ (pair_closed_def source quotient state representative relation);
    let zero = 0 in
    ghost_ (quotient_one_step source entries previous stable initial state zero);
    ghost_ (collect_classes_member stable states state);
    ghost_ (quotient_default source stable states classes initial representative);
    ghost_ (refine_partition_idempotent source previous alphabet states state);
    ghost_ (letters_in_refl states);
    ghost_ (collect_classes_representatives source previous
      alphabet states states representative);
    ghost_ (stable_partition_default source entries previous stable
      state representative);
    ghost_ (reachable_states_default source entries state);
    let target = default source state in
    ghost_ (quotient_related stable states target);
    ghost_ (quotient_labels_closed source entries previous stable
      initial state letters);
    let u = () in u

  let rec (quotient_all_closed @ total) :
      (source : machine) -> (entries : (int * int list) list) ->
      (previous : (int * int) list) ->
      (stable : (int * int) list) -> (initial : int) ->
      (remaining : int list) ->
      {u : unit | let states = states_of_entries entries in
        let alphabet = collect_alphabet source states in
        let classes = collect_classes stable states in
        let quotient = quotient_raw source stable states classes initial in
        let relation = quotient_relation stable states in
        if all_reach_closed source entries entries &&
          accepting_partition source stable states &&
          partition_stable previous stable states &&
          stable === refine_partition source previous alphabet states &&
          letters_in remaining states then
          all_closed source quotient relation
            (quotient_relation stable remaining) else true}
        @ immutable contended =
    fun source entries previous stable initial remaining ->
    let states = states_of_entries entries in
    let classes = collect_classes stable states in
    let quotient = quotient_raw source stable states classes initial in
    let relation = quotient_relation stable states in
    let pairs = quotient_relation stable remaining in
    ghost_ (quotient_relation_def stable remaining);
    ghost_ (all_closed_def source quotient relation pairs);
    ghost_ (letters_in_def remaining states);
    let u = () in
    match remaining with
    | [] -> u
    | state :: rest ->
      ghost_ (quotient_pair_closed source entries previous stable initial state);
      ghost_ (letters_in_tail remaining states);
      ghost_ (quotient_all_closed source entries previous stable initial rest);
      u

  let (quotient_check @ total) (source : machine)
      (entries : (int * int list) list)
      (previous : (int * int) list) (stable : (int * int) list) :
      {u : unit | let initial, _ = source in
        let states = states_of_entries entries in
        let alphabet = collect_alphabet source states in
        let classes = collect_classes stable states in
        let quotient = quotient_raw source stable states classes initial in
        let relation = quotient_relation stable states in
        if all_reach_closed source entries entries &&
          access_member initial entries &&
          accepting_partition source stable states &&
          partition_stable previous stable states &&
          stable === refine_partition source previous alphabet states then
          check source quotient relation else true} =
    let initial, _ = source in
    let states = states_of_entries entries in
    let classes = collect_classes stable states in
    let quotient = quotient_raw source stable states classes initial in
    let relation = quotient_relation stable states in
    ghost_ (states_of_entries_member entries initial);
    ghost_ (quotient_related stable states initial);
    ghost_ (letters_in_refl states);
    ghost_ (quotient_all_closed source entries previous stable initial states);
    ghost_ (check_def source quotient relation);
    ghost_ (quotient_raw_def source stable states classes initial);
    let u = () in u

  let (quotient_check_reduced @ total) (source : machine)
      (entries : (int * int list) list)
      (previous : (int * int) list) (stable : (int * int) list)
      (reduced : machine) :
      {u : unit | let initial, _ = source in
        let states = states_of_entries entries in
        let alphabet = collect_alphabet source states in
        let classes = collect_classes stable states in
        let raw = quotient_raw source stable states classes initial in
        let relation = quotient_relation stable states in
        if all_reach_closed source entries entries &&
          access_member initial entries &&
          accepting_partition source stable states &&
          partition_stable previous stable states &&
          stable === refine_partition source previous alphabet states &&
          of_raw raw === Some reduced then
          check source reduced relation else true} =
    let initial, _ = source in
    let states = states_of_entries entries in
    let classes = collect_classes stable states in
    let raw = quotient_raw source stable states classes initial in
    ghost_ (quotient_check source entries previous stable);
    ghost_ (of_raw_identity raw reduced);
    let u = () in u

  let rec (quotient_run_from @ total) :
      (source : machine) -> (entries : (int * int list) list) ->
      (previous : (int * int) list) ->
      (stable : (int * int) list) -> (initial : int) ->
      (state : int) -> (word : int list) ->
      {u : unit | let states = states_of_entries entries in
        let alphabet = collect_alphabet source states in
        let classes = collect_classes stable states in
        let quotient = quotient_raw source stable states classes initial in
        if all_reach_closed source entries entries &&
          accepting_partition source stable states &&
          partition_stable previous stable states &&
          stable === refine_partition source previous alphabet states &&
          has_letter state states then
          run_from quotient (partition_class stable state) word ===
            run_from source state word else true} @ immutable contended =
    fun source entries previous stable initial state word ->
    let states = states_of_entries entries in
    let classes = collect_classes stable states in
    let quotient = quotient_raw source stable states classes initial in
    let representative = partition_class stable state in
    let u = () in
    match word with
    | [] ->
      let zero = 0 in
      ghost_ (quotient_one_step source entries previous stable initial state zero);
      ghost_ (run_from_empty quotient representative);
      ghost_ (run_from_empty source state);
      u
    | letter :: suffix ->
      ghost_ (quotient_one_step source entries previous stable
        initial state letter);
      ghost_ (reachable_states_step source entries state letter);
      let next = step source state letter in
      ghost_ (quotient_run_from source entries previous stable
        initial next suffix);
      ghost_ (run_from_letter quotient representative letter suffix);
      ghost_ (run_from_letter source state letter suffix);
      u

  let (quotient_run @ total) (source : machine)
      (entries : (int * int list) list)
      (previous : (int * int) list) (stable : (int * int) list)
      (word : int list) :
      {u : unit | let initial, _ = source in
        let states = states_of_entries entries in
        let alphabet = collect_alphabet source states in
        let classes = collect_classes stable states in
        let quotient = quotient_raw source stable states classes initial in
        if all_reach_closed source entries entries &&
          access_member initial entries &&
          accepting_partition source stable states &&
          partition_stable previous stable states &&
          stable === refine_partition source previous alphabet states then
          run quotient word === run source word else true} =
    let initial, _ = source in
    let states = states_of_entries entries in
    let classes = collect_classes stable states in
    let quotient = quotient_raw source stable states classes initial in
    let representative = partition_class stable initial in
    ghost_ (quotient_raw_def source stable states classes initial);
    ghost_ (states_of_entries_member entries initial);
    ghost_ (quotient_run_from source entries previous stable initial initial word);
    ghost_ (run_def quotient word);
    ghost_ (run_def source word);
    ghost_ (run_from_def quotient representative word);
    ghost_ (run_from_def source initial word);
    let u = () in u

  let rec (quotient_drive @ total) :
      (source : machine) -> (entries : (int * int list) list) ->
      (previous : (int * int) list) ->
      (stable : (int * int) list) -> (initial : int) ->
      (state : int) -> (word : int list) ->
      {u : unit | let states = states_of_entries entries in
        let alphabet = collect_alphabet source states in
        let classes = collect_classes stable states in
        let quotient = quotient_raw source stable states classes initial in
        if all_reach_closed source entries entries &&
          accepting_partition source stable states &&
          partition_stable previous stable states &&
          stable === refine_partition source previous alphabet states &&
          has_letter state states then
          drive quotient (partition_class stable state) word ===
            partition_class stable (drive source state word)
        else true} @ immutable contended =
    fun source entries previous stable initial state word ->
    let states = states_of_entries entries in
    let classes = collect_classes stable states in
    let quotient = quotient_raw source stable states classes initial in
    let representative = partition_class stable state in
    ghost_ (drive_def quotient representative word);
    ghost_ (drive_def source state word);
    let u = () in
    match word with
    | [] -> u
    | letter :: suffix ->
      ghost_ (quotient_one_step source entries previous stable
        initial state letter);
      ghost_ (reachable_states_step source entries state letter);
      let next = step source state letter in
      ghost_ (quotient_drive source entries previous stable
        initial next suffix);
      u

  let (quotient_reached @ total) (source : machine)
      (entries : (int * int list) list)
      (previous : (int * int) list) (stable : (int * int) list)
      (word : int list) :
      {u : unit | let initial, _ = source in
        let states = states_of_entries entries in
        let alphabet = collect_alphabet source states in
        let classes = collect_classes stable states in
        let quotient = quotient_raw source stable states classes initial in
        if all_reach_closed source entries entries &&
          access_member initial entries &&
          accepting_partition source stable states &&
          partition_stable previous stable states &&
          stable === refine_partition source previous alphabet states then
          reached quotient word ===
            partition_class stable (reached source word)
        else true} =
    let initial, _ = source in
    let states = states_of_entries entries in
    let classes = collect_classes stable states in
    let quotient = quotient_raw source stable states classes initial in
    ghost_ (states_of_entries_member entries initial);
    ghost_ (quotient_drive source entries previous stable initial initial word);
    ghost_ (quotient_raw_def source stable states classes initial);
    ghost_ (reached_def quotient word);
    ghost_ (reached_def source word);
    let u = () in u

  let (quotient_access_word @ total) (source : machine)
      (entries : (int * int list) list)
      (previous : (int * int) list) (stable : (int * int) list)
      (class_id : int) :
      {u : unit | let initial, _ = source in
        let states = states_of_entries entries in
        let alphabet = collect_alphabet source states in
        let classes = collect_classes stable states in
        let quotient = quotient_raw source stable states classes initial in
        if access_valid source entries &&
          all_reach_closed source entries entries &&
          access_member initial entries &&
          accepting_partition source stable states &&
          partition_stable previous stable states &&
          stable === refine_partition source previous alphabet states &&
          has_letter class_id classes then
          match access_word class_id (quotient_access stable entries) with
          | None -> false
          | Some word -> reached quotient word === class_id
        else true} =
    let access = quotient_access stable entries in
    ghost_ (quotient_access_member stable entries class_id);
    ghost_ (access_word_complete class_id access);
    ghost_ (quotient_access_word_source source stable entries class_id);
    let u = () in
    match access_word class_id access with
    | None -> u
    | Some word ->
      ghost_ (quotient_reached source entries previous stable word);
      u

  let rec (quotient_all_access_rows @ total) :
      (source : machine) -> (entries : (int * int list) list) ->
      (previous : (int * int) list) ->
      (stable : (int * int) list) -> (remaining : int list) ->
      {u : unit | let initial, _ = source in
        let states = states_of_entries entries in
        let alphabet = collect_alphabet source states in
        let classes = collect_classes stable states in
        let quotient = quotient_raw source stable states classes initial in
        let access = quotient_access stable entries in
        if access_valid source entries &&
          all_reach_closed source entries entries &&
          access_member initial entries &&
          accepting_partition source stable states &&
          partition_stable previous stable states &&
          stable === refine_partition source previous alphabet states &&
          letters_in remaining classes then
          all_access quotient remaining access else true}
        @ immutable contended =
    fun source entries previous stable remaining ->
    let initial, _ = source in
    let states = states_of_entries entries in
    let classes = collect_classes stable states in
    let quotient = quotient_raw source stable states classes initial in
    let access = quotient_access stable entries in
    ghost_ (all_access_def quotient remaining access);
    ghost_ (letters_in_def remaining classes);
    let u = () in
    match remaining with
    | [] -> u
    | class_id :: rest ->
      ghost_ (quotient_access_word source entries previous stable class_id);
      let word = access_word class_id access in
      (match word with
       | None -> u
       | Some word ->
         ghost_ (reached_def quotient word);
         ghost_ (letters_in_tail remaining classes);
         ghost_ (quotient_all_access_rows source entries previous stable rest);
         u)

  let (quotient_all_access @ total) (source : machine)
      (entries : (int * int list) list)
      (previous : (int * int) list) (stable : (int * int) list) :
      {u : unit | let initial, _ = source in
        let states = states_of_entries entries in
        let alphabet = collect_alphabet source states in
        let classes = collect_classes stable states in
        let quotient = quotient_raw source stable states classes initial in
        let access = quotient_access stable entries in
        if access_valid source entries &&
          all_reach_closed source entries entries &&
          access_member initial entries &&
          accepting_partition source stable states &&
          partition_stable previous stable states &&
          stable === refine_partition source previous alphabet states then
          all_access quotient classes access else true} =
    let states = states_of_entries entries in
    let classes = collect_classes stable states in
    ghost_ (letters_in_refl classes);
    ghost_ (quotient_all_access_rows source entries previous stable classes);
    let u = () in u

  let (quotient_all_access_reduced @ total) (source : machine)
      (entries : (int * int list) list)
      (previous : (int * int) list) (stable : (int * int) list)
      (reduced : machine) :
      {u : unit | let initial, _ = source in
        let states = states_of_entries entries in
        let alphabet = collect_alphabet source states in
        let classes = collect_classes stable states in
        let raw = quotient_raw source stable states classes initial in
        let access = quotient_access stable entries in
        let _, table = reduced in
        if access_valid source entries &&
          all_reach_closed source entries entries &&
          access_member initial entries &&
          accepting_partition source stable states &&
          partition_stable previous stable states &&
          stable === refine_partition source previous alphabet states &&
          of_raw raw === Some reduced then
          all_access reduced (state_ids table) access else true} =
    let initial, _ = source in
    let states = states_of_entries entries in
    let alphabet = collect_alphabet source states in
    let classes = collect_classes stable states in
    let raw = quotient_raw source stable states classes initial in
    ghost_ (quotient_all_access source entries previous stable);
    ghost_ (quotient_state_ids source stable alphabet classes);
    ghost_ (quotient_raw_def source stable states classes initial);
    ghost_ (of_raw_identity raw reduced);
    let u = () in u

  let (quotient_separates @ total) (source : machine)
      (entries : (int * int list) list)
      (previous : (int * int) list) (stable : (int * int) list)
      (p : int) (q : int) (word : int list) :
      {u : unit | let initial, table = source in
        let states = states_of_entries entries in
        let alphabet = collect_alphabet source states in
        let classes = collect_classes stable states in
        let quotient = quotient_raw source stable states classes initial in
        if all_reach_closed source entries entries &&
          accepting_partition source stable states &&
          partition_stable previous stable states &&
          stable === refine_partition source previous alphabet states &&
          has_letter p classes && has_letter q classes &&
          run (p, table) word <> run (q, table) word then
          execute quotient p word <> execute quotient q word
        else true} =
    let initial, _ = source in
    let states = states_of_entries entries in
    let alphabet = collect_alphabet source states in
    let classes = collect_classes stable states in
    let quotient = quotient_raw source stable states classes initial in
    ghost_ (letters_in_refl states);
    ghost_ (collect_classes_representatives source previous alphabet
      states states p);
    ghost_ (collect_classes_representatives source previous alphabet
      states states q);
    ghost_ (collect_classes_fixed source previous alphabet states states p);
    ghost_ (collect_classes_fixed source previous alphabet states states q);
    ghost_ (quotient_run_from source entries previous stable initial p word);
    ghost_ (quotient_run_from source entries previous stable initial q word);
    ghost_ (run_rebased source p word);
    ghost_ (run_rebased source q word);
    ghost_ (run_from_def quotient p word);
    ghost_ (run_from_def quotient q word);
    let u = () in u

  let rec (quotient_separates_from @ total) :
      (source : machine) -> (entries : (int * int list) list) ->
      (previous : (int * int) list) ->
      (stable : (int * int) list) -> (p : int) ->
      (others : int list) ->
      (separate : (int * int * int list) list) ->
      {u : unit | let initial, _ = source in
        let states = states_of_entries entries in
        let alphabet = collect_alphabet source states in
        let classes = collect_classes stable states in
        let quotient = quotient_raw source stable states classes initial in
        if all_reach_closed source entries entries &&
          accepting_partition source stable states &&
          partition_stable previous stable states &&
          stable === refine_partition source previous alphabet states &&
          has_letter p classes && letters_in others classes &&
          separates_from source p others separate then
          separates_from quotient p others separate else true}
        @ immutable contended =
    fun source entries previous stable p others separate ->
    let initial, _ = source in
    let states = states_of_entries entries in
    let classes = collect_classes stable states in
    let quotient = quotient_raw source stable states classes initial in
    ghost_ (separates_from_def source p others separate);
    ghost_ (separates_from_def quotient p others separate);
    ghost_ (letters_in_def others classes);
    let u = () in
    match others with
    | [] -> u
    | q :: rest ->
      if p = q then begin
        ghost_ (letters_in_tail others classes);
        ghost_ (quotient_separates_from source entries previous stable p rest
          separate);
        u
      end else begin
        ghost_ (separates_from_member source p others separate q);
        let found = separating_word p q separate in
        match found with
        | None ->
          ghost_ (letters_in_tail others classes);
          ghost_ (quotient_separates_from source entries previous stable p rest
            separate);
          u
        | Some word ->
          ghost_ (run_from_def source p word);
          ghost_ (run_from_def source q word);
          ghost_ (run_rebased source p word);
          ghost_ (run_rebased source q word);
          ghost_ (quotient_separates source entries previous stable p q word);
          ghost_ (letters_in_tail others classes);
          ghost_ (quotient_separates_from source entries previous stable p rest
            separate);
          u
      end

  let rec (quotient_all_separated_rows @ total) :
      (source : machine) -> (entries : (int * int list) list) ->
      (previous : (int * int) list) ->
      (stable : (int * int) list) ->
      (remaining : int list) ->
      (separate : (int * int * int list) list) ->
      {u : unit | let initial, _ = source in
        let states = states_of_entries entries in
        let alphabet = collect_alphabet source states in
        let classes = collect_classes stable states in
        let quotient = quotient_raw source stable states classes initial in
        if all_reach_closed source entries entries &&
          accepting_partition source stable states &&
          partition_stable previous stable states &&
          stable === refine_partition source previous alphabet states &&
          letters_in remaining classes &&
          all_separated source classes remaining separate then
          all_separated quotient classes remaining separate else true}
        @ immutable contended =
    fun source entries previous stable remaining separate ->
    let initial, _ = source in
    let states = states_of_entries entries in
    let classes = collect_classes stable states in
    let quotient = quotient_raw source stable states classes initial in
    ghost_ (letters_in_refl classes);
    ghost_ (letters_in_def remaining classes);
    ghost_ (all_separated_def source classes remaining separate);
    ghost_ (all_separated_def quotient classes remaining separate);
    let u = () in
    match remaining with
    | [] -> u
    | p :: rest ->
      ghost_ (quotient_separates_from source entries previous stable p classes
        separate);
      ghost_ (letters_in_tail remaining classes);
      ghost_ (quotient_all_separated_rows source entries previous stable rest
        separate);
      u

  let (quotient_all_separated_reduced @ total) (source : machine)
      (entries : (int * int list) list)
      (previous : (int * int) list) (stable : (int * int) list)
      (reduced : machine)
      (separate : (int * int * int list) list) :
      {u : unit | let initial, _ = source in
        let states = states_of_entries entries in
        let alphabet = collect_alphabet source states in
        let classes = collect_classes stable states in
        let raw = quotient_raw source stable states classes initial in
        let _, table = reduced in
        if all_reach_closed source entries entries &&
          accepting_partition source stable states &&
          partition_stable previous stable states &&
          stable === refine_partition source previous alphabet states &&
          of_raw raw === Some reduced &&
          all_separated source classes classes separate then
          all_separated reduced (state_ids table)
            (state_ids table) separate else true} =
    let initial, _ = source in
    let states = states_of_entries entries in
    let alphabet = collect_alphabet source states in
    let classes = collect_classes stable states in
    let raw = quotient_raw source stable states classes initial in
    ghost_ (letters_in_refl classes);
    ghost_ (quotient_all_separated_rows source entries previous stable classes
      separate);
    ghost_ (quotient_state_ids source stable alphabet classes);
    ghost_ (quotient_raw_def source stable states classes initial);
    ghost_ (of_raw_identity raw reduced);
    let u = () in u

  let[@def] separation_budget source limit =
    valid source && labels_bounded source && 0 < limit && limit <= 65_536 &&
    Bigint.compare (Bigint.mul (state_size source) (state_size source))
      (Bigint.of_int limit) <= 0

  let rec (square_limit @ total) : (limit : int) ->
      {result : int | 0 <= result && result <= 4096 &&
        (if 0 <= limit && limit <= 64 then
          Bigint.of_int result ===
            Bigint.mul (Bigint.of_int limit) (Bigint.of_int limit)
         else true)} @ immutable contended =
    fun limit ->
    if limit <= 0 || limit > 64 then
      let result = 0 in result
    else
      let previous = limit - 1 in
      let smaller = square_limit previous in
      let result = smaller + limit + previous in
      result
  [@@decreases limit]

  let (separation_budget_from_limit @ total) (source : machine) (limit : int)
      (pair_limit : int) :
      {u : unit | if valid source && labels_bounded source &&
        0 < limit && limit <= 64 &&
        Bigint.compare (state_size source) (Bigint.of_int limit) <= 0 &&
        Bigint.of_int pair_limit ===
          Bigint.mul (Bigint.of_int limit) (Bigint.of_int limit) then
        separation_budget source pair_limit else true} =
    let _, table = source in
    let states = state_ids table in
    ghost_ (big_length_nonnegative states);
    ghost_ (state_size_def source);
    ghost_ (separation_budget_def source pair_limit);
    let u = () in u

  let rec (access_member_valid @ total) :
      (source : machine) -> (entries : (int * int list) list) -> (state : int) ->
      {u : unit | if valid source && access_valid source entries &&
        access_member state entries then has_state source state else true}
        @ immutable contended =
    fun source entries state ->
    ghost_ (access_valid_def source entries);
    ghost_ (access_member_def state entries);
    let u = () in
    match entries with
    | [] -> u
    | (head, word) :: rest ->
      ghost_ (reached_valid source word);
      ghost_ (access_member_valid source rest state);
      u

  let rec (cover_row @ total) :
      (source : machine) -> (classes : int list) ->
      (done_states : int list) -> (p : int) ->
      (others : int list) ->
      (separate : (int * int * int list) list) -> (limit : int) ->
      (distinguish : ((p : int) -> (q : int) ->
        {result : int list option |
          (match result with None -> true | Some word ->
            run_from source p word <> run_from source q word) &&
          (if separation_budget source limit && has_letter p classes &&
            has_letter q classes && p <> q then
            match result with None -> false | Some _ -> true else true)})) @ total ->
      {result : (int * int * int list) list option |
        (if separation_budget source limit && has_letter p classes &&
          letters_in others classes then
          match result with None -> false | Some _ -> true else true) &&
        if all_separated source classes done_states separate then
          match result with
          | None -> true
          | Some extended ->
            all_separated source classes done_states extended &&
            separates_from source p others extended
        else true} @ immutable contended =
    fun source classes done_states p others separate limit distinguish ->
    ghost_ (letters_in_def others classes);
    match others with
    | [] ->
      ghost_ (separates_from_empty source p separate);
      let result = Some separate in result
    | q :: rest ->
      let tail_result =
        cover_row source classes done_states p rest separate limit distinguish in
      match tail_result with
      | None -> let result = None in result
      | Some tail ->
        if p = q then begin
          ghost_ (separates_from_def source p others tail);
          let result = Some tail in result
        end else begin
          let witness = distinguish p q in
          match witness with
          | None -> let result = None in result
          | Some word ->
            ghost_ (run_from_def source p word);
            ghost_ (run_from_def source q word);
            ghost_ (all_separated_prepend_valid source classes done_states tail
              p q word);
            ghost_ (separates_from_prepend_valid source p rest tail p q word);
            let extended = (p, q, word) :: tail in
            ghost_ (separating_word_def p q extended);
            ghost_ (separates_from_def source p others extended);
            let result = Some extended in result
        end

  let rec (cover_rows @ total) :
      (source : machine) -> (classes : int list) ->
      (remaining : int list) ->
      (separate : (int * int * int list) list) -> (limit : int) ->
      (distinguish : ((p : int) -> (q : int) ->
        {result : int list option |
          (match result with None -> true | Some word ->
            run_from source p word <> run_from source q word) &&
          (if separation_budget source limit && has_letter p classes &&
            has_letter q classes && p <> q then
            match result with None -> false | Some _ -> true else true)})) @ total ->
      {result : (int * int * int list) list option |
        (if separation_budget source limit && letters_in remaining classes then
          match result with None -> false | Some _ -> true else true) &&
        if all_separated source classes [] separate then
          match result with
          | None -> true
          | Some extended ->
            all_separated source classes remaining extended
        else true} @ immutable contended =
    fun source classes remaining separate limit distinguish ->
    ghost_ (letters_in_def remaining classes);
    ghost_ (letters_in_refl classes);
    match remaining with
    | [] ->
      ghost_ (all_separated_def source classes remaining separate);
      let result = Some separate in result
    | p :: rest ->
      let tail_result =
        cover_rows source classes rest separate limit distinguish in
      match tail_result with
      | None -> let result = None in result
      | Some tail ->
        let row_result =
          cover_row source classes rest p classes tail limit distinguish in
        (match row_result with
         | None -> let result = None in result
         | Some extended ->
           ghost_ (all_separated_def source classes remaining extended);
           let result = Some extended in result)

  let rec (copy_word @ total) :
      (word : int list) -> {result : int list | result === word}
        @ total immutable contended =
    fun word ->
    match word with
    | [] -> let result = [] in result
    | letter :: rest ->
      let tail = copy_word rest in
      let result = letter :: tail in result

  let rec (copy_relation @ total) :
      (relation : relation) ->
      {result : relation | result === relation}
        @ total immutable contended =
    fun relation ->
    match relation with
    | [] -> let result = [] in result
    | pair :: rest ->
      let tail = copy_relation rest in
      let result = pair :: tail in result

  let rec (copy_access @ total) :
      (access : (int * int list) list) ->
      {result : (int * int list) list | result === access}
        @ total immutable contended =
    fun access ->
    match access with
    | [] -> let result = [] in result
    | (state, word) :: rest ->
      let copied_word = copy_word word in
      let copied_rest = copy_access rest in
      let result = (state, copied_word) :: copied_rest in
      result

  let rec (copy_separations @ total) :
      (separate : (int * int * int list) list) ->
      {result : (int * int * int list) list | result === separate}
        @ total immutable contended =
    fun separate ->
    match separate with
    | [] -> let result = [] in result
    | (p, q, word) :: rest ->
      let copied_word = copy_word word in
      let copied_rest = copy_separations rest in
      let result = (p, q, copied_word) :: copied_rest in
      result

  let rec (copy_table @ total) :
      (table : (int * bool * row) list) ->
      {result : (int * bool * row) list | result === table}
        @ total immutable contended =
    fun table ->
    match table with
    | [] -> let result = [] in result
    | (state, accepting, (edges, fallback)) :: rest ->
      let copied_edges = copy_relation edges in
      let copied_rest = copy_table rest in
      let result = (state, accepting, (copied_edges, fallback)) ::
        copied_rest in
      result

  let (copy_machine @ total) (machine : machine) :
      {result : machine | result === machine} @ total =
    let initial, table = machine in
    let copied_table = copy_table table in
    let result = initial, copied_table in
    result

  let (quotient_of_raw_preserves @ total) (source : machine)
      (entries : (int * int list) list)
      (previous : (int * int) list) (stable : (int * int) list)
      (reduced : machine) (word : int list) :
      {u : unit | let initial, _ = source in
        let states = states_of_entries entries in
        let alphabet = collect_alphabet source states in
        let classes = collect_classes stable states in
        let raw = quotient_raw source stable states classes initial in
        if all_reach_closed source entries entries &&
          access_member initial entries &&
          accepting_partition source stable states &&
          partition_stable previous stable states &&
          stable === refine_partition source previous alphabet states &&
          of_raw raw === Some reduced then
          run reduced word === run source word else true} =
    let initial, _ = source in
    let states = states_of_entries entries in
    let classes = collect_classes stable states in
    let raw = quotient_raw source stable states classes initial in
    ghost_ (quotient_run source entries previous stable word);
    ghost_ (of_raw_run raw reduced word);
    ghost_ (raw_run_def raw word);
    let u = () in u

  type 'a stable_partition = {
    stable_value : 'a @@ total;
    prior_value : 'a Ghost.t @@ total;
  }

  let[@def] rec (refine_to_stable @ total) :
      (source : machine) -> (alphabet : int list) ->
      (states : int list) -> (partition : (int * int) list) @ total ->
      (measure : {n : Bigint.t | n === same_class_pairs partition states &&
        Bigint.compare 0Z n <= 0}) @ ghost ->
      {result : relation stable_partition |
        let previous = result.prior_value.ghost in
        let stable = result.stable_value in
        if accepting_partition source partition states then
          accepting_partition source previous states &&
          accepting_partition source stable states &&
          partition_stable previous stable states &&
          stable === refine_partition source previous alphabet states
        else true} @ total immutable contended =
    fun source alphabet states partition measure ->
    let _measure = measure in
    let original_next = refine_partition source partition alphabet states in
    let next = copy_relation original_next in
    ghost_ (same_class_pairs_nonnegative partition states);
    ghost_ (letters_in_refl states);
    ghost_ (refinement_stable_rows source partition alphabet states states);
    ghost_ (same_class_pairs_progress partition next states);
    ghost_ (refine_accepting_partition source partition alphabet states);
    if partition_stable partition next states then
      let result = { stable_value = next;
        prior_value = { ghost = ghost_ partition } } in result
    else
      let next_measure = ghost_ (same_class_pairs next states) in
      ghost_ (same_class_pairs_nonnegative next states);
      let result = refine_to_stable source alphabet states next
        (next_measure) in
      result
  [@@decreases let value = measure in value]

  let rec (refine_to_stable_respects @ total) :
      (source : machine) -> (alphabet : int list) ->
      (states : int list) -> (partition : relation) ->
      (measure : {n : Bigint.t | n === same_class_pairs partition states &&
        Bigint.compare 0Z n <= 0}) ->
      (entries : (int * int list) list) -> (relation : relation) ->
      {u : unit | let result =
          refine_to_stable source alphabet states partition measure in
        let stable = result.stable_value in
        if states === states_of_entries entries &&
          all_reach_closed source entries entries &&
          all_closed source source relation relation &&
          partition_respects partition states relation then
          partition_respects stable states relation else true}
        @ immutable contended =
    fun source alphabet states partition measure entries relation ->
    let _measure = measure in
    ghost_ (refine_to_stable_def source alphabet states partition measure);
    let original_next = refine_partition source partition alphabet states in
    let next = copy_relation original_next in
    ghost_ (relation_included_self relation);
    ghost_ (refine_partition_respects source entries partition relation relation
      alphabet);
    let u = () in
    if partition_stable partition next states then u
    else begin
      ghost_ (letters_in_refl states);
      ghost_ (refinement_stable_rows source partition alphabet states states);
      ghost_ (same_class_pairs_progress partition next states);
      let next_measure = same_class_pairs next states in
      ghost_ (same_class_pairs_nonnegative next states);
      let next_measure : {n : Bigint.t |
        n === same_class_pairs next states && Bigint.compare 0Z n <= 0} =
        next_measure in
      ghost_ (refine_to_stable_respects source alphabet states next
        next_measure entries relation);
      u
    end
  [@@decreases let value = measure in value]

  let rec (quotient_rows_valid @ total) :
      (source : machine) -> (entries : (int * int list) list) ->
      (previous : (int * int) list) -> (remaining : int list) ->
      {u : unit | let states = states_of_entries entries in
        let alphabet = collect_alphabet source states in
        let stable = refine_partition source previous alphabet states in
        let classes = collect_classes stable states in
        let table = quotient_table source stable alphabet classes in
        if all_reach_closed source entries entries &&
          letters_in remaining classes then
          rows_valid table (quotient_table source stable alphabet remaining)
        else true} @ immutable contended =
    fun source entries previous remaining ->
    let states = states_of_entries entries in
    let alphabet = collect_alphabet source states in
    let stable = refine_partition source previous alphabet states in
    let classes = collect_classes stable states in
    let table = quotient_table source stable alphabet classes in
    let rows = quotient_table source stable alphabet remaining in
    ghost_ (quotient_table_def source stable alphabet remaining);
    ghost_ (rows_valid_def table rows);
    ghost_ (letters_in_def remaining classes);
    let u = () in
    match remaining with
    | [] -> u
    | representative :: rest ->
      ghost_ (letters_in_refl states);
      ghost_ (collect_classes_representatives source previous alphabet states
        states representative);
      ghost_ (states_of_entries_member entries representative);
      ghost_ (all_reach_closed_member source entries entries representative);
      ghost_ (reach_state_closed_def source entries representative);
      let fallback = default source representative in
      let fallback_class = partition_class stable fallback in
      ghost_ (states_of_entries_member entries fallback);
      ghost_ (collect_classes_member stable states fallback);
      ghost_ (quotient_table_keys source stable alphabet classes fallback_class);
      ghost_ (collect_alphabet_distinct source states);
      ghost_ (quotient_edges_unique source stable representative alphabet);
      ghost_ (quotient_edges_targets source entries stable representative
        alphabet alphabet);
      ghost_ (letters_in_tail remaining classes);
      ghost_ (quotient_rows_valid source entries previous rest);
      u

  let (quotient_valid @ total) (source : machine)
      (entries : (int * int list) list) (previous : (int * int) list) :
      {u : unit | let initial, _ = source in
        let states = states_of_entries entries in
        let alphabet = collect_alphabet source states in
        let stable = refine_partition source previous alphabet states in
        let classes = collect_classes stable states in
        if all_reach_closed source entries entries &&
          access_member initial entries then
          valid (quotient_raw source stable states classes initial)
        else true} =
    let initial, _ = source in
    let states = states_of_entries entries in
    let alphabet = collect_alphabet source states in
    let stable = refine_partition source previous alphabet states in
    let classes = collect_classes stable states in
    let initial_class = partition_class stable initial in
    let raw = quotient_raw source stable states classes initial in
    ghost_ (states_of_entries_member entries initial);
    ghost_ (collect_classes_member stable states initial);
    ghost_ (quotient_table_keys source stable alphabet classes initial_class);
    ghost_ (collect_classes_distinct stable states);
    ghost_ (quotient_table_unique source stable alphabet classes);
    ghost_ (letters_in_refl classes);
    ghost_ (quotient_rows_valid source entries previous classes);
    ghost_ (quotient_raw_def source stable states classes initial);
    ghost_ (valid_def raw);
    let u = () in u

  let (propose_reduction @ total) (source : machine) (limit : int) :
      {result : (machine * reduction_certificate) option |
        (if valid source && labels_bounded source &&
          0 < limit && limit <= 64 &&
          Bigint.compare (state_size source) (Bigint.of_int limit) <= 0 then
          match result with None -> false | Some _ -> true else true) &&
        match result with
        | None -> true
        | Some (candidate, certificate) ->
          check_reduction source candidate certificate} =
    let initial, _ = source in
    if limit <= 0 || limit > 64 then
      let result = None in result
    else begin
      let search_result = reachable_search_initial source limit in
      match search_result with
      | None -> let result = None in result
      | Some entries ->
        let original_reachable = states_of_entries entries in
        let reachable = copy_word original_reachable in
        let original_alphabet = collect_alphabet source reachable in
        let alphabet = copy_word original_alphabet in
        let original_initial_classes = initial_partition source reachable in
        let initial_classes = copy_relation original_initial_classes in
        let refinement_measure = ghost_ (same_class_pairs initial_classes reachable) in
        ghost_ (same_class_pairs_nonnegative initial_classes reachable);
        let refinement_measure : {n : Bigint.t |
          n === same_class_pairs initial_classes reachable &&
          Bigint.compare 0Z n <= 0} = refinement_measure in
        let partition_result =
          refine_to_stable source alphabet reachable
            initial_classes refinement_measure in
        let previous = ghost_ partition_result.prior_value.ghost in
        let partition = partition_result.stable_value in
        let original_class_ids = collect_classes partition reachable in
        let class_ids = copy_word original_class_ids in
        let pair_limit = square_limit limit in
        ghost_ (separation_budget_from_limit source limit pair_limit);
        let original_candidate_raw =
          quotient_raw source partition reachable class_ids initial in
        let candidate_raw = copy_machine original_candidate_raw in
        ghost_ (initial_accepting_partition source reachable);
        ghost_ (quotient_valid source entries previous);
        ghost_ (of_raw_def candidate_raw);
        let reduced = candidate_raw in
        let original_equivalent =
          quotient_relation partition reachable in
        let equivalent =
          copy_relation original_equivalent in
        ghost_ (initial_accepting_partition source reachable);
        ghost_ (quotient_check_reduced source entries previous
          partition reduced);
        ghost_ (quotient_all_access_reduced source entries previous
          partition reduced);
        ghost_ (of_raw_valid candidate_raw);
        ghost_ (valid_def reduced);
        let original_access = quotient_access partition entries in
        let access = copy_access original_access in
        let empty = [] in
        ghost_ (all_separated_def source class_ids empty empty);
        let (respect @ total) (relation : relation) :
            {u : unit | if all_closed source source relation relation then
              partition_respects partition reachable relation else true} =
          ghost_ (initial_partition_respects source reachable relation
            relation);
          ghost_ (refine_to_stable_respects source alphabet reachable
            initial_classes refinement_measure entries relation);
          let u = () in u in
        let (distinguish @ total) (p : int) (q : int) :
            {result : int list option |
              (match result with None -> true | Some word ->
                run_from source p word <> run_from source q word) &&
              (if separation_budget source pair_limit &&
                has_letter p class_ids && has_letter q class_ids && p <> q
               then match result with None -> false | Some _ -> true
               else true)} =
          ghost_ (separation_budget_def source pair_limit);
          ghost_ (letters_in_refl reachable);
          ghost_ (collect_classes_representatives source previous alphabet
            reachable reachable p);
          ghost_ (collect_classes_representatives source previous alphabet
            reachable reachable q);
          ghost_ (collect_classes_fixed source previous alphabet reachable
            reachable p);
          ghost_ (collect_classes_fixed source previous alphabet reachable
            reachable q);
          ghost_ (states_of_entries_member entries p);
          ghost_ (states_of_entries_member entries q);
          ghost_ (access_member_valid source entries p);
          ghost_ (access_member_valid source entries q);
          let result = distinguish_classes source reachable
            partition respect p q pair_limit in
          result in
        ghost_ (letters_in_refl class_ids);
        let separation = cover_rows source class_ids
          class_ids empty pair_limit distinguish in
        (match separation with
         | None -> let result = None in result
         | Some separation ->
           let separation =
             copy_separations separation in
           ghost_ (quotient_all_separated_reduced source entries previous
             partition reduced separation);
           let certificate = equivalent, access, separation in
           ghost_ (check_reduction_def source reduced certificate);
           let result = Some (reduced, certificate) in
           result)
    end

  type ('a, 'proof) proved = {
    result_value : 'a @@ total;
    result_proof : 'proof Ghost.t @@ total;
  }

  let[@def] (minimize_proved @ total) (source : machine) (limit : int) :
      {result : (machine, reduction_certificate) proved option |
        (if valid source && labels_bounded source &&
          0 < limit && limit <= 64 &&
          Bigint.compare (state_size source) (Bigint.of_int limit) <= 0 then
          match result with None -> false | Some _ -> true else true) &&
        match result with
        | None -> true
        | Some packet -> valid packet.result_value &&
          check_reduction source packet.result_value packet.result_proof.ghost}
        @ total =
    let initial, _ = source in
    if limit <= 0 || limit > 64 || not (valid source) ||
      not (labels_bounded source) ||
      Bigint.compare (state_size source) (Bigint.of_int limit) > 0 then
      let result = None in result
    else begin
      let search_result = reachable_states_initial source limit in
      match search_result with
      | None -> let result = None in result
      | Some search ->
        let entries = ghost_ (Access_trace.entries search.seen_access.ghost) in
        ghost_ (state_search_valid_def search);
        ghost_ (state_search_view_def search);
        let reachable = search.seen_states in
        let original_alphabet = collect_alphabet source reachable in
        let alphabet = copy_word original_alphabet in
        let original_initial_classes = initial_partition source reachable in
        let initial_classes = copy_relation original_initial_classes in
        let refinement_measure = ghost_ (same_class_pairs initial_classes reachable) in
        ghost_ (same_class_pairs_nonnegative initial_classes reachable);
        let refinement_measure : {n : Bigint.t |
          n === same_class_pairs initial_classes reachable &&
          Bigint.compare 0Z n <= 0} = refinement_measure in
        let partition_result =
          refine_to_stable source alphabet reachable
            initial_classes refinement_measure in
        let previous = ghost_ partition_result.prior_value.ghost in
        let partition = partition_result.stable_value in
        let original_class_ids = collect_classes partition reachable in
        let class_ids = copy_word original_class_ids in
        let pair_limit = ghost_ (square_limit limit) in
        ghost_ (separation_budget_from_limit source limit pair_limit);
        let original_candidate_raw =
          quotient_raw source partition reachable class_ids initial in
        let candidate_raw = copy_machine original_candidate_raw in
        ghost_ (initial_accepting_partition source reachable);
        ghost_ (quotient_valid source entries previous);
        ghost_ (of_raw_def candidate_raw);
        let reduced = candidate_raw in
        let certificate = ghost_ (
        let original_equivalent =
          quotient_relation partition reachable in
        let equivalent =
          copy_relation original_equivalent in
        ghost_ (initial_accepting_partition source reachable);
        ghost_ (quotient_check_reduced source entries previous
          partition reduced);
        ghost_ (quotient_all_access_reduced source entries previous
          partition reduced);
        ghost_ (of_raw_valid candidate_raw);
        ghost_ (valid_def reduced);
        let original_access = quotient_access partition entries in
        let access = copy_access original_access in
        let empty = [] in
        ghost_ (all_separated_def source class_ids empty empty);
        let (respect @ total) (relation : relation) :
            {u : unit | if all_closed source source relation relation then
              partition_respects partition reachable relation else true} =
          ghost_ (initial_partition_respects source reachable relation
            relation);
          ghost_ (refine_to_stable_respects source alphabet reachable
            initial_classes refinement_measure entries relation);
          let u = () in u in
        let (distinguish @ total) (p : int) (q : int) :
            {result : int list option |
              (match result with None -> true | Some word ->
                run_from source p word <> run_from source q word) &&
              (if separation_budget source pair_limit &&
                has_letter p class_ids && has_letter q class_ids && p <> q
               then match result with None -> false | Some _ -> true
               else true)} =
          ghost_ (separation_budget_def source pair_limit);
          ghost_ (letters_in_refl reachable);
          ghost_ (collect_classes_representatives source previous alphabet
            reachable reachable p);
          ghost_ (collect_classes_representatives source previous alphabet
            reachable reachable q);
          ghost_ (collect_classes_fixed source previous alphabet reachable
            reachable p);
          ghost_ (collect_classes_fixed source previous alphabet reachable
            reachable q);
          ghost_ (states_of_entries_member entries p);
          ghost_ (states_of_entries_member entries q);
          ghost_ (access_member_valid source entries p);
          ghost_ (access_member_valid source entries q);
          let result = distinguish_classes source reachable
            partition respect p q pair_limit in
          result in
        ghost_ (letters_in_refl class_ids);
        let separation = cover_rows source class_ids
          class_ids empty pair_limit distinguish in
        (match separation with
         | None ->
           let certificate : reduction_certificate = [], [], [] in
           (certificate : {certificate : reduction_certificate |
             check_reduction source reduced certificate})
         | Some separation ->
           let separation =
             copy_separations separation in
           ghost_ (quotient_all_separated_reduced source entries previous
             partition reduced separation);
           let certificate = equivalent, access, separation in
           ghost_ (check_reduction_def source reduced certificate);
           (certificate : {certificate : reduction_certificate |
             check_reduction source reduced certificate}))
        ) in
        let packet = { result_value = reduced;
          result_proof = { ghost = certificate } } in
        let result = Some packet in result
    end

  let (diagnose_reduction @ total) (source : machine) (limit : int) :
      {result : (machine * reduction_certificate) option |
        (if valid source && labels_bounded source &&
          0 < limit && limit <= 64 &&
          Bigint.compare (state_size source) (Bigint.of_int limit) <= 0 then
          match result with None -> false | Some _ -> true else true) &&
        match result with
        | None -> true
        | Some (candidate, certificate) ->
          check_reduction source candidate certificate} =
    let proposal = propose_reduction source limit in
    proposal

  let[@def] (reduce @ total) (source : machine) (limit : int) :
      machine option @ total =
    let proposal = minimize_proved source limit in
    match proposal with
    | None -> None
    | Some packet -> Some packet.result_value

  let (reduce_complete @ total) (source : machine) (limit : int) :
      {u : unit | if valid source && labels_bounded source &&
        0 < limit && limit <= 64 &&
        Bigint.compare (state_size source) (Bigint.of_int limit) <= 0 then
        match reduce source limit with None -> false | Some candidate -> valid candidate
        else true} =
    let _proof = ghost_ (
ghost_ (reduce_def source limit);
    let proposal = minimize_proved source limit in
    let u = () in
    match proposal with None | Some _ -> u
      : {u : unit | if valid source && labels_bounded source &&
        0 < limit && limit <= 64 &&
        Bigint.compare (state_size source) (Bigint.of_int limit) <= 0 then
        match reduce source limit with None -> false | Some candidate -> valid candidate
        else true}) in
    let u = () in u

  let (reduce_preserves @ total) (source : machine) (limit : int)
      (word : int list) :
      {u : unit | let result = reduce source limit in
        match result with None -> true | Some candidate ->
          run source word === run candidate word} =
    let _proof = ghost_ (
      ghost_ (reduce_def source limit);
      let proposal = minimize_proved source limit in
      let u = () in
      (match proposal with
       | None -> u
       | Some packet ->
         let candidate = packet.result_value in
         let certificate = packet.result_proof.ghost in
         ghost_ (reduction_preserves source candidate certificate word);
         u)
      : {u : unit | let result = reduce source limit in
          match result with None -> true | Some candidate ->
            run source word === run candidate word}) in
    let u = () in u

  let (reduce_minimum @ total) (source : machine) (limit : int)
      (other : machine)
      (agreement : ((word : int list) ->
        {u : unit | run source word === run other word}) @ total) :
      {u : unit | let result = reduce source limit in
        match result with None -> true | Some candidate ->
          if valid other then
            Bigint.compare (state_size candidate) (state_size other) <= 0
          else true} =
    let _proof = ghost_ (
      ghost_ (reduce_def source limit);
      let proposal = minimize_proved source limit in
      let u = () in
      (match proposal with
       | None -> u
       | Some packet ->
         let candidate = packet.result_value in
         let certificate = packet.result_proof.ghost in
         ghost_ (minimum_count_source_semantic source candidate certificate
           other agreement);
         u)
      : {u : unit | let result = reduce source limit in
          match result with None -> true | Some candidate ->
            if valid other then
              Bigint.compare (state_size candidate) (state_size other) <= 0
            else true}) in
    let u = () in u
  type comparison = Equivalent | Inequivalent | Comparison_limit
  [@@inductive]

  let[@def] decision_kind decision =
    match decision with
    | Equal _ -> Equivalent
    | Different _ -> Inequivalent
    | Limit -> Comparison_limit

  module Pair_trace : sig
    type t : value mod immutable
    val entries : t -> (int * int * int list) list @@ total
    val make : (values : (int * int * int list) list) @ total ->
      {result : t | entries result === values} @ total @@ total
  end = struct
    type t = (int * int * int list) list
    let[@def] entries values = values
    let (make @ total) (values : (int * int * int list) list @ total) :
        {result : t | entries result === values} @ total =
      ghost_ (entries_def values);
      values
  end

  let[@def] rec pairs_of_pending pending =
    match pending with
    | [] -> []
    | (p, q, _) :: rest -> (p, q) :: pairs_of_pending rest

  type pair_search = {
    pending_pairs : relation @@ total;
    seen_pairs : relation @@ total;
    pair_count : int @@ total;
    pending_trace : Pair_trace.t Ghost.t @@ total;
  }

  let[@def] pair_search_valid (search : pair_search @ total) = ghost_ (
    search.pending_pairs ===
      pairs_of_pending (Pair_trace.entries search.pending_trace.ghost))

  let[@def] pair_search_view (search : pair_search @ total) = ghost_ (
    Pair_trace.entries search.pending_trace.ghost,
    search.seen_pairs, search.pair_count)

  let (push_search_pair @ total) (limit : int) (pair : state_pair @ total)
      (word : int list @ ghost) (before : pair_search @ total) :
      {result : pair_search option |
        if pair_search_valid before then
          let pending, seen, count = pair_search_view before in
          match result with
          | None -> push_pair limit pair word pending seen count === None
          | Some after -> pair_search_valid after &&
            push_pair limit pair word pending seen count ===
              Some (pair_search_view after)
        else true} @ total =
    let pending = ghost_ (Pair_trace.entries before.pending_trace.ghost) in
    let seen = before.seen_pairs in
    let count = before.pair_count in
    ghost_ (pair_search_valid_def before);
    ghost_ (pair_search_view_def before);
    ghost_ (push_pair_def limit pair word pending seen count);
    if pair_member pair seen then
      let result = Some before in result
    else if count >= limit then
      let result = None in result
    else
      let pending_access = ghost_ (
        let p, q = pair in (p, q, word) :: pending) in
      let trace = ghost_ (Pair_trace.make pending_access) in
      let after = {
        pending_pairs = pair :: before.pending_pairs;
        seen_pairs = pair :: seen;
        pair_count = count + 1;
        pending_trace = { ghost = trace };
      } in
      ghost_ (pairs_of_pending_def pending_access);
      ghost_ (pair_search_valid_def after);
      ghost_ (pair_search_view_def after);
      let result = Some after in result

  let rec (expand_search_pairs @ total) :
      (left : machine) -> (right : machine) -> (p : int) -> (q : int) ->
      (word : int list) @ ghost -> (letters : int list) ->
      (before : pair_search) @ total -> (limit : int) ->
      {result : pair_search option |
        if pair_search_valid before then
          let pending, seen, count = pair_search_view before in
          match result with
          | None -> push_labels left right p q word letters
              pending seen count limit === None
          | Some after -> pair_search_valid after &&
            push_labels left right p q word letters pending seen count limit
              === Some (pair_search_view after)
        else true} @ total immutable contended =
    fun left right p q word letters before limit ->
    let pending = ghost_ (Pair_trace.entries before.pending_trace.ghost) in
    let seen = before.seen_pairs in
    let count = before.pair_count in
    ghost_ (pair_search_view_def before);
    ghost_ (push_labels_def left right p q word letters pending seen count limit);
    match letters with
    | [] -> let result = Some before in result
    | letter :: rest ->
      let pair = step left p letter, step right q letter in
      let suffix = ghost_ [letter] in
      let next_word = ghost_ (append_word word suffix) in
      let pushed = push_search_pair limit pair next_word before in
      match pushed with
      | None -> let result = None in result
      | Some updated ->
        ghost_ (pair_search_view_def updated);
        let result = expand_search_pairs left right p q word rest
          updated limit in
        result

  let rec (search_pairs_loop @ total) :
      (left : machine) -> (right : machine) ->
      (before : pair_search) @ total ->
      (processed : relation) @ ghost -> (limit : int) ->
      (remaining : {fuel : int |
        let pending, seen, count = pair_search_view before in
        pair_search_valid before && 0 <= fuel && fuel <= 65_536 &&
        0 <= count && count <= limit && limit <= 65_536 &&
        Bigint.of_int count === big_length seen &&
        pending_valid left right pending && distinct_pairs seen &&
        (if valid left && valid right then pairs_valid left right seen else true) &&
        Bigint.add (Bigint.of_int fuel) (Bigint.of_int count) ===
          Bigint.add (Bigint.of_int limit) (big_length pending)}) @ ghost ->
      {packet : (comparison, decision) proved |
        let pending, seen, _ = pair_search_view before in
        let decision = packet.result_proof.ghost in
        packet.result_value === decision_kind decision &&
        (if valid left && valid right && labels_bounded left && labels_bounded right &&
          Bigint.compare (Bigint.mul (state_size left) (state_size right))
            (Bigint.of_int limit) <= 0 then
          match decision with Limit -> false | Equal _ | Different _ -> true
         else true) &&
        (match decision with
         | Equal relation -> distinct_pairs relation &&
           (if valid left && valid right then pairs_valid left right relation
            else true)
         | Different _ | Limit -> true) &&
        (match pending with
         | [] -> decision === Equal seen
         | _ :: _ -> true) &&
        let left_initial, _ = left in
        let right_initial, _ = right in
        if pending_valid left right pending &&
          all_seen_accounted seen processed pending &&
          all_closed left right seen processed &&
          related (left_initial, right_initial) seen then
        match decision with
        | Different word -> run left word <> run right word
        | Equal relation -> check left right relation
        | Limit -> true
        else true} @ total immutable contended =
    fun left right before processed limit remaining ->
    let pending = ghost_ (Pair_trace.entries before.pending_trace.ghost) in
    let seen = before.seen_pairs in
    let count = before.pair_count in
    ghost_ (pair_search_valid_def before);
    ghost_ (pair_search_view_def before);
    ghost_ (pairs_of_pending_def pending);
    match before.pending_pairs with
    | [] ->
      ghost_ (accounted_empty_included seen processed);
      ghost_ (closed_processed_covers_seen left right seen processed seen);
      ghost_ (check_def left right seen);
      let decision = ghost_ (Equal seen) in
      ghost_ (decision_kind_def decision);
      let packet = { result_value = Equivalent; result_proof = { ghost = decision } } in
      packet
    | (p, q) :: rest_pairs ->
      let word = ghost_ (match pending with [] -> [] | (_, _, word) :: _ -> word) in
      let rest = ghost_ (match pending with [] -> [] | _ :: rest -> rest) in
      let trace = ghost_ (Pair_trace.make rest) in
      let popped = { before with pending_pairs = rest_pairs;
        pending_trace = { ghost = trace } } in
      ghost_ (pair_search_valid_def popped);
      ghost_ (pair_search_view_def popped);
      ghost_ (big_length_def pending);
      ghost_ (product_pending_length_nonnegative rest);
      ghost_ (pending_valid_def left right pending);
      ghost_ (reached_valid left word);
      ghost_ (reached_valid right word);
      ghost_ (labels_bounded_state left p);
      ghost_ (labels_bounded_state right q);
      if final left p <> final right q then
        let left_initial, _ = left in
        let right_initial, _ = right in
        ghost_ (reached_def left word);
        ghost_ (reached_def right word);
        ghost_ (execute_reached left left_initial word);
        ghost_ (execute_reached right right_initial word);
        ghost_ (run_def left word);
        ghost_ (run_def right word);
        let decision = ghost_ (Different word) in
        ghost_ (decision_kind_def decision);
        let packet = { result_value = Inequivalent; result_proof = { ghost = decision } } in
        packet
      else
        let left_labels = labels left p in
        let right_labels = labels right q in
        if list_size left_labels > 64 || list_size right_labels > 64
        then let decision = ghost_ (Limit) in
          ghost_ (decision_kind_def decision);
          let packet = { result_value = Comparison_limit; result_proof = { ghost = decision } } in
          packet
        else
          let letters = append left_labels right_labels in
          let zero = 0 in
          let missing_budget = list_size letters + 1 in
          ghost_ (pair_labels_size left_labels right_labels);
          ghost_ (missing_letter_budget letters);
          ghost_ (missing_letter_sound letters zero missing_budget);
          match missing_letter letters zero missing_budget with
          | None -> let decision = ghost_ (Limit) in
            ghost_ (decision_kind_def decision);
            let packet = { result_value = Comparison_limit; result_proof = { ghost = decision } } in
            packet
          | Some outsider ->
            let new_processed = ghost_ ((p, q) :: processed) in
            ghost_ (accounted_pop seen processed p q word rest);
            ghost_ (push_labels_accounted left right p q word letters
              rest seen new_processed count limit);
            ghost_ (push_labels_counts left right p q word letters
              rest seen count limit);
            ghost_ (push_labels_distinct left right p q word letters
              rest seen count limit);
            ghost_ (push_labels_domain left right p q word letters
              rest seen count limit);
            ghost_ (push_labels_capacity left right p q word letters
              rest seen count limit);
            ghost_ (push_labels_valid left right p q word letters
              rest seen count limit);
            ghost_ (push_labels_seen_included left right p q word letters
              rest seen count limit);
            ghost_ (expanded_pair_closed left right p q word outsider
              rest seen count limit);
            let expanded = expand_search_pairs left right p q word letters popped limit in
            match expanded with
            | None -> let decision = ghost_ (Limit) in
              ghost_ (decision_kind_def decision);
              let packet = { result_value = Comparison_limit; result_proof = { ghost = decision } } in
              packet
            | Some updated ->
              let updated_pending = ghost_ (Pair_trace.entries updated.pending_trace.ghost) in
              let updated_seen = updated.seen_pairs in
              let updated_count = updated.pair_count in
              ghost_ (pair_search_view_def updated);
              let pair = step left p outsider, step right q outsider in
              let suffix = ghost_ [outsider] in
              let next_word = ghost_ (append_word word suffix) in
              ghost_ (reached_push left word outsider);
              ghost_ (reached_push right word outsider);
              ghost_ (push_pair_accounted limit pair next_word
                updated_pending updated_seen new_processed updated_count);
              ghost_ (push_pair_seen_included limit pair next_word
                updated_pending updated_seen updated_count);
              ghost_ (push_pair_valid left right limit pair next_word
                updated_pending updated_seen updated_count);
              ghost_ (push_pair_counts limit pair next_word
                updated_pending updated_seen updated_count);
              ghost_ (push_pair_distinct limit pair next_word
                updated_pending updated_seen updated_count);
              ghost_ (step_valid left p outsider);
              ghost_ (step_valid right q outsider);
              ghost_ (push_pair_domain left right limit pair next_word
                updated_pending updated_seen updated_count);
              ghost_ (push_pair_capacity left right limit pair next_word
                updated_pending updated_seen updated_count);
              let pushed = push_search_pair limit pair next_word updated in
              match pushed with
              | None -> let decision = ghost_ (Limit) in
                ghost_ (decision_kind_def decision);
                let packet = { result_value = Comparison_limit; result_proof = { ghost = decision } } in
                packet
              | Some next ->
                let next_seen = next.seen_pairs in
                ghost_ (pair_search_valid_def next);
                ghost_ (pair_search_view_def next);
                ghost_ (relation_included_trans seen updated_seen next_seen);
                ghost_ (all_closed_weaken left right seen next_seen processed);
                ghost_ (all_closed_def left right next_seen new_processed);
                let left_initial, _ = left in
                let right_initial, _ = right in
                let initial_pair = ghost_ (left_initial, right_initial) in
                ghost_ (related_included initial_pair seen next_seen);
                let next_remaining = ghost_ (remaining - 1) in
                let packet =
                  search_pairs_loop left right next new_processed
                    limit (next_remaining) in
                packet
  [@@decreases let fuel = remaining in fuel]


  let (comparison_proved @ total) (left : machine) (right : machine) (limit : int) :
      {packet : (comparison, decision) proved |
        let decision = packet.result_proof.ghost in
        packet.result_value === decision_kind decision &&
        (if valid left && valid right && labels_bounded left && labels_bounded right &&
          0 < limit && limit <= 65_536 &&
          Bigint.compare (Bigint.mul (state_size left) (state_size right))
            (Bigint.of_int limit) <= 0 then
          match decision with Limit -> false | Equal _ | Different _ -> true
         else true) &&
        match decision with
        | Different word -> run left word <> run right word
        | Equal relation -> check left right relation
        | Limit -> true} =
    let left_initial, _ = left in
    let right_initial, _ = right in
    let pair = left_initial, right_initial in
    let count = if limit > 65_536 then 65_536 else limit in
    if count <= 0 then let decision = ghost_ Limit in
      ghost_ (decision_kind_def decision);
      let packet = { result_value = Comparison_limit; result_proof = { ghost = decision } } in
      packet
    else
      let empty_word = ghost_ [] in
      let empty_pending = ghost_ ([] : (int * int * int list) list) in
      let empty_seen : relation = [] in
      let empty_processed = ghost_ ([] : relation) in
      let zero = 0 in
      let remaining = ghost_ count in
      let trace = ghost_ (Pair_trace.make empty_pending) in
      let start = { pending_pairs = empty_seen; seen_pairs = empty_seen;
        pair_count = zero; pending_trace = { ghost = trace } } in
      ghost_ (pairs_of_pending_def empty_pending);
      ghost_ (pair_search_valid_def start);
      ghost_ (pair_search_view_def start);
      ghost_ (big_length_def empty_pending);
      ghost_ (big_length_def empty_seen);
      ghost_ (distinct_pairs_def empty_seen);
      ghost_ (pairs_valid_def left right empty_seen);
      ghost_ (pending_valid_def left right empty_pending);
      ghost_ (all_seen_accounted_def empty_seen empty_processed empty_pending);
      ghost_ (reached_empty_equal left);
      ghost_ (reached_empty_equal right);
      ghost_ (reached_valid left empty_word);
      ghost_ (reached_valid right empty_word);
      ghost_ (push_pair_valid left right count pair empty_word empty_pending
        empty_seen zero);
      ghost_ (push_pair_accounted count pair empty_word empty_pending empty_seen
        empty_processed zero);
      ghost_ (push_pair_related count pair empty_word empty_pending empty_seen zero);
      ghost_ (push_pair_counts count pair empty_word empty_pending empty_seen zero);
      ghost_ (push_pair_distinct count pair empty_word empty_pending empty_seen zero);
      ghost_ (push_pair_domain left right count pair empty_word empty_pending empty_seen zero);
      ghost_ (pair_member_def pair empty_seen);
      ghost_ (push_pair_def count pair empty_word empty_pending empty_seen zero);
      let pushed = push_search_pair count pair empty_word start in
      match pushed with
      | None -> let decision = ghost_ Limit in
        ghost_ (decision_kind_def decision);
        let packet = { result_value = Comparison_limit; result_proof = { ghost = decision } } in
        packet
      | Some before ->
        let pending = ghost_ (Pair_trace.entries before.pending_trace.ghost) in
        let seen = before.seen_pairs in
        ghost_ (pair_search_valid_def before);
        ghost_ (pair_search_view_def before);
        ghost_ (big_length_def pending);
        ghost_ (big_length_def seen);
        ghost_ (all_closed_def left right seen empty_processed);
        let packet = search_pairs_loop left right before empty_processed
          count (remaining) in
        packet



  let[@def] (compare @ total) (left : machine) (right : machine) (limit : int) :
      comparison @ total =
    let packet = comparison_proved left right limit in
    packet.result_value

  let (compare_complete @ total) (left : machine) (right : machine) (limit : int) :
    {u : unit | if valid left && valid right && labels_bounded left && labels_bounded right &&
      0 < limit && limit <= 65_536 &&
      Bigint.compare (Bigint.mul (state_size left) (state_size right))
        (Bigint.of_int limit) <= 0 then
      (match compare left right limit with Comparison_limit -> false
         | Equivalent | Inequivalent -> true) else true} =
    let _proof = ghost_ (
      ghost_ (compare_def left right limit);
      let packet = comparison_proved left right limit in
      let decision = packet.result_proof.ghost in
      ghost_ (decision_kind_def decision);
      let u = () in
      (match decision with Equal _ | Different _ | Limit -> u)
      : {u : unit | if valid left && valid right && labels_bounded left && labels_bounded right &&
        0 < limit && limit <= 65_536 &&
        Bigint.compare (Bigint.mul (state_size left) (state_size right))
          (Bigint.of_int limit) <= 0 then
        (match compare left right limit with Comparison_limit -> false
         | Equivalent | Inequivalent -> true) else true}) in
    let u = () in u

  let (compare_equal @ total) (left : machine) (right : machine) (limit : int)
      (word : int list) :
    {u : unit | if compare left right limit === Equivalent then
      run left word === run right word else true} =
    let _proof = ghost_ (
      ghost_ (compare_def left right limit);
      let packet = comparison_proved left right limit in
      let decision = packet.result_proof.ghost in
      ghost_ (decision_kind_def decision);
      let u = () in
      (match decision with
       | Equal relation -> ghost_ (check_agrees left right relation word); u
       | Different _ | Limit -> u)
      : {u : unit | if compare left right limit === Equivalent then
        run left word === run right word else true}) in
    let u = () in u

  let (comparison_witness @ total) (left : machine) (right : machine) (limit : int) :
    {witness : int list Ghost.t | if compare left right limit === Inequivalent then
      run left witness.ghost <> run right witness.ghost else true} =
    let word = ghost_ (
      ghost_ (compare_def left right limit);
      let packet = comparison_proved left right limit in
      let decision = packet.result_proof.ghost in
      ghost_ (decision_kind_def decision);
      (match decision with
       | Different word -> word
       | Equal _ | Limit -> let word = [] in word)
      : {word : int list | if compare left right limit === Inequivalent then
        run left word <> run right word else true}) in
    let witness : int list Ghost.t = { ghost = word } in witness

  let valid = Dfa_semantics.valid
  let run = Dfa_semantics.run
  let state_size = Dfa_semantics.state_size
  let labels_bounded = Dfa_semantics.labels_bounded
  let has_state = Dfa_semantics.has_state
  let final = Dfa_semantics.final
  let step = Dfa_semantics.step
  let (valid_semantics @ total) (machine : machine) :
      {u : unit | valid machine === Dfa_semantics.valid machine} =
    let u = () in u

  let (run_semantics @ total) (machine : machine) (word : int list) :
      {u : unit | run machine word === Dfa_semantics.run machine word} =
    let u = () in u

  let (state_size_semantics @ total) (machine : machine) :
      {u : unit | state_size machine === Dfa_semantics.state_size machine} =
    let u = () in u

  let (labels_bounded_semantics @ total) (machine : machine) :
      {u : unit | labels_bounded machine === Dfa_semantics.labels_bounded machine} =
    let u = () in u

end;;

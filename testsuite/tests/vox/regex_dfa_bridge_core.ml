module Regex_dfa_bridge : sig
  val lower : Regex.Dfa.automaton -> Dfa_proof.machine option @@ total
  val lower_compiled_matches : (root : Regex.t) -> (word : int list) ->
    {u : unit | match lower (Regex.Dfa.compile root) with
      | None -> true
      | Some machine -> Dfa_semantics.run machine word ===
        Regex.matches root word} @@ total
end = struct
  open Regex.Dfa

  let rec (simulation_run_from @ total) :
      (dfa : automaton) -> (raw : Dfa_proof.raw) ->
      (source_of : (int -> state)) @ total ->
      (output_agrees : ((id : int) ->
        {u : unit | Dfa_proof.raw_final raw id ===
          output dfa (source_of id)})) @ total ->
      (step_agrees : ((id : int) -> (letter : int) ->
        {u : unit | source_of (Dfa_proof.raw_step raw id letter)
          === next dfa (source_of id) letter})) @ total ->
      (id : int) -> (word : int list) ->
      {u : unit | Dfa_proof.raw_run_from raw id word ===
        run_from dfa (source_of id) word} @ immutable contended =
    fun dfa raw source_of output_agrees step_agrees id word ->
    let source = source_of id in
    let u = () in
    match word with
    | [] ->
      output_agrees id;
      Dfa_proof.raw_run_from_empty raw id;
      run_from_empty dfa source;
      u
    | letter :: suffix ->
      let target = Dfa_proof.raw_step raw id letter in
      step_agrees id letter;
      Dfa_proof.raw_run_from_letter raw id letter suffix;
      run_from_letter dfa source letter suffix;
      simulation_run_from dfa raw source_of output_agrees step_agrees
        target suffix;
      u

  let[@def] rec index_from state states index =
    match states with
    | [] -> None
    | head :: rest ->
      if same_state state head then Some index
      else index_from state rest (index + 1)

  let[@def] index state states = index_from state states 0

  let[@def] member_state state states = contains_in state states

  let rec (index_from_complete @ total) :
      (state : state) -> (states : state list) -> (start : int) ->
      {u : unit | if member_state state states then
        match index_from state states start with
        | None -> false
        | Some _ -> true
        else true} @ immutable contended =
    fun state states start ->
    member_state_def state states;
    index_from_def state states start;
    let u = () in
    match states with
    | [] ->
      contains_in_empty state;
      u
    | head :: rest ->
      contains_in_cons state head rest;
      if same_state state head then u
      else begin
        let next_start = start + 1 in
        member_state_def state rest;
        index_from_complete state rest next_start;
        u
      end

  let (index_complete @ total) state states :
      {u : unit | if member_state state states then
        match index state states with
        | None -> false
        | Some _ -> true
        else true} =
    let zero = 0 in
    index_from_complete state states zero;
    index_def state states;
    let u = () in u

  let (member_prefix @ total) (dfa : automaton) (source : state) :
      {u : unit | let all_states = initial dfa :: [] :: states dfa in
        if contains_state dfa source then
          member_state source all_states else true} =
    let initial_state = initial dfa in
    let nil = [] in
    let source_states = states dfa in
    let tail = nil :: source_states in
    let all_states = initial_state :: tail in
    contains_state_equation dfa source;
    contains_in_cons source initial_state tail;
    contains_in_cons source nil source_states;
    member_state_def source all_states;
    let u = () in u

  let (empty_prefix @ total) (dfa : automaton) :
      {u : unit | member_state [] (initial dfa :: [] :: states dfa)} =
    let initial_state = initial dfa in
    let nil = [] in
    let source_states = states dfa in
    let tail = nil :: source_states in
    let all_states = initial_state :: tail in
    contains_in_cons nil initial_state tail;
    contains_in_cons nil nil source_states;
    same_state_correct nil nil;
    member_state_def nil all_states;
    let u = () in u

  let (initial_index @ total) (dfa : automaton) :
      {u : unit | index (initial dfa)
        (initial dfa :: [] :: states dfa) === Some 0} =
    let source = initial dfa in
    let all_states = source :: [] :: states dfa in
    let zero = 0 in
    same_state_correct source source;
    index_def source all_states;
    index_from_def source all_states zero;
    let u = () in u

  let (empty_index @ total) (dfa : automaton) :
      {u : unit | let all_states = initial dfa :: [] :: states dfa in
        match index [] all_states with
        | None -> false
        | Some id -> id = 0 || id = 1} =
    let source = initial dfa in
    let empty = [] in
    let all_states = source :: empty :: states dfa in
    let tail = empty :: states dfa in
    let zero = 0 in
    let one = 1 in
    same_state_correct empty source;
    same_state_correct empty empty;
    index_def empty all_states;
    index_from_def empty all_states zero;
    index_from_def empty tail one;
    let u = () in u

  let[@def] rec has_letter (letter : int) (letters : int list) =
    match letters with
    | [] -> false
    | head :: rest -> letter = head || has_letter letter rest

  let rec (has_letter_agrees @ total) :
      (letter : int) -> (letters : int list) ->
      {u : unit | has_letter letter letters === label_member letter letters}
        @ immutable contended =
    fun letter letters ->
    has_letter_def letter letters;
    let u = () in
    match letters with
    | [] ->
      label_member_empty letter;
      u
    | head :: rest ->
      label_member_cons letter head rest;
      has_letter_agrees letter rest;
      u

  let[@def] rec distinct_letters letters seen =
    match letters with
    | [] -> []
    | head :: rest ->
      if has_letter head seen then distinct_letters rest seen
      else head :: distinct_letters rest (head :: seen)

  let rec (distinct_letters_member @ total) :
      (letters : int list) -> (seen : int list) -> (letter : int) ->
      {u : unit | has_letter letter (distinct_letters letters seen) ===
        (has_letter letter letters && not (has_letter letter seen))}
        @ immutable contended =
    fun letters seen letter ->
    distinct_letters_def letters seen;
    has_letter_def letter letters;
    let output = distinct_letters letters seen in
    has_letter_def letter output;
    let u = () in
    match letters with
    | [] -> u
    | head :: rest ->
      if has_letter head seen then begin
        distinct_letters_member rest seen letter;
        u
      end else begin
        let extended = head :: seen in
        distinct_letters_member rest extended letter;
        has_letter_def letter extended;
        u
      end

  let[@def] rec unique_letters letters =
    match letters with
    | [] -> true
    | head :: rest -> not (has_letter head rest) && unique_letters rest

  let rec (distinct_letters_unique @ total) :
      (letters : int list) -> (seen : int list) ->
      {u : unit | unique_letters (distinct_letters letters seen)}
        @ immutable contended =
    fun letters seen ->
    let output = distinct_letters letters seen in
    distinct_letters_def letters seen;
    unique_letters_def output;
    let u = () in
    match letters with
    | [] -> u
    | head :: rest ->
      if has_letter head seen then begin
        distinct_letters_unique rest seen;
        u
      end else begin
        let extended = head :: seen in
        let tail = distinct_letters rest extended in
        distinct_letters_unique rest extended;
        distinct_letters_member rest extended head;
        has_letter_def head extended;
        has_letter_def head tail;
        unique_letters_def tail;
        u
      end

  let (labels_outside_next @ total) (dfa : automaton)
      (source : state) (letter : int) :
      {u : unit | if not (has_letter letter
        (distinct_letters (labels dfa source) [])) then
        next dfa source letter === [] else true} =
    let source_labels = labels dfa source in
    let nil = [] in
    distinct_letters_member source_labels nil letter;
    has_letter_def letter nil;
    has_letter_agrees letter source_labels;
    next_outside_labels dfa source letter;
    let u = () in u

  let[@def] rec build_edges dfa states source letters =
    match letters with
    | [] -> Some []
    | letter :: rest ->
      match index (next dfa source letter) states,
        build_edges dfa states source rest with
      | Some target, Some edges -> Some ((letter, target) :: edges)
      | _ -> None

  let[@def] rec edge_lookup edges fallback (letter : int) =
    match edges with
    | [] -> fallback
    | (label, target) :: rest ->
      if label = letter then target else edge_lookup rest fallback letter

  let rec (edge_lookup_agrees @ total) :
      (edges : (int * int) list) -> (fallback : int) ->
      (letter : int) ->
      {u : unit | edge_lookup edges fallback letter ===
        Dfa_proof.raw_edge_step edges fallback letter}
        @ immutable contended =
    fun edges fallback letter ->
    edge_lookup_def edges fallback letter;
    let u = () in
    match edges with
    | [] ->
      Dfa_proof.raw_edge_step_empty fallback letter;
      u
    | (label, target) :: rest ->
      Dfa_proof.raw_edge_step_cons label target rest fallback letter;
      edge_lookup_agrees rest fallback letter;
      u

  let[@def] target_index dfa states source fallback letter =
    match index (next dfa source letter) states with
    | None -> fallback
    | Some id -> id

  let[@def] next_index dfa states source letter =
    target_index dfa states source 0 letter

  let rec (build_edges_step @ total) :
      (dfa : automaton) -> (states : state list) ->
      (source : state) -> (letters : int list) ->
      (fallback : int) -> (letter : int) ->
      {u : unit | match build_edges dfa states source letters with
        | None -> true
        | Some edges -> edge_lookup edges fallback letter ===
          (if has_letter letter letters then
            target_index dfa states source fallback letter
           else fallback)} @ immutable contended =
    fun dfa states source letters fallback letter ->
    build_edges_def dfa states source letters;
    has_letter_def letter letters;
    let u = () in
    match letters with
    | [] ->
      let nil = [] in
      edge_lookup_def nil fallback letter;
      u
    | head :: rest ->
      let head_target = next dfa source head in
      let head_index = index head_target states in
      let tail_edges = build_edges dfa states source rest in
      if head = letter then begin
        target_index_def dfa states source fallback letter;
        (match head_index, tail_edges with
         | Some target, Some edges ->
           let row = (head, target) :: edges in
           edge_lookup_def row fallback letter;
           u
         | _ -> u)
      end else begin
        build_edges_step dfa states source rest fallback letter;
        (match head_index, tail_edges with
         | Some target, Some edges ->
           let row = (head, target) :: edges in
           edge_lookup_def row fallback letter;
           u
         | _ -> u)
      end

  let (built_row_step @ total) (dfa : automaton)
      (all_states : state list) (source : state) (letter : int) :
      {u : unit | let letters =
          distinct_letters (labels dfa source) [] in
        match index (default dfa source) all_states,
          build_edges dfa all_states source letters with
        | Some fallback, Some edges ->
          edge_lookup edges fallback letter ===
            target_index dfa all_states source fallback letter
        | _ -> true} =
    let letters = distinct_letters (labels dfa source) [] in
    let fallback_index = index (default dfa source) all_states in
    let built_edges = build_edges dfa all_states source letters in
    let u = () in
    match fallback_index, built_edges with
    | Some fallback, Some edges ->
      build_edges_step dfa all_states source letters fallback letter;
      if has_letter letter letters then u
      else begin
        default_empty dfa source;
        labels_outside_next dfa source letter;
        target_index_def dfa all_states source fallback letter;
        u
      end
    | _ -> u

  let (built_head_final @ total) (dfa : automaton)
      (source : state) (id : int) (row : Dfa_proof.row)
      (tail : (int * bool * Dfa_proof.row) list) :
      {u : unit | let raw =
          id, (id, output dfa source, row) :: tail in
        Dfa_proof.raw_final raw id === output dfa source} =
    let accepting = output dfa source in
    let table = (id, accepting, row) :: tail in
    let raw = id, table in
    Dfa_proof.raw_view_cons id accepting row tail id;
    Dfa_proof.raw_final_view raw id;
    let u = () in u

  let (built_head_step @ total) (dfa : automaton)
      (all_states : state list) (source : state) (id : int)
      (tail : (int * bool * Dfa_proof.row) list)
      (letter : int) :
      {u : unit | let letters =
          distinct_letters (labels dfa source) [] in
        match index (default dfa source) all_states,
          build_edges dfa all_states source letters with
        | Some fallback, Some edges ->
          let raw = id, (id, output dfa source,
            (edges, fallback)) :: tail in
          Dfa_proof.raw_step raw id letter ===
            target_index dfa all_states source fallback letter
        | _ -> true} =
    let letters = distinct_letters (labels dfa source) [] in
    let fallback_index = index (default dfa source) all_states in
    let built_edges = build_edges dfa all_states source letters in
    let u = () in
    match fallback_index, built_edges with
    | Some fallback, Some edges ->
      let accepting = output dfa source in
      let row = edges, fallback in
      let table = (id, accepting, row) :: tail in
      let raw = id, table in
      built_row_step dfa all_states source letter;
      edge_lookup_agrees edges fallback letter;
      Dfa_proof.raw_view_cons id accepting row tail id;
      Dfa_proof.raw_row_step_edges edges fallback letter;
      Dfa_proof.raw_step_view raw id letter;
      u
    | _ -> u

  let (built_row_index_step @ total) (dfa : automaton)
      (all_states : state list) (source : state) (letter : int) :
      {u : unit | let letters =
          distinct_letters (labels dfa source) [] in
        if member_state (next dfa source letter) all_states then
          match index (default dfa source) all_states,
            build_edges dfa all_states source letters with
          | Some fallback, Some edges ->
            Dfa_proof.raw_row_step (edges, fallback) letter ===
              next_index dfa all_states source letter
          | _ -> true
        else true} =
    let letters = distinct_letters (labels dfa source) [] in
    let fallback_index = index (default dfa source) all_states in
    let built_edges = build_edges dfa all_states source letters in
    let target = next dfa source letter in
    let zero = 0 in
    let u = () in
    match fallback_index, built_edges with
    | Some fallback, Some edges ->
      built_row_step dfa all_states source letter;
      edge_lookup_agrees edges fallback letter;
      Dfa_proof.raw_row_step_edges edges fallback letter;
      index_complete target all_states;
      target_index_def dfa all_states source fallback letter;
      target_index_def dfa all_states source zero letter;
      next_index_def dfa all_states source letter;
      u
    | _ -> u

  let[@def] rec edges_closed dfa states source letters =
    match letters with
    | [] -> true
    | letter :: rest ->
      member_state (next dfa source letter) states &&
      edges_closed dfa states source rest

  let rec (source_edges_closed @ total) :
      (dfa : automaton) ->
      (closure : ((source : state) -> (letter : int) ->
        {u : unit | if contains_state dfa source then
          contains_state dfa (next dfa source letter) else true})) @ total ->
      (source : state) -> (letters : int list) ->
      {u : unit | let all_states = initial dfa :: [] :: states dfa in
        if contains_state dfa source then
          edges_closed dfa all_states source letters else true}
        @ immutable contended =
    fun dfa closure source letters ->
    let all_states = initial dfa :: [] :: states dfa in
    edges_closed_def dfa all_states source letters;
    let u = () in
    match letters with
    | [] -> u
    | letter :: rest ->
      let target = next dfa source letter in
      closure source letter;
      member_prefix dfa target;
      source_edges_closed dfa closure source rest;
      u

  let rec (build_edges_complete @ total) :
      (dfa : automaton) -> (states : state list) ->
      (source : state) -> (letters : int list) ->
      {u : unit | if edges_closed dfa states source letters then
        match build_edges dfa states source letters with
        | None -> false
        | Some _ -> true
        else true} @ immutable contended =
    fun dfa states source letters ->
    edges_closed_def dfa states source letters;
    build_edges_def dfa states source letters;
    let u = () in
    match letters with
    | [] -> u
    | letter :: rest ->
      let target = next dfa source letter in
      index_complete target states;
      build_edges_complete dfa states source rest;
      u

  let[@def] rec build_table dfa all_states remaining id =
    match remaining with
    | [] -> Some []
    | source :: rest ->
      let letters = distinct_letters (labels dfa source) [] in
      match index (default dfa source) all_states,
        build_edges dfa all_states source letters,
        build_table dfa all_states rest (id + 1) with
      | Some fallback, Some edges, Some table ->
        Some ((id, output dfa source, (edges, fallback)) :: table)
      | _ -> None

  let rec (index_in_built_table @ total) :
      (dfa : automaton) -> (all_states : state list) ->
      (remaining : state list) -> (start : int) -> (source : state) ->
      {u : unit | match index_from source remaining start,
          build_table dfa all_states remaining start with
        | Some target, Some table ->
          Dfa_proof.raw_has_key table target
        | _ -> true} @ immutable contended =
    fun dfa all_states remaining start source ->
    index_from_def source remaining start;
    build_table_def dfa all_states remaining start;
    let u = () in
    match remaining with
    | [] -> u
    | head :: rest ->
      let letters = distinct_letters (labels dfa head) [] in
      let fallback_index = index (default dfa head) all_states in
      let built_edges = build_edges dfa all_states head letters in
      let next_start = start + 1 in
      let tail_table = build_table dfa all_states rest next_start in
      if same_state source head then begin
        match fallback_index, built_edges, tail_table with
        | Some fallback, Some edges, Some tail ->
          let accepting = output dfa head in
          let row = edges, fallback in
          Dfa_proof.raw_has_key_cons start accepting row tail start;
          u
        | _ -> u
      end else begin
        index_in_built_table dfa all_states rest next_start source;
        match index_from source rest next_start,
          fallback_index, built_edges, tail_table with
        | Some target, Some fallback, Some edges, Some tail ->
          let accepting = output dfa head in
          let row = edges, fallback in
          Dfa_proof.raw_has_key_cons start accepting row tail target;
          u
        | _ -> u
      end

  let rec (indexed_output_sound @ total) :
      (dfa : automaton) -> (all_states : state list) ->
      (remaining : state list) -> (start : int) -> (source : state) ->
      {u : unit | match index_from source remaining start,
          build_table dfa all_states remaining start with
        | Some target, Some table ->
          if Dfa_proof.raw_unique_keys table then
            let accepting, _ = Dfa_proof.raw_view table target in
            accepting === output dfa source
          else true
        | _ -> true} @ immutable contended =
    fun dfa all_states remaining start source ->
    index_from_def source remaining start;
    build_table_def dfa all_states remaining start;
    let u = () in
    match remaining with
    | [] -> u
    | head :: rest ->
      let letters = distinct_letters (labels dfa head) [] in
      let fallback_index = index (default dfa head) all_states in
      let built_edges = build_edges dfa all_states head letters in
      let next_start = start + 1 in
      let tail_table = build_table dfa all_states rest next_start in
      if same_state source head then begin
        same_state_correct source head;
        match fallback_index, built_edges, tail_table with
        | Some fallback, Some edges, Some tail ->
          let accepting = output dfa head in
          let row = edges, fallback in
          Dfa_proof.raw_view_cons start accepting row tail start;
          u
        | _ -> u
      end else begin
        indexed_output_sound dfa all_states rest next_start source;
        index_in_built_table dfa all_states rest next_start source;
        match index_from source rest next_start,
          fallback_index, built_edges, tail_table with
        | Some target, Some fallback, Some edges, Some tail ->
          let accepting = output dfa head in
          let row = edges, fallback in
          Dfa_proof.raw_unique_tail start accepting row tail;
          Dfa_proof.raw_tail_key_distinct
            start accepting row tail target;
          Dfa_proof.raw_view_cons start accepting row tail target;
          u
        | _ -> u
      end

  let rec (indexed_step_sound @ total) :
      (dfa : automaton) -> (all_states : state list) ->
      (remaining : state list) -> (start : int) -> (source : state) ->
      (letter : int) ->
      {u : unit | match index_from source remaining start,
          build_table dfa all_states remaining start with
        | Some target, Some table ->
          if Dfa_proof.raw_unique_keys table &&
            member_state (next dfa source letter) all_states then
            let _, row = Dfa_proof.raw_view table target in
            Dfa_proof.raw_row_step row letter ===
              next_index dfa all_states source letter
          else true
        | _ -> true} @ immutable contended =
    fun dfa all_states remaining start source letter ->
    index_from_def source remaining start;
    build_table_def dfa all_states remaining start;
    let u = () in
    match remaining with
    | [] -> u
    | head :: rest ->
      let letters = distinct_letters (labels dfa head) [] in
      let fallback_index = index (default dfa head) all_states in
      let built_edges = build_edges dfa all_states head letters in
      let next_start = start + 1 in
      let tail_table = build_table dfa all_states rest next_start in
      if same_state source head then begin
        same_state_correct source head;
        match fallback_index, built_edges, tail_table with
        | Some fallback, Some edges, Some tail ->
          let accepting = output dfa head in
          let row = edges, fallback in
          built_row_index_step dfa all_states head letter;
          Dfa_proof.raw_view_cons start accepting row tail start;
          u
        | _ -> u
      end else begin
        indexed_step_sound dfa all_states rest next_start source letter;
        index_in_built_table dfa all_states rest next_start source;
        match index_from source rest next_start,
          fallback_index, built_edges, tail_table with
        | Some target, Some fallback, Some edges, Some tail ->
          let accepting = output dfa head in
          let row = edges, fallback in
          Dfa_proof.raw_unique_tail start accepting row tail;
          Dfa_proof.raw_tail_key_distinct
            start accepting row tail target;
          Dfa_proof.raw_view_cons start accepting row tail target;
          u
        | _ -> u
      end

  let[@def] rec rows_closed dfa all_states remaining =
    match remaining with
    | [] -> true
    | source :: rest ->
      member_state (default dfa source) all_states &&
      edges_closed dfa all_states source
        (distinct_letters (labels dfa source) []) &&
      rows_closed dfa all_states rest

  let[@def] rec source_list_member dfa remaining =
    match remaining with
    | [] -> true
    | source :: rest ->
      contains_state dfa source && source_list_member dfa rest

  let[@def] rec all_members_in sources remaining =
    match remaining with
    | [] -> true
    | source :: rest ->
      contains_in source sources && all_members_in sources rest

  let (contains_in_prefix @ total) source head rest :
      {u : unit | if contains_in source rest then
        contains_in source (head :: rest) else true} =
    contains_in_cons source head rest;
    let u = () in u

  let rec (all_members_prefix @ total) :
      (head : state) -> (rest : state list) ->
      (remaining : state list) ->
      {u : unit | if all_members_in rest remaining then
        all_members_in (head :: rest) remaining else true}
        @ immutable contended =
    fun head rest remaining ->
    all_members_in_def rest remaining;
    let extended = head :: rest in
    all_members_in_def extended remaining;
    let u = () in
    match remaining with
    | [] -> u
    | source :: tail ->
      contains_in_prefix source head rest;
      all_members_prefix head rest tail;
      u

  let rec (all_members_self @ total) :
      (sources : state list) ->
      {u : unit | all_members_in sources sources}
        @ immutable contended =
    fun sources ->
    all_members_in_def sources sources;
    let u = () in
    match sources with
    | [] -> u
    | head :: rest ->
      all_members_self rest;
      all_members_prefix head rest rest;
      contains_in_cons head head rest;
      same_state_correct head head;
      u

  let rec (source_list_member_of_all @ total) :
      (dfa : automaton) -> (remaining : state list) ->
      {u : unit | if all_members_in (states dfa) remaining then
        source_list_member dfa remaining else true}
        @ immutable contended =
    fun dfa remaining ->
    let sources = states dfa in
    all_members_in_def sources remaining;
    source_list_member_def dfa remaining;
    let u = () in
    match remaining with
    | [] -> u
    | source :: rest ->
      contains_state_equation dfa source;
      source_list_member_of_all dfa rest;
      u

  let (compiled_source_list_member @ total) (root @ total) :
      {u : unit | let dfa = compile root in
        source_list_member dfa (initial dfa :: [] :: states dfa)} =
    let (dfa @ total) = compile root in
    let initial_state = initial dfa in
    let nil = [] in
    let source_states = states dfa in
    let tail = nil :: source_states in
    let all_states = initial_state :: tail in
    compiled_initial_member root;
    compiled_empty_member root;
    all_members_self source_states;
    source_list_member_of_all dfa source_states;
    source_list_member_def dfa all_states;
    source_list_member_def dfa tail;
    let u = () in u

  let rec (source_rows_closed @ total) :
      (dfa : automaton) ->
      (closure : ((source : state) -> (letter : int) ->
        {u : unit | if contains_state dfa source then
          contains_state dfa (next dfa source letter) else true})) @ total ->
      (remaining : state list) ->
      {u : unit | let all_states = initial dfa :: [] :: states dfa in
        if source_list_member dfa remaining then
          rows_closed dfa all_states remaining else true}
        @ immutable contended =
    fun dfa closure remaining ->
    let all_states = initial dfa :: [] :: states dfa in
    source_list_member_def dfa remaining;
    rows_closed_def dfa all_states remaining;
    let u = () in
    match remaining with
    | [] -> u
    | source :: rest ->
      let fallback = default dfa source in
      let letters = distinct_letters (labels dfa source) [] in
      default_empty dfa source;
      empty_prefix dfa;
      member_state_def fallback all_states;
      source_edges_closed dfa closure source letters;
      source_rows_closed dfa closure rest;
      u

  let (compiled_rows_closed @ total) (root @ total) :
      {u : unit | let dfa = compile root in
        let all_states = initial dfa :: [] :: states dfa in
        rows_closed dfa all_states all_states} =
    let (dfa @ total) = compile root in
    let all_states = initial dfa :: [] :: states dfa in
    let (closure @ total) (source : state) (letter : int) :
        {u : unit | if contains_state dfa source then
          contains_state dfa (next dfa source letter) else true} =
      compiled_next_member root source letter;
      let u = () in u in
    compiled_source_list_member root;
    source_rows_closed dfa closure all_states;
    let u = () in u

  let rec (build_table_complete @ total) :
      (dfa : automaton) -> (all_states : state list) ->
      (remaining : state list) -> (id : int) ->
      {u : unit | if rows_closed dfa all_states remaining then
        match build_table dfa all_states remaining id with
        | None -> false
        | Some _ -> true
        else true} @ immutable contended =
    fun dfa all_states remaining id ->
    rows_closed_def dfa all_states remaining;
    build_table_def dfa all_states remaining id;
    let u = () in
    match remaining with
    | [] -> u
    | source :: rest ->
      let fallback = default dfa source in
      let letters = distinct_letters (labels dfa source) [] in
      let next_id = id + 1 in
      index_complete fallback all_states;
      build_edges_complete dfa all_states source letters;
      build_table_complete dfa all_states rest next_id;
      u

  let[@def] lower_raw dfa =
    let all_states = initial dfa :: [] :: states dfa in
    match index (initial dfa) all_states,
      build_table dfa all_states all_states 0 with
    | Some initial, Some table -> Some (initial, table)
    | _ -> None

  let (lower_raw_initial @ total) (dfa : automaton) :
      {u : unit | match lower_raw dfa with
        | None -> true
        | Some (initial, _) -> initial = 0} =
    let source = initial dfa in
    let all_states = source :: [] :: states dfa in
    initial_index dfa;
    lower_raw_def dfa;
    index_def source all_states;
    let u = () in u

  let (lower_raw_complete @ total) (dfa : automaton) :
      {u : unit | let all_states = initial dfa :: [] :: states dfa in
        if rows_closed dfa all_states all_states then
          match lower_raw dfa with
          | None -> false
          | Some _ -> true
        else true} =
    let all_states = initial dfa :: [] :: states dfa in
    let zero = 0 in
    initial_index dfa;
    build_table_complete dfa all_states all_states zero;
    lower_raw_def dfa;
    let u = () in u

  let (compiled_lower_raw_complete @ total) (root @ total) :
      {u : unit | match lower_raw (compile root) with
        | None -> false
        | Some _ -> true} =
    let (dfa @ total) = compile root in
    compiled_rows_closed root;
    lower_raw_complete dfa;
    let u = () in u

  let rec (raw_run_indexed @ total) :
      (dfa : automaton) ->
      (closure : ((source : state) -> (letter : int) ->
        {u : unit | if contains_state dfa source then
          contains_state dfa (next dfa source letter) else true})) @ total ->
      (all_states : state list) ->
      (table : (int * bool * Dfa_proof.row) list) ->
      (source : state) -> (id : int) -> (word : int list) ->
      {u : unit | if all_states === (initial dfa :: [] :: states dfa) &&
        build_table dfa all_states all_states 0 === Some table &&
        Dfa_proof.raw_unique_keys table &&
        contains_state dfa source &&
        index source all_states === Some id then
        Dfa_proof.raw_run_from (0, table) id word ===
          run_from dfa source word
        else true} @ immutable contended =
    fun dfa closure all_states table source id word ->
    let raw = 0, table in
    let zero = 0 in
    index_def source all_states;
    let u = () in
    match word with
    | [] ->
      indexed_output_sound dfa all_states all_states zero source;
      Dfa_proof.raw_final_view raw id;
      Dfa_proof.raw_run_from_empty raw id;
      run_from_empty dfa source;
      u
    | letter :: suffix ->
      let target = next dfa source letter in
      closure source letter;
      member_prefix dfa target;
      index_complete target all_states;
      indexed_step_sound dfa all_states all_states zero source letter;
      Dfa_proof.raw_step_view raw id letter;
      (match index target all_states with
       | None -> u
       | Some target_id ->
         index_def target all_states;
         next_index_def dfa all_states source letter;
         target_index_def dfa all_states source zero letter;
         Dfa_proof.raw_run_from_letter raw id letter suffix;
         run_from_letter dfa source letter suffix;
         raw_run_indexed dfa closure all_states table target target_id suffix;
         u)

  let (compiled_raw_run @ total) (root @ total) (word : int list) :
      {u : unit | let dfa = compile root in
        match lower_raw dfa with
        | None -> true
        | Some raw -> if Dfa_proof.raw_valid raw then
            Dfa_proof.raw_run raw word === run dfa word
          else true} =
    let (dfa @ total) = compile root in
    let all_states = initial dfa :: [] :: states dfa in
    let zero = 0 in
    let (closure @ total) (source : state) (letter : int) :
        {u : unit | if contains_state dfa source then
          contains_state dfa (next dfa source letter) else true} =
      compiled_next_member root source letter;
      let u = () in u in
    compiled_lower_raw_complete root;
    lower_raw_initial dfa;
    compiled_initial_member root;
    let source = initial dfa in
    member_prefix dfa source;
    let u = () in
    match lower_raw dfa with
    | None -> u
    | Some (initial_id, table) ->
      let raw = initial_id, table in
      Dfa_proof.raw_valid_unique raw;
      initial_index dfa;
      lower_raw_def dfa;
      raw_run_indexed dfa closure all_states table source zero word;
      Dfa_proof.raw_run_initial raw word;
      run_initial dfa word;
      u

  let[@def] lower dfa =
    match lower_raw dfa with
    | None -> None
    | Some raw -> Dfa_proof.of_raw raw

  let (lower_compiled_matches @ total) (root : Regex.t)
      (word : int list) :
      {u : unit | match lower (compile root) with
        | None -> true
        | Some machine -> Dfa_semantics.run machine word ===
          Regex.matches root word} =
    let (dfa @ total) = compile root in
    lower_def dfa;
    compiled_raw_run root word;
    Regex.Dfa.correct root word;
    let u = () in
    match lower_raw dfa with
    | None -> u
    | Some raw ->
      Dfa_proof.of_raw_raw_valid raw;
      (match Dfa_proof.of_raw raw with
       | None -> u
       | Some machine ->
         Dfa_proof.of_raw_run raw machine word;
         u)
end;;

module Dfa_semantics = struct
  type row = (int * int) list * int
  type raw = int * (int * bool * row) list
  type machine = raw

  let[@def] rec has_key (key : int) (table : (int * bool * row) list) =
    match table with
    | [] -> false
    | (candidate, _, _) :: rest -> key = candidate || has_key key rest

  let[@def] has_state (machine : machine) state =
    let _, table = machine in
    has_key state table

  let[@def] rec unique_keys (table : (int * bool * row) list) =
    match table with
    | [] -> true
    | (key, _, _) :: rest ->
      not (has_key key rest) && unique_keys rest

  let[@def] rec has_label (label : int) (edges : (int * int) list) =
    match edges with
    | [] -> false
    | (candidate, _) :: rest -> label = candidate || has_label label rest

  let[@def] rec unique_labels (edges : (int * int) list) =
    match edges with
    | [] -> true
    | (label, _) :: rest ->
      not (has_label label rest) && unique_labels rest

  let[@def] rec targets_valid (table : (int * bool * row) list)
      (edges : (int * int) list) =
    match edges with
    | [] -> true
    | (_, target) :: rest ->
      has_key target table && targets_valid table rest

  let[@def] rec rows_valid (table : (int * bool * row) list)
      (remaining : (int * bool * row) list) =
    match remaining with
    | [] -> true
    | (_, _, (edges, default)) :: rest ->
      unique_labels edges && has_key default table &&
      targets_valid table edges && rows_valid table rest

  let[@def] valid (machine : machine) =
    let initial, table = machine in
    has_key initial table && unique_keys table && rows_valid table table

  let[@def] rec edge_step (edges : (int * int) list)
      (default : int) (c : int) =
    match edges with
    | [] -> default
    | (label, target) :: rest ->
      if c = label then target else edge_step rest default c

  let[@def] row_step (row : row) c =
    let edges, default = row in
    edge_step edges default c

  let[@def] rec edge_labels (edges : (int * int) list) =
    match edges with
    | [] -> []
    | (label, _) :: rest -> label :: edge_labels rest

  let[@def] row_labels (row : row) =
    let edges, _ = row in
    edge_labels edges

  let[@def] rec view (table : (int * bool * row) list) (state : int) =
    match table with
    | [] -> false, ([], 0)
    | (key, accepting, row) :: rest ->
      if key = state then accepting, row else view rest state

  let[@def] final (machine : machine) state =
    let _, table = machine in
    match view table state with accepting, _ -> accepting

  let[@def] step (machine : machine) state c =
    let _, table = machine in
    match view table state with _, row -> row_step row c

  let[@def] labels (machine : machine) state =
    let _, table = machine in
    match view table state with _, row -> row_labels row

  let[@def] rec execute machine state word =
    match word with
    | [] -> final machine state
    | c :: rest -> execute machine (step machine state c) rest

  let[@def] run (machine : machine) word =
    let initial, _ = machine in
    execute machine initial word

  let[@def] rec big_length xs =
    match xs with
    | [] -> 0Z
    | _ :: rest -> Bigint.add 1Z (big_length rest)

  let[@def] rec state_ids (table : (int * bool * row) list) =
    match table with
    | [] -> []
    | (state, _, _) :: rest -> state :: state_ids rest

  let[@def] state_size (machine : machine) =
    let _, table = machine in
    big_length table

  let[@def] rec list_size (xs : int list) =
    match xs with
    | [] -> 0
    | _ :: rest ->
      let size = list_size rest in
      if size >= 129 then 129 else size + 1

  let[@def] rec bounded_labels_from machine states =
    match states with
    | [] -> true
    | state :: rest ->
      list_size (labels machine state) <= 64 && bounded_labels_from machine rest

  let[@def] labels_bounded (machine : machine) =
    let _, table = machine in
    bounded_labels_from machine (state_ids table)
end;;

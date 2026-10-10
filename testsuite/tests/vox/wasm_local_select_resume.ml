module B = Wasm_u32
module C = Wasm_code
module T = Wasm_control
module X = Wasm_memory_execution
module Select = Wasm_local_select
module Table = Hmc_wasm_dispatch_code
module Labels = Wasm_empty_labels
let[@def] rec (cost @ total) (table : Table.table @ immutable) (pc : B.u32) = match table with
  | Table.Empty -> C.Zero
  | Table.Add (number, _, rest) -> C.Succ (if pc = number then C.Zero else cost rest pc)
let rec (correct @ total) : (table : Table.table) @ immutable -> (pc : B.u32) -> (code_local : B.u32) ->
    (outer : T.labels) @ immutable -> (state : X.state) @ immutable ->
    {u : unit | Labels.related (cost table pc) outer (Select.selection table pc code_local outer state).T.labels} @ ghost =
  fun table pc code_local outer state -> ghost_ (
    cost_def table pc; Select.selection_def table pc code_local outer state;
    match table with
    | Table.Empty -> Labels.related_def C.Zero outer outer
    | Table.Add (number, _, rest) ->
      Select.label_def outer;
      if pc = number then (
        Labels.related_def (C.Succ C.Zero) outer (Select.label outer);
        Labels.related_def C.Zero outer outer)
      else (
        correct rest pc code_local (Select.label outer) state;
        Labels.extend (cost rest pc) outer (Select.selection rest pc code_local (Select.label outer) state).T.labels ()))

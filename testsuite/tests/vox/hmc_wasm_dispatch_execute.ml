module B = Wasm_u32
module W = Hmc_word64
module C = Wasm_code
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module M = Wasm_memory
module T = Wasm_control
module Lift = Wasm_control_lift
module Fuel = Wasm_control_compose
module Table = Hmc_wasm_table_lower
module Block = Hmc_wasm_block_lower
module Select = Hmc_wasm_dispatch_select
module Header = Hmc_wasm_header_update
let[@def] rec (depth @ total) (table : Table.table @ immutable) (pc : W.limb) = match table with
  | Table.Empty -> C.Zero | Table.Add (number, _, rest) -> C.Succ (if pc = number then C.Zero else depth rest pc)
let[@def] rec (labels @ total) (count : C.count @ immutable) (outer : T.labels @ immutable) = match count with
  | C.Zero -> outer | C.Succ rest -> labels rest (Select.label outer)
let rec (selected_labels @ total) : (table : Table.table) @ immutable -> (pc : W.limb) -> (base_local : B.u32) ->
    (outer : T.labels) @ immutable -> (state : X.state) @ immutable ->
    {u : unit | (Select.selection table pc base_local outer state).T.labels === labels (depth table pc) outer} @ ghost =
  fun table pc base_local outer state -> ghost_ (
    Select.selection_def table pc base_local outer state; depth_def table pc; labels_def (depth table pc) outer;
    match table with
    | Table.Empty -> ()
    | Table.Add (number, _, rest) -> if pc = number then labels_def C.Zero (Select.label outer)
      else selected_labels rest pc base_local (Select.label outer) state)
let rec (one @ total) : (count : C.count) @ immutable ->
    {u : unit | Fuel.add count (C.Succ C.Zero) === C.Succ count} @ ghost = fun count -> ghost_ (
    Fuel.add_def count (C.Succ C.Zero); match count with C.Zero -> () | C.Succ rest -> one rest)
let rec (unwind @ total) : (count : C.count) @ immutable -> (outer : T.labels) @ immutable -> (state : X.state) @ immutable ->
    {u : unit | state.X.machine.E.stack === S.Empty} ->
    {u : unit | T.run count {T.code = T.Empty; labels = labels count outer; state} === T.Running {T.code = T.Empty; labels = outer; state}} @ ghost =
  fun count outer state premise -> ghost_ (
    labels_def count outer;
    match count with
    | C.Zero -> T.run_def C.Zero {T.code = T.Empty; labels = outer; state}
    | C.Succ rest ->
      unwind rest (Select.label outer) state (); one rest;
      Fuel.correct rest (C.Succ C.Zero) {T.code = T.Empty; labels = labels rest (Select.label outer); state};
      Select.label_def outer;
      T.run_def (C.Succ C.Zero) {T.code = T.Empty; labels = Select.label outer; state};
      T.step_def {T.code = T.Empty; labels = Select.label outer; state}; T.stack_def state S.Empty;
      T.run_def C.Zero {T.code = T.Empty; labels = outer; state})
let[@def] (cost @ total) (table : Table.table @ immutable) (pc : W.limb) (base_local : B.u32) (fragment : Block.fragment @ immutable) =
  Fuel.add (Select.cost table pc base_local) (Fuel.add (C.length (Block.emit fragment base_local)) (depth table pc))
let (correct @ total) : (table : Table.table) @ immutable -> (pc : W.limb) -> (base_local : B.u32) -> (base : B.u32) ->
    (fragment : Block.fragment) @ immutable -> (outer : T.labels) @ immutable -> (before : X.state) @ immutable -> (after : X.state) @ immutable ->
    {u : unit | Table.lookup table pc === Some fragment && before.X.machine.E.stack === S.Empty && after.X.machine.E.stack === S.Empty
      && Wasm_locals.get before.X.machine.E.locals base_local === Some (S.I32 base)
      && M.load before.X.memory base (Hmc_wasm_pc_update.offset ()) M.W64 === Some (S.I64 (Header.number pc))
      && X.run (Block.emit fragment base_local) before === X.Done after} ->
    {u : unit | T.run (cost table pc base_local fragment) {T.code = Select.emit table base_local; labels = outer; state = before}
      === T.Running {T.code = T.Empty; labels = outer; state = after}} @ ghost =
  fun table pc base_local base fragment outer before after premise -> ghost_ (
    cost_def table pc base_local fragment;
    Select.correct table pc base_local base outer before (); Select.selected_code table pc base_local outer before;
    selected_labels table pc base_local outer before;
    Wasm_control_success.straight (Block.emit fragment base_local) before after ();
    let selected = Select.selection table pc base_local outer before in
    Lift.correct (Block.emit fragment base_local) T.Empty selected.T.labels before after ();
    unwind (depth table pc) outer after ();
    Fuel.correct (C.length (Block.emit fragment base_local)) (depth table pc) selected;
    Fuel.correct (Select.cost table pc base_local) (Fuel.add (C.length (Block.emit fragment base_local)) (depth table pc))
      {T.code = Select.emit table base_local; labels = outer; state = before})

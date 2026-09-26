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
module Table = Hmc_wasm_dispatch_code
module Block = Hmc_wasm_block_lower
module Select = Hmc_wasm_structured_select
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
let (correct @ total) : (table : Table.table) @ immutable -> (pc : W.limb) -> (base_local : B.u32) -> (base : B.u32) ->
    (outer : T.labels) @ immutable -> (before : X.state) @ immutable -> (after : X.state) @ immutable -> (entry_cost : C.count) @ immutable ->
    {u : unit | before.X.machine.E.stack === S.Empty && after.X.machine.E.stack === S.Empty
      && Wasm_locals.get before.X.machine.E.locals base_local === Some (S.I32 base)
      && M.load before.X.memory base (Hmc_wasm_pc_update.offset ()) M.W64 === Some (S.I64 (Header.number pc))
      && T.run entry_cost (Select.selection table pc base_local outer before) ===
        T.Running {T.code = T.Empty; labels = (Select.selection table pc base_local outer before).T.labels; state = after}} ->
    {u : unit | T.run (Fuel.add (Select.cost table pc base_local) (Fuel.add entry_cost (depth table pc)))
        {T.code = Select.emit table base_local; labels = outer; state = before}
      === T.Running {T.code = T.Empty; labels = outer; state = after}} @ ghost =
  fun table pc base_local base outer before after entry_cost premise -> ghost_ (
    selected_labels table pc base_local outer before;
    unwind (depth table pc) outer after ();
    Fuel.correct entry_cost (depth table pc) (Select.selection table pc base_local outer before);
    Hmc_wasm_structured_execute.correct table pc base_local base outer before (Fuel.add entry_cost (depth table pc))
      (T.Running {T.code = T.Empty; labels = outer; state = after}) ())

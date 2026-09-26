module B = Wasm_u32
module W = Hmc_word64
module C = Wasm_code
module T = Wasm_control
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module M = Wasm_memory
module Table = Hmc_wasm_dispatch_code
module Select = Hmc_wasm_structured_select
module Fuel = Wasm_control_compose
module Header = Hmc_wasm_header_update
let (correct @ total) : (table : Table.table) @ immutable -> (pc : W.limb) -> (base_local : B.u32) -> (base : B.u32) ->
    (outer : T.labels) @ immutable -> (state : X.state) @ immutable -> (entry_cost : C.count) @ immutable -> (result : T.result) @ immutable ->
    {u : unit | state.X.machine.E.stack === S.Empty && Wasm_locals.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && M.load state.X.memory base (Hmc_wasm_pc_update.offset ()) M.W64 === Some (S.I64 (Header.number pc))
      && T.run entry_cost (Select.selection table pc base_local outer state) === result} ->
    {u : unit | T.run (Fuel.add (Select.cost table pc base_local) entry_cost)
      {T.code = Select.emit table base_local; labels = outer; state} === result} @ ghost =
  fun table pc base_local base outer state entry_cost result premise -> ghost_ (
    Select.correct table pc base_local base outer state ();
    Fuel.correct (Select.cost table pc base_local) entry_cost {T.code = Select.emit table base_local; labels = outer; state})

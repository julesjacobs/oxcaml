module B = Wasm_u32
module S = Wasm_scalar
module L = Wasm_locals
module Replace = Wasm_local_replace
module I = Wasm_instruction
module C = Wasm_code
module E = Wasm_execution
module X = Wasm_memory_execution
module G = Wasm_globals
module GE = Wasm_global_execution
module Transfer = Wasm_global_local_transfer
let (global @ total) : (globals : G.t) @ immutable -> (index : B.u32) -> (value : S.value) @ immutable ->
    {u : unit | G.can_set globals index value} ->
    {u : unit | G.set globals index value === Some {G.values = Replace.replace globals.G.values index value; permissions = globals.G.permissions}} @ ghost =
  fun globals index value premise -> ghost_ (
    match G.set globals index value with None -> () | Some after -> Replace.unique globals.G.values index value after.G.values ())
let (correct @ total) : (global_index : B.u32) -> (local : B.u32) -> (value : S.value) @ immutable -> (state : GE.state) @ immutable ->
    {u : unit | L.get state.GE.execution.X.machine.E.locals local === Some value && G.can_set state.GE.globals global_index value} ->
    {u : unit | GE.run (Transfer.store_code global_index local) state === GE.Done
      {GE.globals = {G.values = Replace.replace state.GE.globals.G.values global_index value; permissions = state.GE.globals.G.permissions}; execution = state.GE.execution}} @ ghost =
  fun global_index local value state premise -> ghost_ (
    global state.GE.globals global_index value ();
    Transfer.store_code_def global_index local;
    GE.run_def (Transfer.store_code global_index local) state; GE.step_def (I.Local_get local) state;
    X.step_def (I.Local_get local) state.GE.execution; E.step_def (I.Local_get local) state.GE.execution.X.machine;
    let pushed = {GE.globals = state.GE.globals; execution = {X.memory = state.GE.execution.X.memory;
      machine = {E.locals = state.GE.execution.X.machine.E.locals; stack = S.Push (value, state.GE.execution.X.machine.E.stack)}}} in
    GE.run_def (C.Next (I.Global_set global_index, C.Empty)) pushed; GE.step_def (I.Global_set global_index) pushed;
    GE.run_def C.Empty {GE.globals = {G.values = Replace.replace state.GE.globals.G.values global_index value; permissions = state.GE.globals.G.permissions}; execution = state.GE.execution})

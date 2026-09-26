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
let (correct @ total) : (global : B.u32) -> (local : B.u32) -> (value : S.value) @ immutable -> (state : GE.state) @ immutable ->
    {u : unit | G.get state.GE.globals global === Some value && L.can_set state.GE.execution.X.machine.E.locals local value} ->
    {u : unit | GE.run (Transfer.load_code global local) state === GE.Done
      {GE.globals = state.GE.globals; execution = {X.memory = state.GE.execution.X.memory;
        machine = {E.locals = Replace.replace state.GE.execution.X.machine.E.locals local value; stack = state.GE.execution.X.machine.E.stack}}}} @ ghost =
  fun global local value state premise -> ghost_ (
    Replace.correct state.GE.execution.X.machine.E.locals local value ();
    Transfer.load_code_def global local;
    GE.run_def (Transfer.load_code global local) state; GE.step_def (I.Global_get global) state;
    let pushed = {GE.globals = state.GE.globals; execution = {X.memory = state.GE.execution.X.memory;
      machine = {E.locals = state.GE.execution.X.machine.E.locals; stack = S.Push (value, state.GE.execution.X.machine.E.stack)}}} in
    GE.run_def (C.Next (I.Local_set local, C.Empty)) pushed;
    GE.step_def (I.Local_set local) pushed; X.step_def (I.Local_set local) pushed.GE.execution;
    E.step_def (I.Local_set local) pushed.GE.execution.X.machine;
    GE.run_def C.Empty {GE.globals = state.GE.globals; execution = {X.memory = state.GE.execution.X.memory;
      machine = {E.locals = Replace.replace state.GE.execution.X.machine.E.locals local value; stack = state.GE.execution.X.machine.E.stack}}})

module B = Wasm_u32
module I = Wasm_instruction
module C = Wasm_code
module S = Wasm_scalar
module L = Wasm_locals
module E = Wasm_execution
module X = Wasm_memory_execution
module G = Wasm_globals
module GE = Wasm_global_execution
let[@def] (load_code @ total) (global : B.u32) (local : B.u32) =
  C.Next (I.Global_get global, C.Next (I.Local_set local, C.Empty))
let[@def] (store_code @ total) (global : B.u32) (local : B.u32) =
  C.Next (I.Local_get local, C.Next (I.Global_set global, C.Empty))
let (load @ total) : (global : B.u32) -> (local : B.u32) -> (state : GE.state) @ immutable ->
    {out : GE.state option | GE.run (load_code global local) state === (match out with None -> GE.Type_error | Some next -> GE.Done next)
      && (match out with None -> true | Some next -> next.GE.globals === state.GE.globals
        && next.GE.execution.X.memory === state.GE.execution.X.memory
        && next.GE.execution.X.machine.E.stack === state.GE.execution.X.machine.E.stack
        && (match G.get state.GE.globals global with None -> false | Some value ->
          L.get next.GE.execution.X.machine.E.locals local === Some value
          && L.replaced state.GE.execution.X.machine.E.locals local value next.GE.execution.X.machine.E.locals))} @ immutable =
  fun global local state ->
    let execution = state.GE.execution in
    ghost_ (load_code_def global local; GE.run_def (load_code global local) state; GE.step_def (I.Global_get global) state);
    match G.get state.GE.globals global with
    | None -> None
    | Some value ->
      let pushed = {GE.globals = state.GE.globals; execution = {X.memory = execution.X.memory;
        machine = {E.locals = execution.X.machine.E.locals; stack = S.Push (value, execution.X.machine.E.stack)}}} in
      ghost_ (GE.run_def (C.Next (I.Local_set local, C.Empty)) pushed;
        GE.step_def (I.Local_set local) pushed; X.step_def (I.Local_set local) pushed.GE.execution;
        E.step_def (I.Local_set local) pushed.GE.execution.X.machine);
      match L.set execution.X.machine.E.locals local value with
      | None -> None
      | Some locals ->
        let next = {GE.globals = state.GE.globals; execution = {X.memory = execution.X.memory;
          machine = {E.locals; stack = execution.X.machine.E.stack}}} in
        ghost_ (GE.run_def C.Empty next);
        Some next
let (store @ total) : (global : B.u32) -> (local : B.u32) -> (state : GE.state) @ immutable ->
    {out : GE.state option | GE.run (store_code global local) state === (match out with None -> GE.Type_error | Some next -> GE.Done next)
      && (match out with None -> true | Some next -> next.GE.execution === state.GE.execution
        && next.GE.globals.G.permissions === state.GE.globals.G.permissions
        && (match L.get state.GE.execution.X.machine.E.locals local with None -> false | Some value ->
          G.get next.GE.globals global === Some value
          && L.replaced state.GE.globals.G.values global value next.GE.globals.G.values))} @ immutable =
  fun global local state ->
    let execution = state.GE.execution in
    ghost_ (store_code_def global local; GE.run_def (store_code global local) state;
      GE.step_def (I.Local_get local) state; X.step_def (I.Local_get local) execution; E.step_def (I.Local_get local) execution.X.machine);
    match L.get execution.X.machine.E.locals local with
    | None -> None
    | Some value ->
      let pushed = {GE.globals = state.GE.globals; execution = {X.memory = execution.X.memory;
        machine = {E.locals = execution.X.machine.E.locals; stack = S.Push (value, execution.X.machine.E.stack)}}} in
      ghost_ (GE.run_def (C.Next (I.Global_set global, C.Empty)) pushed; GE.step_def (I.Global_set global) pushed);
      match G.set state.GE.globals global value with
      | None -> None
      | Some globals ->
        let next = {GE.globals; execution} in
        ghost_ (GE.run_def C.Empty next);
        Some next

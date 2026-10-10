module B = Wasm_u32
module I = Wasm_instruction
module C = Wasm_code
module S = Wasm_scalar
module L = Wasm_locals
module E = Wasm_execution
module X = Wasm_memory_execution
module G = Wasm_globals
module GE = Wasm_global_execution

let (write @ total) : (index : B.u32) -> (value : S.value) @ immutable -> (state : GE.state) @ ghost ->
    {u : unit | G.can_set state.GE.globals index value} ->
    {code : C.t | match GE.run code state with
      | GE.Done after -> after.GE.execution === state.GE.execution
        && G.get after.GE.globals index === Some value
        && after.GE.globals.G.permissions === state.GE.globals.G.permissions
        && L.same_types state.GE.globals.G.values after.GE.globals.G.values
        && L.replaced state.GE.globals.G.values index value after.GE.globals.G.values
      | _ -> false} @ immutable = fun index value state premise ->
  let instruction = match value with S.I32 n -> I.I32_const n | S.I64 n -> I.I64_const n in
  let tail = C.Next (I.Global_set index, C.Empty) in
  let code = C.Next (instruction, tail) in
  ghost_ (
    let execution = state.GE.execution in
    let pushed = {GE.globals = state.GE.globals; execution = {X.memory = execution.X.memory;
      machine = {E.locals = execution.X.machine.E.locals; stack = S.Push (value, execution.X.machine.E.stack)}}} in
    GE.run_def code state; GE.step_def instruction state;
    X.step_def instruction execution; E.step_def instruction execution.X.machine;
    S.step_def instruction execution.X.machine.E.stack;
    GE.run_def tail pushed; GE.step_def (I.Global_set index) pushed;
    match G.set state.GE.globals index value with
    | None -> ()
    | Some globals -> GE.run_def C.Empty {GE.globals; execution});
  code
let (read @ total) : (index : B.u32) -> (destination : B.u32) ->
    (state : GE.state) @ ghost -> (value : S.value) @ ghost ->
    {u : unit | G.get state.GE.globals index === Some value
      && L.can_set state.GE.execution.X.machine.E.locals destination value} ->
    {code : C.t | match GE.run code state with
      | GE.Done after -> after.GE.globals === state.GE.globals
        && after.GE.execution.X.memory === state.GE.execution.X.memory
        && after.GE.execution.X.machine.E.stack === state.GE.execution.X.machine.E.stack
        && L.get after.GE.execution.X.machine.E.locals destination === Some value
        && L.replaced state.GE.execution.X.machine.E.locals destination value after.GE.execution.X.machine.E.locals
      | _ -> false} @ immutable = fun index destination state value premise ->
  let tail = C.Next (I.Local_set destination, C.Empty) in
  let code = C.Next (I.Global_get index, tail) in
  ghost_ (
    let execution = state.GE.execution in
    let pushed = {GE.globals = state.GE.globals; execution = {X.memory = execution.X.memory;
      machine = {E.locals = execution.X.machine.E.locals; stack = S.Push (value, execution.X.machine.E.stack)}}} in
    GE.run_def code state; GE.step_def (I.Global_get index) state;
    GE.run_def tail pushed; GE.step_def (I.Local_set destination) pushed;
    X.step_def (I.Local_set destination) pushed.GE.execution;
    E.step_def (I.Local_set destination) pushed.GE.execution.X.machine;
    match L.set execution.X.machine.E.locals destination value with
    | None -> ()
    | Some locals -> GE.run_def C.Empty {GE.globals = state.GE.globals;
        execution = {X.memory = execution.X.memory; machine = {E.locals; stack = execution.X.machine.E.stack}}});
  code

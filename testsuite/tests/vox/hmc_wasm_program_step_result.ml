module D = Hm_declarative
module I = Hmc_tail_ir
module U = Hmc_tail_semantics
module F = Hmc_heap_frame
module Q = Hmc_heap_state
module V = Hmc_tagged_cell
module Machine = Hmc_heap_machine
module Inv = Hmc_heap_invariant
module Lower = Hmc_wasm_program_lower
module Registers = Hmc_wasm_program_registers
module State = Hmc_wasm_program_state
module Resource = Hmc_wasm_program_resource_step
module Guard = Hmc_failed_guard_calls
module Failed = Hmc_failed_guard_model
module Calls = Wasm_calls
module Root = Hmc_wasm_program_source_root
module C = Wasm_code
module S = Wasm_scalar
type returned = {state : State.running; registers : Registers.registers; fuel : {n : C.count | not (n === C.Zero)}}
type result = Continued of State.transition | Returned of returned | Exhausted of Resource.result
let[@def] (finished @ total) (state : State.running @ immutable) (registers : Registers.registers @ immutable) =
  Calls.Finished {Wasm_global_execution.globals = Registers.globals registers;
    execution = {Wasm_memory_execution.memory = state.State.memory;
      machine = {Wasm_execution.locals = S.Empty; stack = S.Push (S.I32 registers.Registers.status, S.Empty)}}}
let[@def] (correct @ total) (program : I.program @ immutable) (globals : Machine.globals @ immutable)
    (lowered : Lower.program @ immutable) (context : State.context @ immutable) (before : State.running @ immutable) (out : result @ immutable) = ghost_ (
  match out with
  | Continued next -> State.valid program globals lowered context next.State.state
    && next.State.state.State.registers.Registers.heap_limit = before.State.registers.Registers.heap_limit
    && next.State.state.State.elapsed === D.S before.State.elapsed
    && next.State.state.State.abstract === U.step program before.State.abstract
    && Machine.step program globals before.State.registers.Registers.heap_limit context.State.stack_capacity (State.configuration before)
      === Machine.Advanced (State.configuration next.State.state)
    && Calls.run next.State.fuel (State.module_ program lowered context) (State.loop context before)
      === Calls.Running (State.loop context next.State.state)
  | Returned final -> final.state === before && final.registers.Registers.status = 1
    && final.registers.Registers.tag === V.tag before.State.activation.F.accumulator
    && final.registers.Registers.payload === V.payload before.State.activation.F.accumulator
    && Machine.step program globals before.State.registers.Registers.heap_limit context.State.stack_capacity (State.configuration before)
      === Machine.Advanced {Machine.heap = before.State.heap; state = Q.Done before.State.activation.F.accumulator}
    && Inv.valid program globals before.State.registers.Registers.heap_limit
      {Machine.heap = before.State.heap; state = Q.Done before.State.activation.F.accumulator} (U.step program before.State.abstract)
    && Calls.run final.fuel (State.module_ program lowered context) (State.loop context before) === finished before final.registers
  | Exhausted final -> State.valid program globals lowered context final.Resource.state
    && State.configuration final.Resource.state === State.configuration before
    && final.Resource.state.State.abstract === before.State.abstract && final.Resource.state.State.elapsed === before.State.elapsed
    && final.Resource.state.State.memory === before.State.memory
    && (match final.Resource.exhausted with None -> false | Some why ->
      (match final.Resource.failed_guard with
        | Guard.Absent -> false
        | Guard.Present guard -> Guard.reaches (match why with Machine.Heap -> Failed.Heap | Machine.Stack -> Failed.Stack)
          (State.module_ program lowered context) (State.loop context before) guard)
      && final.Resource.state.State.registers.Registers.status = (match why with Machine.Heap -> 2 | Machine.Stack -> 3)
      && Machine.step program globals before.State.registers.Registers.heap_limit context.State.stack_capacity (State.configuration before) === Machine.Exhausted why)
    && Calls.run final.Resource.fuel (State.module_ program lowered context) (State.loop context before)
      === finished before final.Resource.state.State.registers)
let (continued @ total) : (program : I.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (lowered : Lower.program) @ immutable -> (context : State.context) @ immutable -> (before : State.running) @ immutable ->
    {next : State.transition | State.valid program globals lowered context next.State.state
      && next.State.state.State.registers.Registers.heap_limit = before.State.registers.Registers.heap_limit
      && next.State.state.State.elapsed === D.S before.State.elapsed
      && next.State.state.State.abstract === U.step program before.State.abstract
      && Machine.step program globals before.State.registers.Registers.heap_limit context.State.stack_capacity (State.configuration before)
        === Machine.Advanced (State.configuration next.State.state)
      && Wasm_calls.run next.State.fuel (State.module_ program lowered context) (State.loop context before)
        === Wasm_calls.Running (State.loop context next.State.state)} @ immutable ->
    {out : result | correct program globals lowered context before out} @ immutable =
  fun program globals lowered context before next ->
    let out = Continued next in
    ghost_ (correct_def program globals lowered context before out); out
let (returned @ total) : (program : I.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (lowered : Lower.program) @ immutable -> (context : State.context) @ immutable -> (before : State.running) @ immutable ->
    {last : Root.result | last.Root.registers.Registers.status = 1
      && last.Root.registers.Registers.tag === V.tag before.State.activation.F.accumulator
      && last.Root.registers.Registers.payload === V.payload before.State.activation.F.accumulator
      && Machine.step program globals before.State.registers.Registers.heap_limit context.State.stack_capacity (State.configuration before)
        === Machine.Advanced {Machine.heap = before.State.heap; state = Q.Done before.State.activation.F.accumulator}
      && Hmc_heap_invariant.valid program globals before.State.registers.Registers.heap_limit
        {Machine.heap = before.State.heap; state = Q.Done before.State.activation.F.accumulator}
        (Hmc_tail_semantics.step program before.State.abstract)
      && Wasm_calls.run last.Root.fuel (State.module_ program lowered context) (State.loop context before)
        === Wasm_calls.Finished {Wasm_global_execution.globals = Registers.globals last.Root.registers;
          execution = {Wasm_memory_execution.memory = before.State.memory;
            machine = {Wasm_execution.locals = Wasm_scalar.Empty;
              stack = Wasm_scalar.Push (Wasm_scalar.I32 (Hmc_wasm_program_root.finished ()), Wasm_scalar.Empty)}}}} @ immutable ->
    {out : result | correct program globals lowered context before out} @ immutable =
  fun program globals lowered context before last ->
    let out = Returned {state = before; registers = last.Root.registers; fuel = last.Root.fuel} in
    ghost_ (Hmc_wasm_program_root.finished_def (); finished_def before last.Root.registers;
      correct_def program globals lowered context before out); out
let (resource @ total) : (program : I.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (lowered : Lower.program) @ immutable -> (context : State.context) @ immutable -> (before : State.running) @ immutable ->
    {last : Resource.result | State.valid program globals lowered context last.Resource.state
      && last.Resource.state.State.registers.Registers.heap_limit = before.State.registers.Registers.heap_limit
      && (match last.Resource.exhausted with
        | None -> last.Resource.state.State.elapsed === D.S before.State.elapsed
          && last.Resource.state.State.abstract === U.step program before.State.abstract
          && Machine.step program globals before.State.registers.Registers.heap_limit context.State.stack_capacity (State.configuration before)
            === Machine.Advanced (State.configuration last.Resource.state)
          && Wasm_calls.run last.Resource.fuel (State.module_ program lowered context) (State.loop context before)
            === Wasm_calls.Running (State.loop context last.Resource.state)
        | Some why -> (match last.Resource.failed_guard with
            | Guard.Absent -> false
            | Guard.Present guard -> Guard.reaches (match why with Machine.Heap -> Failed.Heap | Machine.Stack -> Failed.Stack)
              (State.module_ program lowered context) (State.loop context before) guard)
          && State.configuration last.Resource.state === State.configuration before
          && last.Resource.state.State.elapsed === before.State.elapsed
          && last.Resource.state.State.abstract === before.State.abstract
          && last.Resource.state.State.memory === before.State.memory
          && last.Resource.state.State.registers.Registers.status = (match why with Machine.Heap -> 2 | Machine.Stack -> 3)
          && Machine.step program globals before.State.registers.Registers.heap_limit context.State.stack_capacity (State.configuration before)
            === Machine.Exhausted why
          && Wasm_calls.run last.Resource.fuel (State.module_ program lowered context) (State.loop context before)
            === Wasm_calls.Finished {Wasm_global_execution.globals = Registers.globals last.Resource.state.State.registers;
              execution = {Wasm_memory_execution.memory = before.State.memory; machine = {Wasm_execution.locals = Wasm_scalar.Empty;
                stack = Wasm_scalar.Push (Wasm_scalar.I32 last.Resource.state.State.registers.Registers.status, Wasm_scalar.Empty)}}})} @ immutable ->
    {out : result | correct program globals lowered context before out} @ immutable =
  fun program globals lowered context before last ->
    match last.Resource.exhausted with
    | None ->
      let next = {State.state = last.Resource.state; fuel = last.Resource.fuel} in
      continued program globals lowered context before next
    | Some _ ->
      let out = Exhausted last in
      ghost_ (finished_def before last.Resource.state.State.registers;
        correct_def program globals lowered context before out); out

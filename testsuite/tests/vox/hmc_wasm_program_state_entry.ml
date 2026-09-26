module I = Hmc_tail_ir
module G = Hmc_cfg_ir
module Lower = Hmc_wasm_program_lower
module State = Hmc_wasm_program_state
module Registers = Hmc_wasm_program_registers
module Assembly = Hmc_wasm_program_functions
module Runtime = Hmc_wasm_program_runtime
module Dispatch = Hmc_wasm_program_dispatch
module Calls = Wasm_calls
module F = Wasm_functions
module C = Wasm_code
module T = Wasm_control
module P = Wasm_instance_control
module S = Wasm_scalar
let[@def] (configuration @ total) (context : State.context @ immutable) (state : State.running @ immutable) =
  Dispatch.point (Runtime.dispatcher ()).F.code T.No_labels (Registers.globals state.State.registers) state.State.memory S.Empty
    (C.Succ context.State.host_capacity)
let (start @ total) : (program : I.program) @ immutable -> (globals : Hmc_heap_machine.globals) @ immutable ->
    (lowered : Lower.program) @ immutable -> (context : State.context) @ immutable -> (state : State.running) @ immutable ->
    {u : unit | State.valid program globals lowered context state} ->
    {u : unit | Calls.start (State.module_ program lowered context) context.State.block_count state.State.memory
      (Registers.globals state.State.registers) (C.Succ context.State.host_capacity) === Calls.Running (configuration context state)
      && Calls.step (State.module_ program lowered context) (configuration context state) === Calls.Running (State.loop context state)} @ ghost =
  fun program globals lowered context state premise -> ghost_ (
    State.valid_def program globals lowered context state;
    Lower.corresponds_def program globals context.State.max_pc lowered;
    Assembly.source_order globals program.I.origin.Hmc_cfg_program.blocks program.I.code lowered.Lower.blocks lowered.Lower.capacity context.State.max_pc context.State.block_count ();
    Assembly.main_correct lowered (G.size program.I.origin.Hmc_cfg_program.blocks) context.State.block_count
      (Runtime.config context.State.table_base context.State.stack_base) (Runtime.dispatcher ()) ();
    State.module__def program lowered context;
    Runtime.dispatcher_def (); F.zero_locals_def F.No_locals;
    Calls.start_def (State.module_ program lowered context) context.State.block_count state.State.memory
      (Registers.globals state.State.registers) (C.Succ context.State.host_capacity);
    configuration_def context state;
    Dispatch.point_def (Runtime.dispatcher ()).F.code T.No_labels (Registers.globals state.State.registers) state.State.memory S.Empty (C.Succ context.State.host_capacity);
    State.loop_def context state; Dispatch.loop_def (Registers.globals state.State.registers) state.State.memory (C.Succ context.State.host_capacity);
    Dispatch.labels_def (); Dispatch.exit_def ();
    Dispatch.point_def (Runtime.dispatch_body ()) (Dispatch.labels ()) (Registers.globals state.State.registers) state.State.memory S.Empty (C.Succ context.State.host_capacity);
    P.step_def (configuration context state).Calls.current;
    T.step_def (configuration context state).Calls.current.P.body;
    T.enter_def (Runtime.dispatch_body ()) (Dispatch.exit ()) (Some (Runtime.dispatch_body ())) (configuration context state).Calls.current.P.body;
    T.stack_def (configuration context state).Calls.current.P.body.T.state S.Empty;
    Wasm_calls_body.step (State.module_ program lowered context) (configuration context state) (State.loop context state).Calls.current ())
let (execution @ total) : (program : I.program) @ immutable -> (globals : Hmc_heap_machine.globals) @ immutable ->
    (lowered : Lower.program) @ immutable -> (context : State.context) @ immutable -> (state : State.running) @ immutable ->
    (fuel : C.count) @ immutable ->
    {u : unit | State.valid program globals lowered context state} ->
    {u : unit | Calls.run (C.Succ fuel) (State.module_ program lowered context) (configuration context state)
      === Calls.run fuel (State.module_ program lowered context) (State.loop context state)} @ ghost =
  fun program globals lowered context state fuel premise -> ghost_ (
    start program globals lowered context state ();
    Calls.run_def (C.Succ fuel) (State.module_ program lowered context) (configuration context state))

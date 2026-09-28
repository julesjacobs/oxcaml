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
module B = Wasm_u32
module WG = Wasm_globals
module L = Wasm_locals
module Replace = Wasm_local_replace
(* The dispatcher loop, after the prologue. *)
let[@def] (configuration @ total) (context : State.context @ immutable) (state : State.running @ immutable) =
  Dispatch.point (Runtime.loop_code ()) T.No_labels (Registers.globals state.State.registers) state.State.memory S.Empty
    (C.Succ context.State.host_capacity)
(* The dispatcher's entry, before the prologue, with the globals set by the host. *)
let[@def] (initial @ total) (context : State.context @ immutable) (globals : WG.t @ immutable) (memory : B.bytes @ immutable) =
  Dispatch.point (Runtime.dispatcher ()).F.code T.No_labels globals memory S.Empty (C.Succ context.State.host_capacity)
let (start @ total) : (program : I.program) @ immutable -> (globals : Hmc_heap_machine.globals) @ immutable ->
    (lowered : Lower.program) @ immutable -> (context : State.context) @ immutable -> (state : State.running) @ immutable ->
    {u : unit | State.valid program globals lowered context state} ->
    {u : unit | Calls.step (State.module_ program lowered context) (configuration context state) === Calls.Running (State.loop context state)} @ ghost =
  fun program globals lowered context state premise -> ghost_ (
    Runtime.loop_code_def ();
    configuration_def context state;
    Dispatch.point_def (Runtime.loop_code ()) T.No_labels (Registers.globals state.State.registers) state.State.memory S.Empty (C.Succ context.State.host_capacity);
    State.loop_def context state; Dispatch.loop_def (Registers.globals state.State.registers) state.State.memory (C.Succ context.State.host_capacity);
    Dispatch.labels_def (); Dispatch.exit_def ();
    Dispatch.point_def (Runtime.dispatch_body ()) (Dispatch.labels ()) (Registers.globals state.State.registers) state.State.memory S.Empty (C.Succ context.State.host_capacity);
    P.step_def (configuration context state).Calls.current;
    T.step_def (configuration context state).Calls.current.P.body;
    T.enter_def (Runtime.dispatch_body ()) (Dispatch.exit ()) (Some (Runtime.dispatch_body ())) (configuration context state).Calls.current.P.body;
    T.stack_def (configuration context state).Calls.current.P.body.T.state S.Empty;
    Wasm_calls_body.step (State.module_ program lowered context) (configuration context state) (State.loop context state).Calls.current ())
let (enter @ total) : (program : I.program) @ immutable -> (globals : Hmc_heap_machine.globals) @ immutable ->
    (lowered : Lower.program) @ immutable -> (context : State.context) @ immutable -> (state : State.running) @ immutable ->
    (wasm : WG.t) @ immutable ->
    {u : unit | State.valid program globals lowered context state} ->
    {u : unit | Calls.start (State.module_ program lowered context) context.State.block_count state.State.memory
      wasm (C.Succ context.State.host_capacity) === Calls.Running (initial context wasm state.State.memory)} @ ghost =
  fun program globals lowered context state wasm premise -> ghost_ (
    State.valid_def program globals lowered context state;
    Lower.corresponds_def program globals context.State.max_pc lowered;
    Assembly.source_order globals program.I.origin.Hmc_cfg_program.blocks program.I.code lowered.Lower.blocks lowered.Lower.capacity context.State.max_pc context.State.block_count ();
    Assembly.main_correct lowered (G.size program.I.origin.Hmc_cfg_program.blocks) context.State.block_count
      (Runtime.config context.State.table_base context.State.stack_base) (Runtime.dispatcher ()) ();
    State.module__def program lowered context;
    Runtime.dispatcher_def (); F.zero_locals_def F.No_locals;
    Calls.start_def (State.module_ program lowered context) context.State.block_count state.State.memory
      wasm (C.Succ context.State.host_capacity);
    initial_def context wasm state.State.memory;
    Dispatch.point_def (Runtime.dispatcher ()).F.code T.No_labels wasm state.State.memory S.Empty (C.Succ context.State.host_capacity))
(* The host sets global 7 to the input; the prologue then reaches the
   dispatcher loop of [target], whose memory holds the input in the entry
   frame and whose registers are those of [state]. *)
let (launch @ total) : (program : I.program) @ immutable -> (globals : Hmc_heap_machine.globals) @ immutable ->
    (lowered : Lower.program) @ immutable -> (context : State.context) @ immutable -> (state : State.running) @ immutable ->
    (input : Hmc_word64.t) @ immutable -> (target_context : State.context) @ immutable -> (target : State.running) @ immutable ->
    {u : unit | State.valid program globals lowered context state
      && state.State.registers.Registers.payload === Dispatch.cleared ()
      && target.State.registers === state.State.registers && target_context.State.host_capacity === context.State.host_capacity
      && Wasm_memory.store state.State.memory state.State.registers.Registers.frame (Runtime.input_offset ()) (S.I64 input)
        === Some target.State.memory} ->
    {wasm : WG.t | WG.set (Registers.globals state.State.registers) (Dispatch.payload_global ()) (S.I64 input) === Some wasm
      && Calls.start (State.module_ program lowered context) context.State.block_count state.State.memory
        wasm (C.Succ context.State.host_capacity) === Calls.Running (initial context wasm state.State.memory)
      && Calls.run (Dispatch.five ()) (State.module_ program lowered context) (initial context wasm state.State.memory)
        === Calls.Running (configuration target_context target)} @ immutable ghost =
  fun program globals lowered context state input target_context target premise -> ghost_ (
    let registers = state.State.registers in
    let zero : Hmc_word64.t = {Hmc_word64.lo = 0; hi = 0} in
    Registers.globals_def registers; Registers.values_def registers; Registers.permissions_def ();
    let p7 = WG.Global (true, WG.Empty) in
    let p6 = WG.Global (true, p7) in
    let p5 = WG.Global (true, p6) in
    let p4 = WG.Global (false, p5) in
    let p3 = WG.Global (true, p4) in
    let p2 = WG.Global (false, p3) in
    let p1 = WG.Global (true, p2) in
    let p0 = WG.Global (false, p1) in
    let g7 = S.Push (S.I64 registers.Registers.payload, S.Empty) in
    let g6 = S.Push (S.I64 registers.Registers.tag, g7) in
    let g5 = S.Push (S.I32 registers.Registers.status, g6) in
    let g4 = S.Push (S.I32 registers.Registers.stack_limit, g5) in
    let g3 = S.Push (S.I32 registers.Registers.top, g4) in
    let g2 = S.Push (S.I32 registers.Registers.heap_limit, g3) in
    let g1 = S.Push (S.I32 registers.Registers.heap, g2) in
    let g0 = S.Push (S.I32 registers.Registers.frame, g1) in
    let h7 = S.Push (S.I64 input, S.Empty) in
    let h6 = S.Push (S.I64 registers.Registers.tag, h7) in
    let h5 = S.Push (S.I32 registers.Registers.status, h6) in
    let h4 = S.Push (S.I32 registers.Registers.stack_limit, h5) in
    let h3 = S.Push (S.I32 registers.Registers.top, h4) in
    let h2 = S.Push (S.I32 registers.Registers.heap_limit, h3) in
    let h1 = S.Push (S.I32 registers.Registers.heap, h2) in
    let h0 = S.Push (S.I32 registers.Registers.frame, h1) in
    let host = Registers.globals registers in
    let wasm = {WG.values = h0; permissions = Registers.permissions ()} in
    WG.writable_def p0 7; WG.writable_def p1 6; WG.writable_def p2 5; WG.writable_def p3 4;
    WG.writable_def p4 3; WG.writable_def p5 2; WG.writable_def p6 1; WG.writable_def p7 0;
    L.get_def g0 7; L.get_def g1 6; L.get_def g2 5; L.get_def g3 4; L.get_def g4 3; L.get_def g5 2; L.get_def g6 1; L.get_def g7 0;
    L.get_def h0 7; L.get_def h1 6; L.get_def h2 5; L.get_def h3 4; L.get_def h4 3; L.get_def h5 2; L.get_def h6 1; L.get_def h7 0;
    L.get_def h0 0;
    S.same_type_def (S.I64 registers.Registers.payload) (S.I64 input); S.same_type_def (S.I64 input) (S.I64 zero);
    WG.can_set_def host 7 (S.I64 input); L.can_set_def g0 7 (S.I64 input);
    WG.can_set_def wasm 7 (S.I64 zero); L.can_set_def h0 7 (S.I64 zero);
    Replace.replace_def g0 7 (S.I64 input); Replace.replace_def g1 6 (S.I64 input); Replace.replace_def g2 5 (S.I64 input);
    Replace.replace_def g3 4 (S.I64 input); Replace.replace_def g4 3 (S.I64 input); Replace.replace_def g5 2 (S.I64 input);
    Replace.replace_def g6 1 (S.I64 input); Replace.replace_def g7 0 (S.I64 input);
    Replace.replace_def h0 7 (S.I64 zero); Replace.replace_def h1 6 (S.I64 zero); Replace.replace_def h2 5 (S.I64 zero);
    Replace.replace_def h3 4 (S.I64 zero); Replace.replace_def h4 3 (S.I64 zero); Replace.replace_def h5 2 (S.I64 zero);
    Replace.replace_def h6 1 (S.I64 zero); Replace.replace_def h7 0 (S.I64 zero);
    Wasm_register_store.global host 7 (S.I64 input) ();
    Wasm_register_store.global wasm 7 (S.I64 zero) ();
    WG.get_def wasm 0; WG.get_def wasm 7;
    Dispatch.frame_global_def (); Dispatch.payload_global_def (); Dispatch.cleared_def ();
    enter program globals lowered context state wasm ();
    initial_def context wasm state.State.memory;
    configuration_def target_context target;
    Dispatch.prologue (State.module_ program lowered context) wasm host state.State.memory target.State.memory
      (C.Succ context.State.host_capacity) registers.Registers.frame input ();
    wasm)
let (execution @ total) : (program : I.program) @ immutable -> (globals : Hmc_heap_machine.globals) @ immutable ->
    (lowered : Lower.program) @ immutable -> (context : State.context) @ immutable -> (state : State.running) @ immutable ->
    (fuel : C.count) @ immutable ->
    {u : unit | State.valid program globals lowered context state} ->
    {u : unit | Calls.run (C.Succ fuel) (State.module_ program lowered context) (configuration context state)
      === Calls.run fuel (State.module_ program lowered context) (State.loop context state)} @ ghost =
  fun program globals lowered context state fuel premise -> ghost_ (
    start program globals lowered context state ();
    Calls.run_def (C.Succ fuel) (State.module_ program lowered context) (configuration context state))

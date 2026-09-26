module D = Hm_declarative
module I = Hmc_tail_ir
module S = Hmc_cfg_semantics
module U = Hmc_tail_semantics
module T = Hmc_tail_stack
module State = Hmc_wasm_program_state
module Lower = Hmc_wasm_program_lower
module Resources = Hmc_wasm_program_resources
module Registers = Hmc_wasm_program_registers
module Machine = Hmc_heap_machine
module Inv = Hmc_heap_invariant
module Q = Hmc_heap_state
module F = Hmc_heap_frame
module Stack = Hmc_memory_stack

let (running @ total) : (program : I.program) @ immutable ->
    (globals : Machine.globals) @ immutable -> (lowered : Lower.program) @ immutable ->
    (context : State.context) @ immutable -> (state : State.running) @ immutable ->
    {u : unit | State.valid program globals lowered context state && T.no_calls program.I.code} ->
    {u : unit | state.State.frames === Q.Halt
      && state.State.registers.Registers.top = context.State.stack_base} @ ghost =
  fun program globals lowered context state premise -> ghost_ (
    State.valid_def program globals lowered context state;
    T.constant_stack program context.State.input state.State.elapsed ();
    T.stack_bound_def D.Z state.State.abstract;
    Resources.valid_def program globals lowered.Lower.width context.State.stack_base state.State.frame_end
      state.State.abstract state.State.heap state.State.activation state.State.frames
      state.State.registers state.State.memory;
    Inv.valid_def program globals state.State.registers.Registers.heap_limit
      {Machine.heap = state.State.heap; state = Q.Running (state.State.activation, state.State.frames)} state.State.abstract;
    Q.decode_def state.State.heap (Q.Running (state.State.activation, state.State.frames));
    Q.decode_frames_def state.State.heap state.State.frames;
    (match F.decode state.State.heap state.State.activation, Q.decode_frames state.State.heap state.State.frames with
    | Some a, Some frames ->
      T.bounded_def D.Z frames;
      (match state.State.frames with Q.Halt -> () | Q.Frame (a, rest) ->
        match F.decode state.State.heap a, Q.decode_frames state.State.heap rest with
        | Some _, Some _ -> () | _ -> ())
    | _ -> ());
    Stack.related_def program.I.origin.Hmc_cfg_program.blocks lowered.Lower.width state.State.memory
      context.State.stack_base state.State.registers.Registers.top state.State.frames)

module Binary = Hmc_wasm_program_binary
module Init = Hmc_wasm_program_initialize
module Heap = Hmc_heap_initialize
module Constant = Hmc_constant_resources
module Source = Hmc_source_semantics
module Origin = Hmc_monomorphic_simulation
module Tail = Hmc_tail_simulation
module M = Hmc_monomorphic
module Cfg = Hmc_cfg_program
module Closure = Hmc_closure_program
module V = Hm_interpreter_typing
module W = Hmc_word64
module B = Wasm_u32
module C = Wasm_code
module Execute = Wasm_binary_execution
module E = Hmc_wasm_program_execution
module Returned = Hmc_wasm_program_source_execution

let (normal @ total) : (program : I.program) @ immutable -> (layout : Init.layout) @ immutable ->
    (input : W.t) @ immutable -> (memory : B.bytes) @ immutable -> (pages : B.u32) ->
    (compiled : Binary.compiled) @ immutable -> (word : W.t) @ immutable ->
    (source_fuel : D.index) @ immutable ->
    {u : unit | Binary.accepted program layout input memory pages compiled
      && layout.Init.stack_capacity === D.Z
      && T.no_calls program.I.code && Constant.no_allocations program.I.code
      && Source.advance source_fuel
        (Origin.source_start program.I.origin.Cfg.origin.Closure.origin input)
        === Source.Done (V.Word word)} ->
    {out : E.execution |
      E.valid program compiled.Binary.start.Heap.globals compiled.Binary.prepared.Init.lowered
        compiled.Binary.prepared.Init.context out.E.endpoint
      && Returned.returned out.E.endpoint word
      && Execute.run (C.Succ out.E.fuel) compiled.Binary.bytes
        (C.Succ compiled.Binary.prepared.Init.context.State.host_capacity)
        === Execute.Result (E.target compiled.Binary.prepared.Init.context out.E.endpoint)} @ immutable ghost =
  fun program layout input memory pages compiled word source_fuel premise -> ghost_ (
    let start = compiled.Binary.start in
    let prepared = compiled.Binary.prepared in
    let state = prepared.Init.state in
    let context = prepared.Init.context in
    let source = program.I.origin.Cfg.origin.Closure.origin in
    Binary.accepted_def program layout input memory pages compiled;
    Init.correct_def program layout input memory (Init.Initialized (start, prepared));
    Init.installed_def program layout input memory start prepared;
    M.ready_def source;
    let definitions : {d : M.definitions | M.origins d} = source.M.definitions in
    let budget = Tail.source_preservation program definitions input word source_fuel () in
    Constant.heap_plan program budget state.State.abstract ();
    T.initial program input;
    Constant.stack_plan program budget state.State.abstract ();
    Hmc_frame_capacity.le_def D.Z context.State.stack_capacity;
    State.valid_def program start.Heap.globals prepared.Init.lowered context state;
    Resources.valid_def program start.Heap.globals prepared.Init.lowered.Lower.width
      context.State.stack_base state.State.frame_end state.State.abstract state.State.heap
      state.State.activation state.State.frames state.State.registers state.State.memory;
    Inv.valid_def program start.Heap.globals state.State.registers.Registers.heap_limit
      {Machine.heap = state.State.heap; state = Q.Running (state.State.activation, state.State.frames)} state.State.abstract;
    Hmc_heap_extent.fits_def D.Z (Hmc_heap_objects.used state.State.heap) state.State.registers.Registers.heap_limit;
    Binary.normal program layout input memory start prepared pages compiled.Binary.bytes word budget ())

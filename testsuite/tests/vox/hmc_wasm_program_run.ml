module D = Hm_declarative
module I = Hmc_tail_ir
module M = Hmc_monomorphic
module Cfg = Hmc_cfg_program
module Closure = Hmc_closure_program
module Init = Hmc_wasm_program_initialize
module Heap = Hmc_heap_initialize
module State = Hmc_wasm_program_state
module Registers = Hmc_wasm_program_registers
module E = Hmc_wasm_program_execution
module Source = Hmc_wasm_program_source_execution
module Entry = Hmc_wasm_program_state_entry
module Calls = Wasm_calls
module C = Wasm_code
type execution = {start : Heap.start; prepared : Init.prepared; run : E.execution}
type result = Rejected | Initialization_exhausted of Hmc_heap_objects.heap * D.index | Execution of execution
let[@def] (correct @ total) (program : I.program @ immutable) (layout : Init.layout @ immutable)
    (input : Hmc_word64.t @ immutable) (memory : Wasm_u32.bytes @ immutable) (out : result @ immutable) = ghost_ (
  match out with
  | Rejected -> true
  | Initialization_exhausted (heap, code) -> Heap.correct program layout.Init.heap_base layout.Init.heap_limit input (Heap.Heap_exhausted (heap, code))
  | Execution execution ->
    let prepared = execution.prepared in
    Init.correct program layout input memory (Init.Initialized (execution.start, prepared))
    && E.valid program execution.start.Heap.globals prepared.Init.lowered prepared.Init.context execution.run.E.endpoint
    && Calls.start (State.module_ program prepared.Init.lowered prepared.Init.context) prepared.Init.context.State.block_count
      prepared.Init.state.State.memory (Registers.globals prepared.Init.state.State.registers) (C.Succ prepared.Init.context.State.host_capacity)
      === Calls.Running (Entry.configuration prepared.Init.context prepared.Init.state)
    && Calls.run (C.Succ execution.run.E.fuel) (State.module_ program prepared.Init.lowered prepared.Init.context)
      (Entry.configuration prepared.Init.context prepared.Init.state) === E.target prepared.Init.context execution.run.E.endpoint)
let (run @ total) : (program : I.program) @ immutable -> (layout : Init.layout) @ immutable ->
    (input : Hmc_word64.t) @ immutable -> (memory : Wasm_u32.bytes) @ immutable -> (budget : D.index) @ immutable ->
    {u : unit | Init.valid_layout layout memory} -> {out : result | correct program layout input memory out} @ immutable =
  fun program layout input memory budget premise ->
    let initialized = Init.initialize program layout input memory () in
    ghost_ (Init.correct_def program layout input memory initialized);
    match initialized with
    | Init.Layout_rejected -> ghost_ (correct_def program layout input memory Rejected); Rejected
    | Init.Heap_exhausted (heap, code) ->
      let out = Initialization_exhausted (heap, code) in ghost_ (correct_def program layout input memory out); out
    | Init.Initialized (start, prepared) ->
      ghost_ (Init.installed_def program layout input memory start prepared);
      let run = E.run budget program start.Heap.globals prepared.Init.lowered prepared.Init.context prepared.Init.state () in
      ghost_ (Entry.start program start.Heap.globals prepared.Init.lowered prepared.Init.context prepared.Init.state ();
        Entry.execution program start.Heap.globals prepared.Init.lowered prepared.Init.context prepared.Init.state run.E.fuel ());
      let out = Execution {start; prepared; run} in ghost_ (correct_def program layout input memory out); out
let (preservation @ total) : (program : I.program) @ immutable -> (layout : Init.layout) @ immutable ->
    (input : Hmc_word64.t) @ immutable -> (memory : Wasm_u32.bytes) @ immutable ->
    (word : Hmc_word64.t) @ immutable -> (source_fuel : D.index) @ immutable ->
    {u : unit | Init.valid_layout layout memory
      && Hmc_source_semantics.advance source_fuel (Hmc_monomorphic_simulation.source_start program.I.origin.Cfg.origin.Closure.origin input)
        === Hmc_source_semantics.Done (Hm_interpreter_typing.Word word)} ->
    {out : result | correct program layout input memory out
      && (match out with Execution execution -> Source.returned execution.run.E.endpoint word || Source.exhausted execution.run.E.endpoint | _ -> true)} @ immutable =
  fun program layout input memory word source_fuel premise ->
    let initialized = Init.initialize program layout input memory () in
    ghost_ (Init.correct_def program layout input memory initialized);
    match initialized with
    | Init.Layout_rejected -> ghost_ (correct_def program layout input memory Rejected); Rejected
    | Init.Heap_exhausted (heap, code) ->
      let out = Initialization_exhausted (heap, code) in ghost_ (correct_def program layout input memory out); out
    | Init.Initialized (start, prepared) ->
      let source = program.I.origin.Cfg.origin.Closure.origin in
      ghost_ (Init.installed_def program layout input memory start prepared; M.ready_def source);
      let definitions : {d : M.definitions | M.origins d} = refine_ source.M.definitions in
      let run = Source.preservation program definitions start.Heap.globals prepared.Init.lowered prepared.Init.context prepared.Init.state word source_fuel () in
      ghost_ (Entry.start program start.Heap.globals prepared.Init.lowered prepared.Init.context prepared.Init.state ();
        Entry.execution program start.Heap.globals prepared.Init.lowered prepared.Init.context prepared.Init.state run.E.fuel ());
      let out = Execution {start; prepared; run} in ghost_ (correct_def program layout input memory out); out
let (safe @ total) : (program : I.program) @ immutable -> (layout : Init.layout) @ immutable ->
    (input : Hmc_word64.t) @ immutable -> (memory : Wasm_u32.bytes) @ immutable ->
    (start : Heap.start) @ immutable -> (prepared : Init.prepared) @ immutable -> (prefix : C.count) @ immutable ->
    {u : unit | Init.correct program layout input memory (Init.Initialized (start, prepared))} ->
    {u : unit | match Calls.run prefix (State.module_ program prepared.Init.lowered prepared.Init.context)
      (Entry.configuration prepared.Init.context prepared.Init.state) with Calls.Running _ | Calls.Finished _ -> true | _ -> false} @ ghost =
  fun program layout input memory start prepared prefix premise -> ghost_ (
    Init.correct_def program layout input memory (Init.Initialized (start, prepared));
    Init.installed_def program layout input memory start prepared;
    let witness = Hmc_wasm_program_observe.entry program start.Heap.globals prepared.Init.lowered prepared.Init.context prepared.Init.state prefix () in
    Hmc_wasm_program_observe.matches_def prepared.Init.context witness.E.endpoint
      (Calls.run prefix (State.module_ program prepared.Init.lowered prepared.Init.context) (Entry.configuration prepared.Init.context prepared.Init.state)))
let (reflection @ total) : (program : I.program) @ immutable -> (layout : Init.layout) @ immutable ->
    (input : Hmc_word64.t) @ immutable -> (memory : Wasm_u32.bytes) @ immutable ->
    (start : Heap.start) @ immutable -> (prepared : Init.prepared) @ immutable -> (prefix : C.count) @ immutable ->
    (after : Wasm_global_execution.state) @ immutable -> (registers : Registers.registers) @ immutable -> (word : Hmc_word64.t) @ immutable ->
    {u : unit | Init.correct program layout input memory (Init.Initialized (start, prepared))
      && Calls.run prefix (State.module_ program prepared.Init.lowered prepared.Init.context)
        (Entry.configuration prepared.Init.context prepared.Init.state) === Calls.Finished after
      && after.Wasm_global_execution.globals === Registers.globals registers && registers.Registers.status = 1
      && registers.Registers.tag === Hmc_tagged_cell.tag (Hmc_tagged_cell.Word word) && registers.Registers.payload === word
      && after.Wasm_global_execution.execution.Wasm_memory_execution.machine.Wasm_execution.stack
        === Wasm_scalar.Push (Wasm_scalar.I32 registers.Registers.status, Wasm_scalar.Empty)} ->
    {n : D.index | Hmc_source_semantics.advance n
      (Hmc_monomorphic_simulation.source_start program.I.origin.Cfg.origin.Closure.origin input)
      === Hmc_source_semantics.Done (Hm_interpreter_typing.Word word)} @ immutable =
  fun program layout input memory start prepared prefix after registers word premise ->
    ghost_ (Init.correct_def program layout input memory (Init.Initialized (start, prepared));
      Init.installed_def program layout input memory start prepared);
    Hmc_wasm_program_observe.reflection program start.Heap.globals prepared.Init.lowered prepared.Init.context prepared.Init.state prefix after registers word ()

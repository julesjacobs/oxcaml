module I = Hmc_tail_ir
module B = Wasm_u32
module C = Wasm_code
module Binary = Wasm_binary_module
module Execute = Wasm_binary_execution
module Limits = Wasm_limits_section
module Exports = Wasm_export_section
module State = Hmc_wasm_program_state
module Lower = Hmc_wasm_program_lower
module Registers = Hmc_wasm_program_registers
module Init = Hmc_wasm_program_initialize
module Heap = Hmc_heap_initialize
module Entry = Hmc_wasm_program_state_entry
module Run = Hmc_wasm_program_run
module Calls = Wasm_calls
let[@def] (image @ total) (program : I.program @ immutable) (lowered : Lower.program @ immutable)
    (context : State.context @ immutable) (state : State.running @ immutable) (pages : B.u32) : Binary.image @ immutable =
  {Binary.module_ = State.module_ program lowered context; globals = Registers.globals state.State.registers;
   table_limits = {Limits.minimum = context.State.block_count; maximum = context.State.block_count};
   memory_limits = {Limits.minimum = pages; maximum = pages};
   exports = {Exports.run = context.State.block_count; memory = 0; tag = 6; payload = 7}; data = state.State.memory}
let[@def] (emittable @ total) (program : I.program @ immutable) (lowered : Lower.program @ immutable)
    (context : State.context @ immutable) (state : State.running @ immutable) (pages : B.u32) = ghost_ (
  Execute.materialized (image program lowered context state pages) && Binary.encodable (image program lowered context state pages))
let (emit @ total) : (program : I.program) @ immutable -> (lowered : Lower.program) @ immutable ->
    (context : State.context) @ immutable -> (state : State.running) @ immutable -> (pages : B.u32) ->
    {out : B.bytes option | match out with None -> not (emittable program lowered context state pages) | Some bytes ->
      emittable program lowered context state pages &&
      Binary.decode bytes === Some (image program lowered context state pages, B.End) &&
      Execute.materialized (image program lowered context state pages)} @ immutable =
  fun program lowered context state pages ->
    ghost_ (emittable_def program lowered context state pages);
    let image = image program lowered context state pages in
    if not (Execute.materialized image) then None else Binary.encode image B.End
let (correspondence @ total) : (program : I.program) @ immutable -> (layout : Init.layout) @ immutable ->
    (input : Hmc_word64.t) @ immutable -> (memory : B.bytes) @ immutable -> (start : Heap.start) @ immutable ->
    (prepared : Init.prepared) @ immutable -> (pages : B.u32) -> (bytes : B.bytes) @ immutable -> (prefix : C.count) @ immutable ->
    {u : unit | Init.correct program layout input memory (Init.Initialized (start, prepared)) &&
      Binary.decode bytes === Some (image program prepared.Init.lowered prepared.Init.context prepared.Init.state pages, B.End) &&
      Execute.materialized (image program prepared.Init.lowered prepared.Init.context prepared.Init.state pages)} ->
    {u : unit | Execute.run prefix bytes (C.Succ prepared.Init.context.State.host_capacity) ===
      Execute.Result (Calls.run prefix (State.module_ program prepared.Init.lowered prepared.Init.context)
        (Entry.configuration prepared.Init.context prepared.Init.state))} @ ghost =
  fun program layout input memory start prepared pages bytes prefix premise -> ghost_ (
    Init.correct_def program layout input memory (Init.Initialized (start, prepared));
    Init.installed_def program layout input memory start prepared;
    Entry.start program start.Heap.globals prepared.Init.lowered prepared.Init.context prepared.Init.state ();
    image_def program prepared.Init.lowered prepared.Init.context prepared.Init.state pages;
    Execute.correspondence (image program prepared.Init.lowered prepared.Init.context prepared.Init.state pages) bytes prefix
      (C.Succ prepared.Init.context.State.host_capacity) (Entry.configuration prepared.Init.context prepared.Init.state) ())
let (safe @ total) : (program : I.program) @ immutable -> (layout : Init.layout) @ immutable ->
    (input : Hmc_word64.t) @ immutable -> (memory : B.bytes) @ immutable -> (start : Heap.start) @ immutable ->
    (prepared : Init.prepared) @ immutable -> (pages : B.u32) -> (bytes : B.bytes) @ immutable -> (prefix : C.count) @ immutable ->
    {u : unit | Init.correct program layout input memory (Init.Initialized (start, prepared)) &&
      Binary.decode bytes === Some (image program prepared.Init.lowered prepared.Init.context prepared.Init.state pages, B.End) &&
      Execute.materialized (image program prepared.Init.lowered prepared.Init.context prepared.Init.state pages)} ->
    {u : unit | match Execute.run prefix bytes (C.Succ prepared.Init.context.State.host_capacity) with
      Execute.Result (Calls.Running _) | Execute.Result (Calls.Finished _) -> true | _ -> false} @ ghost =
  fun program layout input memory start prepared pages bytes prefix premise -> ghost_ (
    correspondence program layout input memory start prepared pages bytes prefix ();
    Run.safe program layout input memory start prepared prefix ())
let (reflection @ total) : (program : I.program) @ immutable -> (layout : Init.layout) @ immutable ->
    (input : Hmc_word64.t) @ immutable -> (memory : B.bytes) @ immutable -> (start : Heap.start) @ immutable ->
    (prepared : Init.prepared) @ immutable -> (pages : B.u32) -> (bytes : B.bytes) @ immutable -> (prefix : C.count) @ immutable ->
    (after : Wasm_global_execution.state) @ immutable -> (registers : Registers.registers) @ immutable -> (word : Hmc_word64.t) @ immutable ->
    {u : unit | Init.correct program layout input memory (Init.Initialized (start, prepared)) &&
      Binary.decode bytes === Some (image program prepared.Init.lowered prepared.Init.context prepared.Init.state pages, B.End) &&
      Execute.materialized (image program prepared.Init.lowered prepared.Init.context prepared.Init.state pages) &&
      Execute.run prefix bytes (C.Succ prepared.Init.context.State.host_capacity) === Execute.Result (Calls.Finished after) &&
      after.Wasm_global_execution.globals === Registers.globals registers && registers.Registers.status = 1 &&
      registers.Registers.tag === Hmc_tagged_cell.tag (Hmc_tagged_cell.Word word) && registers.Registers.payload === word &&
      after.Wasm_global_execution.execution.Wasm_memory_execution.machine.Wasm_execution.stack ===
        Wasm_scalar.Push (Wasm_scalar.I32 registers.Registers.status, Wasm_scalar.Empty)} ->
    {n : Hm_declarative.index | Hmc_source_semantics.advance n
      (Hmc_monomorphic_simulation.source_start program.I.origin.Hmc_cfg_program.origin.Hmc_closure_program.origin input) ===
      Hmc_source_semantics.Done (Hm_interpreter_typing.Word word)} @ immutable =
  fun program layout input memory start prepared pages bytes prefix after registers word premise ->
    ghost_ (correspondence program layout input memory start prepared pages bytes prefix ());
    Run.reflection program layout input memory start prepared prefix after registers word ()
module E = Hmc_wasm_program_execution
module Source = Hmc_wasm_program_source_execution
module M = Hmc_monomorphic
module D = Hm_declarative
let (preservation @ total) : (program : I.program) @ immutable -> (layout : Init.layout) @ immutable ->
    (input : Hmc_word64.t) @ immutable -> (memory : B.bytes) @ immutable -> (start : Heap.start) @ immutable ->
    (prepared : Init.prepared) @ immutable -> (pages : B.u32) -> (bytes : B.bytes) @ immutable ->
    (word : Hmc_word64.t) @ immutable -> (source_fuel : D.index) @ immutable ->
    {u : unit | Init.correct program layout input memory (Init.Initialized (start, prepared)) &&
      Binary.decode bytes === Some (image program prepared.Init.lowered prepared.Init.context prepared.Init.state pages, B.End) &&
      Execute.materialized (image program prepared.Init.lowered prepared.Init.context prepared.Init.state pages) &&
      Hmc_source_semantics.advance source_fuel
        (Hmc_monomorphic_simulation.source_start program.I.origin.Hmc_cfg_program.origin.Hmc_closure_program.origin input) ===
        Hmc_source_semantics.Done (Hm_interpreter_typing.Word word)} ->
    {out : E.execution | E.valid program start.Heap.globals prepared.Init.lowered prepared.Init.context out.E.endpoint &&
      (Source.returned out.E.endpoint word || Source.exhausted out.E.endpoint) &&
      Execute.run (C.Succ out.E.fuel) bytes (C.Succ prepared.Init.context.State.host_capacity) ===
        Execute.Result (E.target prepared.Init.context out.E.endpoint) &&
      Execute.run (C.Succ out.E.prefix) bytes (C.Succ prepared.Init.context.State.host_capacity) ===
        Execute.Result (Calls.Running (E.checkpoint prepared.Init.context out.E.endpoint))} @ immutable =
  fun program layout input memory start prepared pages bytes word source_fuel premise ->
    let source = program.I.origin.Hmc_cfg_program.origin.Hmc_closure_program.origin in
    ghost_ (Init.correct_def program layout input memory (Init.Initialized (start, prepared));
      Init.installed_def program layout input memory start prepared; M.ready_def source);
    let definitions : {d : M.definitions | M.origins d} = refine_ source.M.definitions in
    let out = Source.preservation program definitions start.Heap.globals prepared.Init.lowered prepared.Init.context prepared.Init.state word source_fuel () in
    ghost_ (Entry.execution program start.Heap.globals prepared.Init.lowered prepared.Init.context prepared.Init.state out.E.fuel ();
      correspondence program layout input memory start prepared pages bytes (C.Succ out.E.fuel) ();
      Entry.execution program start.Heap.globals prepared.Init.lowered prepared.Init.context prepared.Init.state out.E.prefix ();
      correspondence program layout input memory start prepared pages bytes (C.Succ out.E.prefix) ());
    out
let (normal @ total) : (program : I.program) @ immutable -> (layout : Init.layout) @ immutable ->
    (input : Hmc_word64.t) @ immutable -> (memory : B.bytes) @ immutable -> (start : Heap.start) @ immutable ->
    (prepared : Init.prepared) @ immutable -> (pages : B.u32) -> (bytes : B.bytes) @ immutable ->
    (word : Hmc_word64.t) @ immutable -> (budget : D.index) @ immutable ->
    {u : unit | Init.correct program layout input memory (Init.Initialized (start, prepared)) &&
      Binary.decode bytes === Some (image program prepared.Init.lowered prepared.Init.context prepared.Init.state pages, B.End) &&
      Execute.materialized (image program prepared.Init.lowered prepared.Init.context prepared.Init.state pages) &&
      Hmc_tail_semantics.advance program budget prepared.Init.state.State.abstract ===
        Hmc_cfg_semantics.Done (Hmc_closure_semantics.V.Word word) &&
      Hmc_heap_extent.fits (Hmc_heap_demand.heap_plan program budget prepared.Init.state.State.abstract)
        (Hmc_heap_objects.used prepared.Init.state.State.heap) prepared.Init.state.State.registers.Registers.heap_limit &&
      Hmc_frame_capacity.le (Hmc_heap_demand.stack_plan program budget prepared.Init.state.State.abstract) prepared.Init.context.State.stack_capacity} ->
    {out : E.execution | E.valid program start.Heap.globals prepared.Init.lowered prepared.Init.context out.E.endpoint &&
      Source.returned out.E.endpoint word &&
      Execute.run (C.Succ out.E.fuel) bytes (C.Succ prepared.Init.context.State.host_capacity) ===
        Execute.Result (E.target prepared.Init.context out.E.endpoint) &&
      Execute.run (C.Succ out.E.prefix) bytes (C.Succ prepared.Init.context.State.host_capacity) ===
        Execute.Result (Calls.Running (E.checkpoint prepared.Init.context out.E.endpoint))} @ immutable =
  fun program layout input memory start prepared pages bytes word budget premise ->
    ghost_ (Init.correct_def program layout input memory (Init.Initialized (start, prepared));
      Init.installed_def program layout input memory start prepared);
    let out = Source.normal program start.Heap.globals prepared.Init.lowered prepared.Init.context prepared.Init.state word budget () in
    ghost_ (Entry.execution program start.Heap.globals prepared.Init.lowered prepared.Init.context prepared.Init.state out.E.fuel ();
      correspondence program layout input memory start prepared pages bytes (C.Succ out.E.fuel) ();
      Entry.execution program start.Heap.globals prepared.Init.lowered prepared.Init.context prepared.Init.state out.E.prefix ();
      correspondence program layout input memory start prepared pages bytes (C.Succ out.E.prefix) ());
    out
type compiled = {start : Heap.start; prepared : Init.prepared; bytes : B.bytes}
type compilation = Layout_rejected | Initialization_exhausted of Hmc_heap_objects.heap * D.index
  | Encoding_rejected | Compiled of compiled
let[@def] (accepted @ total) (program : I.program @ immutable) (layout : Init.layout @ immutable)
    (input : Hmc_word64.t @ immutable) (memory : B.bytes @ immutable) (pages : B.u32) (compiled : compiled @ immutable) = ghost_ (
  Init.correct program layout input memory (Init.Initialized (compiled.start, compiled.prepared)) &&
  emittable program compiled.prepared.Init.lowered compiled.prepared.Init.context compiled.prepared.Init.state pages &&
  Binary.decode compiled.bytes === Some (image program compiled.prepared.Init.lowered compiled.prepared.Init.context compiled.prepared.Init.state pages, B.End) &&
  Execute.materialized (image program compiled.prepared.Init.lowered compiled.prepared.Init.context compiled.prepared.Init.state pages))
let[@def] (compilable @ total) (program : I.program @ immutable) (layout : Init.layout @ immutable)
    (input : Hmc_word64.t @ immutable) (memory : B.bytes @ immutable) (pages : B.u32) = ghost_ (
  if not (Init.valid_layout layout memory) then false else
  match Init.initialize program layout input memory () with
  | Init.Initialized (_, prepared) -> emittable program prepared.Init.lowered prepared.Init.context prepared.Init.state pages
  | _ -> false)
let (compile @ total) : (program : I.program) @ immutable -> (layout : Init.layout) @ immutable ->
    (input : Hmc_word64.t) @ immutable -> (memory : B.bytes) @ immutable -> (pages : B.u32) ->
    {u : unit | Init.valid_layout layout memory} ->
    {out : compilation | match out with
      | Compiled compiled -> compilable program layout input memory pages && accepted program layout input memory pages compiled
      | Initialization_exhausted (heap, code) -> not (compilable program layout input memory pages) && Heap.correct program layout.Init.heap_base layout.Init.heap_limit input (Heap.Heap_exhausted (heap, code))
      | Layout_rejected | Encoding_rejected -> not (compilable program layout input memory pages)} @ immutable =
  fun program layout input memory pages premise ->
    ghost_ (compilable_def program layout input memory pages);
    let initialized = Init.initialize program layout input memory () in
    ghost_ (Init.correct_def program layout input memory initialized);
    match initialized with
    | Init.Layout_rejected -> Layout_rejected
    | Init.Heap_exhausted (heap, code) -> Initialization_exhausted (heap, code)
    | Init.Initialized (start, prepared) ->
      match emit program prepared.Init.lowered prepared.Init.context prepared.Init.state pages with
      | None -> Encoding_rejected
      | Some bytes ->
        let compiled = {start; prepared; bytes} in
        ghost_ (accepted_def program layout input memory pages compiled); Compiled compiled

let (sufficient @ total) : (program : I.program) @ immutable -> (layout : Init.layout) @ immutable ->
    (input : Hmc_word64.t) @ immutable -> (memory : B.bytes) @ immutable -> (pages : B.u32) ->
    {u : unit | Init.valid_layout layout memory && compilable program layout input memory pages} ->
    {out : compiled | accepted program layout input memory pages out} @ immutable =
  fun program layout input memory pages premise ->
    match compile program layout input memory pages () with
    | Compiled compiled -> compiled | _ -> unreachable_ ()

module Guard = Hmc_failed_guard_calls
module Failed = Hmc_failed_guard_model
module Observe = Hmc_wasm_program_observe
module GE = Wasm_global_execution
module X = Wasm_memory_execution
module Exec = Wasm_execution
module S = Wasm_scalar
type exhaustion = {guard : Guard.witness; remaining : C.count}
let (exhaustion @ total) : (program : I.program) @ immutable -> (layout : Init.layout) @ immutable ->
    (input : Hmc_word64.t) @ immutable -> (memory : B.bytes) @ immutable -> (start : Heap.start) @ immutable ->
    (prepared : Init.prepared) @ immutable -> (pages : B.u32) -> (bytes : B.bytes) @ immutable -> (prefix : C.count) @ immutable ->
    (after : GE.state) @ immutable -> (resource : Failed.resource) @ immutable ->
    {u : unit | Init.correct program layout input memory (Init.Initialized (start, prepared))
      && Binary.decode bytes === Some (image program prepared.Init.lowered prepared.Init.context prepared.Init.state pages, B.End)
      && Execute.materialized (image program prepared.Init.lowered prepared.Init.context prepared.Init.state pages)
      && Execute.run prefix bytes (C.Succ prepared.Init.context.State.host_capacity) === Execute.Result (Calls.Finished after)
      && after.GE.execution.X.machine.Exec.stack === S.Push (S.I32 (Failed.status resource), S.Empty)} ->
    {out : exhaustion | Execute.run out.guard.Guard.prefix bytes (C.Succ prepared.Init.context.State.host_capacity)
        === Execute.Result (Calls.Running out.guard.Guard.before)
      && Failed.failed resource out.guard.Guard.before.Calls.current.Wasm_instance_control.body
      && Calls.run out.remaining (State.module_ program prepared.Init.lowered prepared.Init.context) out.guard.Guard.before
        === Calls.Finished after} @ immutable ghost =
  fun program layout input memory start prepared pages bytes prefix after resource premise -> ghost_ (
    correspondence program layout input memory start prepared pages bytes prefix ();
    Init.correct_def program layout input memory (Init.Initialized (start, prepared));
    Init.installed_def program layout input memory start prepared;
    let guard = Observe.exhaustion program start.Heap.globals prepared.Init.lowered prepared.Init.context prepared.Init.state
      prefix after resource () in
    Guard.reaches_def resource (State.module_ program prepared.Init.lowered prepared.Init.context)
      (Entry.configuration prepared.Init.context prepared.Init.state) guard;
    let remaining = Guard.suffix guard.Guard.prefix prefix (State.module_ program prepared.Init.lowered prepared.Init.context)
      (Entry.configuration prepared.Init.context prepared.Init.state) guard.Guard.before after () in
    correspondence program layout input memory start prepared pages bytes guard.Guard.prefix ();
    {guard; remaining})

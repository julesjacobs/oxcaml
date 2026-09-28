(* Emission, and the theorems about running the bytes.

   [compile] initializes the program for the placeholder input
   (Hmc_wasm_program_initialize) and encodes the module [image]: one
   function per block and then the dispatcher, exported as [run]; the eight
   globals that hold the registers; a table and a memory of fixed size; and
   the initial memory as a data segment. [emit] checks that the image can
   be instantiated ([Execute.materialized]); by the contract of the
   encoder, the bytes decode to [image] ([built]).

   The theorems about runs share one step, [launch]: running the bytes on
   [input] is running the decoded module ([Execute.correspondence]), and
   the five steps of the prologue reach the dispatcher loop in the state
   [target] that the initializer builds for [input] ([targets], from
   Hmc_wasm_program_input). From there the lemmas of Hmc_wasm_program_run
   and Hmc_wasm_program_source_execution apply to the loop, and
   [Body.compose] or [Dispatch.split_prologue] adds the prologue's five
   steps back. *)
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
module Input = Hmc_wasm_program_input
module Dispatch = Hmc_wasm_program_dispatch
module Fuel = Wasm_control_compose
module Body = Wasm_calls_body
module E = Hmc_wasm_program_execution
module Source = Hmc_wasm_program_source_execution
module M = Hmc_monomorphic
module D = Hm_declarative
(* The module was built for the placeholder input. *)
let[@def] (built @ total) (program : I.program @ immutable) (layout : Init.layout @ immutable) (memory : B.bytes @ immutable)
    (start : Heap.start @ immutable) (prepared : Init.prepared @ immutable) (pages : B.u32) (bytes : B.bytes @ immutable) = ghost_ (
  Init.correct program layout (Input.placeholder ()) memory (Init.Initialized (start, prepared))
  && Binary.decode bytes === Some (image program prepared.Init.lowered prepared.Init.context prepared.Init.state pages, B.End)
  && Execute.materialized (image program prepared.Init.lowered prepared.Init.context prepared.Init.state pages))
(* [target] is the state the initializer builds for [input]. *)
let[@def] (targets @ total) (program : I.program @ immutable) (layout : Init.layout @ immutable) (memory : B.bytes @ immutable)
    (start : Heap.start @ immutable) (prepared : Init.prepared @ immutable) (input : Hmc_word64.t @ immutable)
    (target : Init.prepared @ immutable) = ghost_ (
  Init.ready program layout input memory (Input.restart start input) target && Input.retargeted prepared input target)
let (retarget @ total) : (program : I.program) @ immutable -> (layout : Init.layout) @ immutable ->
    (memory : B.bytes) @ immutable -> (start : Heap.start) @ immutable -> (prepared : Init.prepared) @ immutable ->
    (input : Hmc_word64.t) @ immutable ->
    {u : unit | Init.correct program layout (Input.placeholder ()) memory (Init.Initialized (start, prepared))} ->
    {target : Init.prepared | targets program layout memory start prepared input target} @ immutable ghost =
  fun program layout memory start prepared input premise -> ghost_ (
    let target = Input.install program layout memory start prepared input () in
    targets_def program layout memory start prepared input target; target)
(* Running the bytes on [input] enters the dispatcher, whose prologue takes
   five steps to reach the dispatcher loop of [target]. *)
let (launch @ total) : (program : I.program) @ immutable -> (layout : Init.layout) @ immutable ->
    (memory : B.bytes) @ immutable -> (start : Heap.start) @ immutable -> (prepared : Init.prepared) @ immutable ->
    (pages : B.u32) -> (bytes : B.bytes) @ immutable -> (input : Hmc_word64.t) @ immutable ->
    (target : Init.prepared) @ immutable -> (fuel : C.count) @ immutable ->
    {u : unit | built program layout memory start prepared pages bytes && targets program layout memory start prepared input target} ->
    {configuration : Calls.configuration |
      Execute.run fuel bytes input (C.Succ target.Init.context.State.host_capacity)
        === Execute.Result (Calls.run fuel (State.module_ program target.Init.lowered target.Init.context) configuration)
      && Calls.run (Dispatch.five ()) (State.module_ program target.Init.lowered target.Init.context) configuration
        === Calls.Running (Entry.configuration target.Init.context target.Init.state)} @ immutable ghost =
  fun program layout memory start prepared pages bytes input target fuel premise -> ghost_ (
    let old = Input.placeholder () in
    built_def program layout memory start prepared pages bytes;
    targets_def program layout memory start prepared input target;
    Init.correct_def program layout old memory (Init.Initialized (start, prepared));
    Init.installed_def program layout old memory start prepared;
    Input.retargeted_def prepared input target; Dispatch.cleared_def (); Dispatch.payload_global_def ();
    image_def program prepared.Init.lowered prepared.Init.context prepared.Init.state pages;
    State.module__def program prepared.Init.lowered prepared.Init.context;
    State.module__def program target.Init.lowered target.Init.context;
    let wasm = Entry.launch program start.Heap.globals prepared.Init.lowered prepared.Init.context prepared.Init.state
      input target.Init.context target.Init.state () in
    let configuration = Entry.initial prepared.Init.context wasm prepared.Init.state.State.memory in
    Execute.correspondence (image program prepared.Init.lowered prepared.Init.context prepared.Init.state pages) bytes input wasm
      fuel (C.Succ prepared.Init.context.State.host_capacity) configuration ();
    configuration)
let (safe @ total) : (program : I.program) @ immutable -> (layout : Init.layout) @ immutable ->
    (memory : B.bytes) @ immutable -> (start : Heap.start) @ immutable -> (prepared : Init.prepared) @ immutable ->
    (pages : B.u32) -> (bytes : B.bytes) @ immutable -> (input : Hmc_word64.t) @ immutable ->
    (target : Init.prepared) @ immutable -> (prefix : C.count) @ immutable ->
    {u : unit | built program layout memory start prepared pages bytes && targets program layout memory start prepared input target} ->
    {u : unit | match Execute.run prefix bytes input (C.Succ target.Init.context.State.host_capacity) with
      Execute.Result (Calls.Running _) | Execute.Result (Calls.Finished _) -> true | _ -> false} @ ghost =
  fun program layout memory start prepared pages bytes input target prefix premise -> ghost_ (
    targets_def program layout memory start prepared input target;
    let configuration = launch program layout memory start prepared pages bytes input target prefix () in
    let module_ = State.module_ program target.Init.lowered target.Init.context in
    Dispatch.split_prologue module_ configuration (Entry.configuration target.Init.context target.Init.state) prefix ();
    match Dispatch.after_prologue prefix with
    | Some rest -> Run.safe program layout input memory (Input.restart start input) target rest ()
    | None -> ())
let (reflection @ total) : (program : I.program) @ immutable -> (layout : Init.layout) @ immutable ->
    (memory : B.bytes) @ immutable -> (start : Heap.start) @ immutable -> (prepared : Init.prepared) @ immutable ->
    (pages : B.u32) -> (bytes : B.bytes) @ immutable -> (input : Hmc_word64.t) @ immutable ->
    (target : Init.prepared) @ immutable -> (prefix : C.count) @ immutable ->
    (after : Wasm_global_execution.state) @ immutable -> (registers : Registers.registers) @ immutable -> (word : Hmc_word64.t) @ immutable ->
    {u : unit | built program layout memory start prepared pages bytes && targets program layout memory start prepared input target &&
      Execute.run prefix bytes input (C.Succ target.Init.context.State.host_capacity) === Execute.Result (Calls.Finished after) &&
      after.Wasm_global_execution.globals === Registers.globals registers && registers.Registers.status = 1 &&
      registers.Registers.tag === Hmc_tagged_cell.tag (Hmc_tagged_cell.Word word) && registers.Registers.payload === word &&
      after.Wasm_global_execution.execution.Wasm_memory_execution.machine.Wasm_execution.stack ===
        Wasm_scalar.Push (Wasm_scalar.I32 registers.Registers.status, Wasm_scalar.Empty)} ->
    {n : Hm_declarative.index | Hmc_source_semantics.advance n
      (Hmc_monomorphic_simulation.source_start program.I.origin.Hmc_cfg_program.origin.Hmc_closure_program.origin input) ===
      Hmc_source_semantics.Done (Hm_interpreter_typing.Word word)} @ immutable ghost =
  fun program layout memory start prepared pages bytes input target prefix after registers word premise -> ghost_ (
    targets_def program layout memory start prepared input target;
    let configuration = launch program layout memory start prepared pages bytes input target prefix () in
    let module_ = State.module_ program target.Init.lowered target.Init.context in
    Dispatch.split_prologue module_ configuration (Entry.configuration target.Init.context target.Init.state) prefix ();
    match Dispatch.after_prologue prefix with
    | Some rest -> Run.reflection program layout input memory (Input.restart start input) target rest after registers word ()
    | None -> unreachable_ ())
let (preservation @ total) : (program : I.program) @ immutable -> (layout : Init.layout) @ immutable ->
    (memory : B.bytes) @ immutable -> (start : Heap.start) @ immutable -> (prepared : Init.prepared) @ immutable ->
    (pages : B.u32) -> (bytes : B.bytes) @ immutable -> (input : Hmc_word64.t) @ immutable ->
    (target : Init.prepared) @ immutable -> (word : Hmc_word64.t) @ immutable -> (source_fuel : D.index) @ immutable ->
    {u : unit | built program layout memory start prepared pages bytes && targets program layout memory start prepared input target &&
      Hmc_source_semantics.advance source_fuel
        (Hmc_monomorphic_simulation.source_start program.I.origin.Hmc_cfg_program.origin.Hmc_closure_program.origin input) ===
        Hmc_source_semantics.Done (Hm_interpreter_typing.Word word)} ->
    {out : E.execution | E.valid program start.Heap.globals target.Init.lowered target.Init.context out.E.endpoint &&
      (Source.returned out.E.endpoint word || Source.exhausted out.E.endpoint) &&
      Execute.run (Fuel.add (Dispatch.five ()) (C.Succ out.E.fuel)) bytes input (C.Succ target.Init.context.State.host_capacity) ===
        Execute.Result (E.target target.Init.context out.E.endpoint) &&
      Execute.run (Fuel.add (Dispatch.five ()) (C.Succ out.E.prefix)) bytes input (C.Succ target.Init.context.State.host_capacity) ===
        Execute.Result (Calls.Running (E.checkpoint target.Init.context out.E.endpoint))} @ immutable ghost =
  fun program layout memory start prepared pages bytes input target word source_fuel premise -> ghost_ (
    let source = program.I.origin.Hmc_cfg_program.origin.Hmc_closure_program.origin in
    let next = Input.restart start input in
    targets_def program layout memory start prepared input target;
    Init.ready_def program layout input memory next target;
    Init.installed_def program layout input memory next target; M.ready_def source;
    Input.restart_def start input;
    let definitions : {d : M.definitions | M.origins d} = refine_ source.M.definitions in
    let out = Source.preservation program definitions start.Heap.globals target.Init.lowered target.Init.context target.Init.state
      word source_fuel () in
    let module_ = State.module_ program target.Init.lowered target.Init.context in
    Entry.execution program start.Heap.globals target.Init.lowered target.Init.context target.Init.state out.E.fuel ();
    Entry.execution program start.Heap.globals target.Init.lowered target.Init.context target.Init.state out.E.prefix ();
    let finished = launch program layout memory start prepared pages bytes input target (Fuel.add (Dispatch.five ()) (C.Succ out.E.fuel)) () in
    Body.compose (Dispatch.five ()) (C.Succ out.E.fuel) module_ finished;
    let checkpoint = launch program layout memory start prepared pages bytes input target (Fuel.add (Dispatch.five ()) (C.Succ out.E.prefix)) () in
    Body.compose (Dispatch.five ()) (C.Succ out.E.prefix) module_ checkpoint;
    out)
let (normal @ total) : (program : I.program) @ immutable -> (layout : Init.layout) @ immutable ->
    (memory : B.bytes) @ immutable -> (start : Heap.start) @ immutable -> (prepared : Init.prepared) @ immutable ->
    (pages : B.u32) -> (bytes : B.bytes) @ immutable -> (input : Hmc_word64.t) @ immutable ->
    (target : Init.prepared) @ immutable -> (word : Hmc_word64.t) @ immutable -> (budget : D.index) @ immutable ->
    {u : unit | built program layout memory start prepared pages bytes && targets program layout memory start prepared input target &&
      Hmc_tail_semantics.advance program budget target.Init.state.State.abstract ===
        Hmc_cfg_semantics.Done (Hmc_closure_semantics.V.Word word) &&
      Hmc_heap_extent.fits (Hmc_heap_demand.heap_plan program budget target.Init.state.State.abstract)
        (Hmc_heap_objects.used target.Init.state.State.heap) target.Init.state.State.registers.Registers.heap_limit &&
      Hmc_frame_capacity.le (Hmc_heap_demand.stack_plan program budget target.Init.state.State.abstract) target.Init.context.State.stack_capacity} ->
    {out : E.execution | E.valid program start.Heap.globals target.Init.lowered target.Init.context out.E.endpoint &&
      Source.returned out.E.endpoint word &&
      Execute.run (Fuel.add (Dispatch.five ()) (C.Succ out.E.fuel)) bytes input (C.Succ target.Init.context.State.host_capacity) ===
        Execute.Result (E.target target.Init.context out.E.endpoint) &&
      Execute.run (Fuel.add (Dispatch.five ()) (C.Succ out.E.prefix)) bytes input (C.Succ target.Init.context.State.host_capacity) ===
        Execute.Result (Calls.Running (E.checkpoint target.Init.context out.E.endpoint))} @ immutable ghost =
  fun program layout memory start prepared pages bytes input target word budget premise -> ghost_ (
    let next = Input.restart start input in
    targets_def program layout memory start prepared input target;
    Init.ready_def program layout input memory next target;
    Init.installed_def program layout input memory next target;
    Input.restart_def start input;
    let out = Source.normal program start.Heap.globals target.Init.lowered target.Init.context target.Init.state word budget () in
    let module_ = State.module_ program target.Init.lowered target.Init.context in
    Entry.execution program start.Heap.globals target.Init.lowered target.Init.context target.Init.state out.E.fuel ();
    Entry.execution program start.Heap.globals target.Init.lowered target.Init.context target.Init.state out.E.prefix ();
    let finished = launch program layout memory start prepared pages bytes input target (Fuel.add (Dispatch.five ()) (C.Succ out.E.fuel)) () in
    Body.compose (Dispatch.five ()) (C.Succ out.E.fuel) module_ finished;
    let checkpoint = launch program layout memory start prepared pages bytes input target (Fuel.add (Dispatch.five ()) (C.Succ out.E.prefix)) () in
    Body.compose (Dispatch.five ()) (C.Succ out.E.prefix) module_ checkpoint;
    out)
(* Compilation. [accepted] is what the theorems above need of the result.
   [compilable] says that initialization and encoding succeed; each
   rejection comes with its negation. *)
type compiled = {start : Heap.start; prepared : Init.prepared; bytes : B.bytes}
type compilation = Layout_rejected | Initialization_exhausted of Hmc_heap_objects.heap * D.index
  | Encoding_rejected | Compiled of compiled
let[@def] (accepted @ total) (program : I.program @ immutable) (layout : Init.layout @ immutable)
    (memory : B.bytes @ immutable) (pages : B.u32) (compiled : compiled @ immutable) = ghost_ (
  built program layout memory compiled.start compiled.prepared pages compiled.bytes &&
  emittable program compiled.prepared.Init.lowered compiled.prepared.Init.context compiled.prepared.Init.state pages)
let[@def] (compilable @ total) (program : I.program @ immutable) (layout : Init.layout @ immutable)
    (memory : B.bytes @ immutable) (pages : B.u32) = ghost_ (
  if not (Init.valid_layout layout memory) then false else
  match Init.initialize program layout (Input.placeholder ()) memory () with
  | Init.Initialized (_, prepared) -> emittable program prepared.Init.lowered prepared.Init.context prepared.Init.state pages
  | _ -> false)
let (compile @ total) : (program : I.program) @ immutable -> (layout : Init.layout) @ immutable ->
    (memory : B.bytes) @ immutable -> (pages : B.u32) ->
    {u : unit | Init.valid_layout layout memory} ->
    {out : compilation | match out with
      | Compiled compiled -> compilable program layout memory pages && accepted program layout memory pages compiled
      | Initialization_exhausted (heap, code) -> not (compilable program layout memory pages)
        && Heap.correct program layout.Init.heap_base layout.Init.heap_limit (Input.placeholder ()) (Heap.Heap_exhausted (heap, code))
      | Layout_rejected | Encoding_rejected -> not (compilable program layout memory pages)} @ immutable =
  fun program layout memory pages premise ->
    let input = Input.placeholder () in
    ghost_ (compilable_def program layout memory pages);
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
        ghost_ (built_def program layout memory start prepared pages bytes; accepted_def program layout memory pages compiled);
        Compiled compiled

let (sufficient @ total) : (program : I.program) @ immutable -> (layout : Init.layout) @ immutable ->
    (memory : B.bytes) @ immutable -> (pages : B.u32) ->
    {u : unit | Init.valid_layout layout memory && compilable program layout memory pages} ->
    {out : compiled | accepted program layout memory pages out} @ immutable =
  fun program layout memory pages premise ->
    match compile program layout memory pages () with
    | Compiled compiled -> compiled | _ -> unreachable_ ()

(* A run that finished with a status of exhaustion stopped at a guard that
   failed: Hmc_wasm_program_observe finds the block step that ran it. *)
module Guard = Hmc_failed_guard_calls
module Failed = Hmc_failed_guard_model
module Observe = Hmc_wasm_program_observe
module GE = Wasm_global_execution
module X = Wasm_memory_execution
module Exec = Wasm_execution
module S = Wasm_scalar
type exhaustion = {guard : Guard.witness; remaining : C.count}
let (exhaustion @ total) : (program : I.program) @ immutable -> (layout : Init.layout) @ immutable ->
    (memory : B.bytes) @ immutable -> (start : Heap.start) @ immutable -> (prepared : Init.prepared) @ immutable ->
    (pages : B.u32) -> (bytes : B.bytes) @ immutable -> (input : Hmc_word64.t) @ immutable ->
    (target : Init.prepared) @ immutable -> (prefix : C.count) @ immutable ->
    (after : GE.state) @ immutable -> (resource : Failed.resource) @ immutable ->
    {u : unit | built program layout memory start prepared pages bytes && targets program layout memory start prepared input target
      && Execute.run prefix bytes input (C.Succ target.Init.context.State.host_capacity) === Execute.Result (Calls.Finished after)
      && after.GE.execution.X.machine.Exec.stack === S.Push (S.I32 (Failed.status resource), S.Empty)} ->
    {out : exhaustion | Execute.run (Fuel.add (Dispatch.five ()) out.guard.Guard.prefix) bytes input (C.Succ target.Init.context.State.host_capacity)
        === Execute.Result (Calls.Running out.guard.Guard.before)
      && Failed.failed resource out.guard.Guard.before.Calls.current.Wasm_instance_control.body
      && Calls.run out.remaining (State.module_ program target.Init.lowered target.Init.context) out.guard.Guard.before
        === Calls.Finished after} @ immutable ghost =
  fun program layout memory start prepared pages bytes input target prefix after resource premise -> ghost_ (
    let next = Input.restart start input in
    targets_def program layout memory start prepared input target;
    Init.ready_def program layout input memory next target;
    Init.installed_def program layout input memory next target;
    Input.restart_def start input;
    let module_ = State.module_ program target.Init.lowered target.Init.context in
    let entry = Entry.configuration target.Init.context target.Init.state in
    let configuration = launch program layout memory start prepared pages bytes input target prefix () in
    Dispatch.split_prologue module_ configuration entry prefix ();
    match Dispatch.after_prologue prefix with
    | None -> unreachable_ ()
    | Some rest ->
      let guard = Observe.exhaustion program start.Heap.globals target.Init.lowered target.Init.context target.Init.state
        rest after resource () in
      Guard.reaches_def resource module_ entry guard;
      let remaining = Guard.suffix guard.Guard.prefix rest module_ entry guard.Guard.before after () in
      let reached = launch program layout memory start prepared pages bytes input target (Fuel.add (Dispatch.five ()) guard.Guard.prefix) () in
      Body.compose (Dispatch.five ()) guard.Guard.prefix module_ reached;
      {guard; remaining})

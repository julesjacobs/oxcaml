module D = Hm_declarative
module W = Hmc_word64
module B = Wasm_u32
module C = Wasm_code
module M = Hmc_compilation_model
module Core = Hmc_compiler
module Init = Hmc_wasm_program_initialize
module R = Hmc_wasm_program_registers
module S = Wasm_scalar
module G = Wasm_globals
module GE = Wasm_global_execution

type payload = {code : B.bytes; term : D.term @@ ghost; argument : W.t @@ ghost;
  configuration : M.layout @@ ghost; memory : B.bytes @@ ghost; pages : B.u32 @@ ghost;
  evidence : Core.artifact @@ ghost}
type artifact = {a : payload | a.code === a.evidence.Core.bytes
  && Core.correct a.term a.configuration a.argument a.memory a.pages a.evidence}
type error = Unbound_variable | Type_error | Entry_type_mismatch
  | Unsupported_polymorphic_local_let | Non_callable_outer_binding
  | Non_callable_entry | Invalid_annotation | Layout_rejected
  | Initialization_exhausted | Encoding_rejected
type result = Rejected of error | Compiled of artifact

let[@def] (bytes @ total) (artifact : artifact @ immutable) = artifact.code
let[@def] (source @ total) (artifact : artifact @ immutable) = ghost_ artifact.term
let[@def] (input @ total) (artifact : artifact @ immutable) = ghost_ artifact.argument
let[@def] (layout @ total) (artifact : artifact @ immutable) = ghost_ artifact.configuration

let compile : (term : D.term) @ immutable -> (configuration : M.layout) @ immutable ->
    (argument : W.t) @ immutable -> (memory : B.bytes) @ immutable -> (pages : B.u32) ->
    {u : unit | M.valid_layout configuration memory} ->
    {out : result | match out with Rejected _ -> true | Compiled artifact ->
      source artifact === term && input artifact === argument && layout artifact === configuration} @ immutable =
  fun term configuration argument memory pages premise ->
    ghost_ (M.valid_layout_def configuration memory;
      Init.valid_layout_def configuration memory; Hmc_linear_bounds.covers_def memory configuration.M.heap_limit);
    match Core.compile term configuration argument memory pages () with
    | Core.Compiled evidence ->
      let artifact : artifact = {code = evidence.Core.bytes; term = ghost_ term;
        argument = ghost_ argument; configuration = ghost_ configuration; memory = ghost_ memory;
        pages = ghost_ pages; evidence = ghost_ evidence} in
      ghost_ (source_def artifact; input_def artifact; layout_def artifact);
      Compiled artifact
    | Core.Unbound_variable -> Rejected Unbound_variable
    | Core.Type_error -> Rejected Type_error
    | Core.Entry_type_mismatch -> Rejected Entry_type_mismatch
    | Core.Unsupported_fragment error -> Rejected (match error with
      | Hmc_admission.Unsupported_polymorphic_local_let -> Unsupported_polymorphic_local_let
      | Hmc_admission.Non_callable_outer_binding -> Non_callable_outer_binding
      | Hmc_admission.Non_callable_entry -> Non_callable_entry
      | Hmc_admission.Invalid_annotation -> Invalid_annotation)
    | Core.Layout_rejected -> Rejected Layout_rejected
    | Core.Initialization_exhausted _ -> Rejected Initialization_exhausted
    | Core.Encoding_rejected -> Rejected Encoding_rejected

let (safe @ total) : (artifact : artifact) @ immutable -> (prefix : C.count) @ immutable ->
    {u : unit | match Wasm_binary_execution.run prefix (bytes artifact)
        (C.Succ (layout artifact).M.host_capacity) with
      Wasm_binary_execution.Result (Wasm_calls.Running _)
      | Wasm_binary_execution.Result (Wasm_calls.Finished _) -> true | _ -> false} @ ghost =
  fun artifact prefix -> ghost_ (
    bytes_def artifact; layout_def artifact; Core.safe artifact.term artifact.configuration artifact.argument artifact.memory
      artifact.pages artifact.evidence prefix ())

let (static_validity @ total) : (artifact : artifact) @ immutable ->
    {u : unit | Wasm_static_module.bytes_valid (bytes artifact)} @ ghost =
  fun artifact -> ghost_ (
    bytes_def artifact;
    Core.static_validity artifact.term artifact.configuration artifact.argument
      artifact.memory artifact.pages artifact.evidence ())

let (reflection @ total) : (artifact : artifact) @ immutable -> (prefix : C.count) @ immutable ->
    (after : GE.state) @ immutable -> (word : W.t) @ immutable ->
    {u : unit | Wasm_binary_execution.run prefix (bytes artifact)
        (C.Succ (layout artifact).M.host_capacity) === Wasm_binary_execution.Result (Wasm_calls.Finished after)
      && M.returned after word} ->
    {fuel : D.index | M.source_returns (source artifact) (input artifact) fuel word} @ immutable ghost =
  fun artifact prefix after word premise -> ghost_ (
    bytes_def artifact; source_def artifact; input_def artifact; layout_def artifact;
    M.returned_def after word;
    match after.GE.globals.G.values, after.GE.globals.G.permissions with
    | S.Push (S.I32 frame, S.Push (S.I32 heap, S.Push (S.I32 heap_limit, S.Push (S.I32 top,
        S.Push (S.I32 stack_limit, S.Push (S.I32 status, S.Push (S.I64 tag, S.Push (S.I64 payload, S.Empty)))))))),
      G.Global (false, G.Global (true, G.Global (false, G.Global (true, G.Global (false,
        G.Global (true, G.Global (true, G.Global (true, G.Empty)))))))) ->
      let registers = {R.frame; heap; heap_limit; top; stack_limit; status; tag; payload} in
      R.globals_def registers; R.values_def registers; R.permissions_def ();
      Hmc_tagged_cell.tag_def (Hmc_tagged_cell.Word word);
      let fuel = Core.reflection artifact.term artifact.configuration artifact.argument
        artifact.memory artifact.pages artifact.evidence prefix after registers word () in
      M.source_returns_def artifact.term artifact.argument fuel word; fuel
    | _ -> unreachable_ ())

module Binary = Hmc_wasm_program_binary
module E = Hmc_wasm_program_execution
module State = Hmc_wasm_program_state
module Result = Hmc_wasm_program_step_result
module Resource = Hmc_wasm_program_resource_step
module Source = Hmc_wasm_program_source_execution

let (observe @ total) : (program : Hmc_tail_ir.program) @ immutable ->
    (globals : Hmc_heap_machine.globals) @ immutable -> (lowered : Hmc_wasm_program_lower.program) @ immutable ->
    (context : State.context) @ immutable -> (endpoint : E.endpoint) @ immutable -> (word : W.t) @ immutable ->
    {u : unit | E.valid program globals lowered context endpoint
      && (Source.returned endpoint word || Source.exhausted endpoint)} ->
    {after : GE.state | E.target context endpoint === Wasm_calls.Finished after
      && (M.returned after word || M.exhausted after)
      && (if Source.returned endpoint word then M.returned after word else true)} @ immutable ghost =
  fun program globals lowered context endpoint word premise -> ghost_ (
    E.valid_def program globals lowered
      context endpoint;
    Source.returned_def endpoint word; Source.exhausted_def endpoint;
    E.target_def context endpoint;
    match endpoint with
    | E.Stopped (before, result) ->
      Result.correct_def program globals lowered
        context before result;
      let registers = match result with
        | Result.Returned final -> final.Result.registers
        | Result.Exhausted final -> final.Resource.state.State.registers
        | Result.Continued _ -> unreachable_ () in
      Result.finished_def before registers;
      R.globals_def registers; R.values_def registers; R.permissions_def ();
      Hmc_tagged_cell.tag_def (Hmc_tagged_cell.Word word);
      Hmc_tagged_cell.payload_def (Hmc_tagged_cell.Word word);
      let after = {GE.globals = R.globals registers;
        execution = {Wasm_memory_execution.memory = before.State.memory;
          machine = {Wasm_execution.locals = S.Empty; stack = S.Push (S.I32 registers.R.status, S.Empty)}}} in
      M.returned_def after word; M.exhausted_def after;
      after
    | E.Paused _ -> unreachable_ ())


let (preservation @ total) : (artifact : artifact) @ immutable -> (word : W.t) @ immutable ->
    (source_fuel : D.index) @ immutable ->
    {u : unit | M.source_returns (source artifact) (input artifact) source_fuel word} ->
    {out : M.execution | Wasm_binary_execution.run out.M.fuel (bytes artifact)
        (C.Succ (layout artifact).M.host_capacity) === Wasm_binary_execution.Result (Wasm_calls.Finished out.M.after)
      && (M.returned out.M.after word || M.exhausted out.M.after)} @ immutable ghost =
  fun artifact word source_fuel premise -> ghost_ (
    bytes_def artifact; source_def artifact; input_def artifact; layout_def artifact;
    M.source_returns_def artifact.term artifact.argument source_fuel word;
    let program = artifact.evidence.Core.program in
    let binary = artifact.evidence.Core.binary in
    let prepared = binary.Binary.prepared in
    Core.correct_def artifact.term artifact.configuration artifact.argument
      artifact.memory artifact.pages artifact.evidence;
    Binary.accepted_def program artifact.configuration artifact.argument
      artifact.memory artifact.pages binary;
    Core.origin_def program;
    Hmc_monomorphic_simulation.source_start_def
      program.Hmc_tail_ir.origin.Hmc_cfg_program.origin.Hmc_closure_program.origin artifact.argument;
    let execution = Binary.preservation program artifact.configuration artifact.argument
      artifact.memory binary.Binary.start prepared artifact.pages binary.Binary.bytes word source_fuel () in
    let after = observe program binary.Binary.start.Hmc_heap_initialize.globals prepared.Init.lowered
      prepared.Init.context execution.E.endpoint word () in
    {M.fuel = C.Succ execution.E.fuel; after})

let rec (stack_fits_agrees @ total) : (frames : D.index) @ immutable -> (capacity : D.index) @ immutable ->
    {u : unit | M.stack_fits frames capacity = Hmc_frame_capacity.le frames capacity} @ ghost =
  fun frames capacity -> ghost_ (
    M.stack_fits_def frames capacity; Hmc_frame_capacity.le_def frames capacity;
    match frames, capacity with D.S rest, D.S remaining -> stack_fits_agrees rest remaining | _ -> ())

let rec (target_budget_agrees @ total) : (fuel : D.index) @ immutable ->
    {u : unit | M.target_budget fuel === Hmc_cfg_height.twice (D.S fuel)} @ ghost = fun fuel -> ghost_ (
  M.target_budget_def fuel; Hmc_cfg_height.twice_def (D.S fuel); Hmc_cfg_height.twice_def fuel;
  match fuel with D.Z -> () | D.S rest -> target_budget_agrees rest)

let rec (heap_budget_agrees @ total) : (steps : D.index) @ immutable -> (width : W.limb) ->
    (start : W.limb) -> (limit : W.limb) ->
    {u : unit | M.heap_budget steps width start limit = Hmc_resource_numbers.heap_budget steps width start limit} @ ghost =
  fun steps width start limit -> ghost_ (
    M.heap_budget_def steps width start limit; Hmc_resource_numbers.heap_budget_def steps width start limit;
    if start > limit then () else match steps with D.Z -> () | D.S rest ->
      if width > 268435455 || 16 * width > limit - start then ()
      else heap_budget_agrees rest width (start + 16 * width) limit)

let (normal @ total) : (artifact : artifact) @ immutable -> (word : W.t) @ immutable ->
    (source_fuel : D.index) @ immutable ->
    {u : unit | M.source_returns (source artifact) (input artifact) source_fuel word
      && M.sufficient (layout artifact) (bytes artifact) source_fuel} ->
    {out : M.execution | Wasm_binary_execution.run out.M.fuel (bytes artifact)
        (C.Succ (layout artifact).M.host_capacity) === Wasm_binary_execution.Result (Wasm_calls.Finished out.M.after)
      && M.returned out.M.after word} @ immutable ghost = fun artifact word source_fuel premise -> ghost_ (
    bytes_def artifact; source_def artifact; input_def artifact; layout_def artifact;
    M.source_returns_def artifact.term artifact.argument source_fuel word;
    let program = artifact.evidence.Core.program in
    let binary = artifact.evidence.Core.binary in
    let prepared = binary.Binary.prepared in
    let state = prepared.Init.state in
    let context = prepared.Init.context in
    let lowered = prepared.Init.lowered in
    let globals = binary.Binary.start.Hmc_heap_initialize.globals in
    Core.correct_def artifact.term artifact.configuration artifact.argument
      artifact.memory artifact.pages artifact.evidence;
    Binary.accepted_def program artifact.configuration artifact.argument
      artifact.memory artifact.pages binary;
    Init.correct_def program artifact.configuration artifact.argument artifact.memory
      (Init.Initialized (binary.Binary.start, prepared));
    Init.installed_def program artifact.configuration artifact.argument artifact.memory
      binary.Binary.start prepared;
    Core.origin_def program;
    let monomorphic = program.Hmc_tail_ir.origin.Hmc_cfg_program.origin.Hmc_closure_program.origin in
    Hmc_monomorphic_simulation.source_start_def monomorphic artifact.argument;
    Hmc_monomorphic.ready_def monomorphic;
    let budget = Hmc_tail_simulation.source_preservation program monomorphic.Hmc_monomorphic.definitions
      artifact.argument word source_fuel () in
    Hmc_heap_bound.initial_plans program artifact.argument budget (Hmc_cfg_height.twice (D.S source_fuel)) ();
    Hmc_resource_numbers.frame_capacity program globals lowered context state ();
    target_budget_agrees source_fuel;
    M.frame_room_def artifact.configuration;
    Hmc_resource_numbers.frame_room_def artifact.configuration.M.frame_base artifact.configuration.M.stack_base;
    M.sufficient_def artifact.configuration artifact.code source_fuel;
    Binary.image_def program lowered context state artifact.pages;
    R.globals_def state.State.registers; R.values_def state.State.registers;
    State.valid_def program globals lowered context state;
    Hmc_wasm_program_resources.valid_def program globals lowered.Hmc_wasm_program_lower.width context.State.stack_base state.State.frame_end
      state.State.abstract state.State.heap state.State.activation state.State.frames state.State.registers state.State.memory;
    let steps = M.target_budget source_fuel in
    let capacity = Hmc_frame_capacity.capacity program.Hmc_tail_ir.origin.Hmc_cfg_program.blocks in
    let heap_start = Hmc_heap_objects.used state.State.heap in
    let heap_cells = Hmc_heap_demand.heap_plan program budget state.State.abstract in
    let stack_frames = Hmc_heap_demand.stack_plan program budget state.State.abstract in
    heap_budget_agrees steps (M.frame_room artifact.configuration) heap_start artifact.configuration.M.heap_limit;
    Hmc_resource_numbers.product_fits steps capacity (M.frame_room artifact.configuration)
      heap_start artifact.configuration.M.heap_limit ();
    Hmc_resource_numbers.fits_fewer heap_cells (Hmc_heap_bound.product steps capacity)
      heap_start artifact.configuration.M.heap_limit ();
    stack_fits_agrees steps artifact.configuration.M.stack_capacity;
    Hmc_heap_bound.le_agrees stack_frames steps;
    Hmc_frame_capacity.transitive stack_frames steps artifact.configuration.M.stack_capacity ();
    let execution = Binary.normal program artifact.configuration artifact.argument
      artifact.memory binary.Binary.start prepared artifact.pages binary.Binary.bytes word budget () in
    let after = observe program globals lowered context execution.E.endpoint word () in
    {M.fuel = C.Succ execution.E.fuel; after})

module Failed = Hmc_failed_guard_model
module Guard = Hmc_failed_guard_calls
let (exhaustion @ total) : (artifact : artifact) @ immutable -> (prefix : C.count) @ immutable ->
    (after : GE.state) @ immutable ->
    {u : unit | Wasm_binary_execution.run prefix (bytes artifact)
        (C.Succ (layout artifact).M.host_capacity) === Wasm_binary_execution.Result (Wasm_calls.Finished after)
      && M.exhausted after} ->
    {witness : M.exhaustion | M.honest_exhaustion (bytes artifact)
      (C.Succ (layout artifact).M.host_capacity) after witness} @ immutable ghost =
  fun artifact prefix after premise -> ghost_ (
    bytes_def artifact; layout_def artifact; M.exhausted_def after;
    let resource = match after.GE.execution.Wasm_memory_execution.machine.Wasm_execution.stack with
      | S.Push (S.I32 status, S.Empty) -> if status = 2 then Failed.Heap else Failed.Stack
      | _ -> unreachable_ () in
    Failed.status_def resource;
    let program = artifact.evidence.Core.program in
    let binary = artifact.evidence.Core.binary in
    let prepared = binary.Binary.prepared in
    Core.correct_def artifact.term artifact.configuration artifact.argument
      artifact.memory artifact.pages artifact.evidence;
    Binary.accepted_def program artifact.configuration artifact.argument
      artifact.memory artifact.pages binary;
    Init.correct_def program artifact.configuration artifact.argument artifact.memory
      (Init.Initialized (binary.Binary.start, prepared));
    Init.installed_def program artifact.configuration artifact.argument artifact.memory binary.Binary.start prepared;
    let out = Binary.exhaustion program artifact.configuration artifact.argument
      artifact.memory binary.Binary.start prepared artifact.pages binary.Binary.bytes prefix after resource () in
    let witness = {M.prefix = out.Binary.guard.Guard.prefix; before = out.Binary.guard.Guard.before;
      remaining = out.Binary.remaining} in
    Binary.image_def program prepared.Init.lowered prepared.Init.context prepared.Init.state artifact.pages;
    M.honest_exhaustion_def artifact.code (C.Succ artifact.configuration.M.host_capacity) after witness;
    witness)

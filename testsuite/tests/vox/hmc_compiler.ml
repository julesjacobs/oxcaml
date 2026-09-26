module D = Hm_declarative
module W = Hmc_word64
module B = Wasm_u32
module C = Wasm_code
module S = Hmc_specialization
module M = Hmc_monomorphic
module T = Hmc_templates
module Closure = Hmc_closure_program
module Cfg = Hmc_cfg_program
module Tail = Hmc_tail_ir
module Init = Hmc_wasm_program_initialize
module Binary = Hmc_wasm_program_binary
module Execute = Wasm_binary_execution
module Registers = Hmc_wasm_program_registers
module State = Hmc_wasm_program_state
module Calls = Wasm_calls
type artifact = {bytes : B.bytes; program : Tail.program @@ ghost; binary : Binary.compiled @@ ghost}
type result = Unbound_variable | Type_error | Entry_type_mismatch
  | Unsupported_fragment of Hmc_admission.error | Layout_rejected
  | Initialization_exhausted of Hmc_heap_objects.heap * D.index | Encoding_rejected
  | Compiled of artifact
let[@def] (origin @ total) (program : Tail.program @ immutable) =
  T.rebuild program.Tail.origin.Cfg.origin.Closure.origin.M.source.T.globals
    program.Tail.origin.Cfg.origin.Closure.origin.M.source.T.entry
let[@def] (correct @ total) (source : D.term @ immutable) (layout : Init.layout @ immutable)
    (input : W.t @ immutable) (memory : B.bytes @ immutable) (pages : B.u32) (artifact : artifact @ immutable) = ghost_ (
  artifact.bytes === artifact.binary.Binary.bytes
  && artifact.binary.Binary.prepared.Init.context.State.host_capacity === layout.host_capacity
  && origin artifact.program === source && Binary.accepted artifact.program layout input memory pages artifact.binary &&
  Binary.compilable artifact.program layout input memory pages)
let compile : (source : D.term) @ immutable -> (layout : Init.layout) @ immutable -> (input : W.t) @ immutable ->
    (memory : B.bytes) @ immutable -> (pages : B.u32) -> {u : unit | Init.valid_layout layout memory} ->
    {out : result | match out with Compiled artifact -> correct source layout input memory pages artifact | _ -> true} @ immutable =
  fun source layout input memory pages premise -> match S.compile source with
  | S.Unbound_variable -> Unbound_variable
  | S.Type_error -> Type_error
  | S.Entry_type_mismatch -> Entry_type_mismatch
  | S.Unsupported_fragment error -> Unsupported_fragment error
  | S.Compiled monomorphic ->
    let program = Tail.build (Cfg.build (Closure.build monomorphic)) in
    match Binary.compile program layout input memory pages () with
    | Binary.Layout_rejected -> Layout_rejected
    | Binary.Initialization_exhausted (heap, code) -> Initialization_exhausted (heap, code)
    | Binary.Encoding_rejected -> Encoding_rejected
    | Binary.Compiled binary ->
      let artifact = {bytes = binary.Binary.bytes; program = ghost_ program; binary = ghost_ binary} in
      ghost_ (Binary.accepted_def program layout input memory pages binary;
        Init.correct_def program layout input memory (Init.Initialized (binary.Binary.start, binary.Binary.prepared));
        Init.installed_def program layout input memory binary.Binary.start binary.Binary.prepared;
        origin_def program; correct_def source layout input memory pages artifact);
      Compiled artifact
let (safe @ total) : (source : D.term) @ immutable -> (layout : Init.layout) @ immutable -> (input : W.t) @ immutable ->
    (memory : B.bytes) @ immutable -> (pages : B.u32) -> (artifact : artifact) @ immutable -> (prefix : C.count) @ immutable ->
    {u : unit | correct source layout input memory pages artifact} ->
    {u : unit | match Execute.run prefix artifact.bytes
        (C.Succ layout.host_capacity) with
      Execute.Result (Calls.Running _) | Execute.Result (Calls.Finished _) -> true | _ -> false} @ ghost =
  fun source layout input memory pages artifact prefix premise -> ghost_ (
    correct_def source layout input memory pages artifact;
    Binary.accepted_def artifact.program layout input memory pages artifact.binary;
    Binary.safe artifact.program layout input memory artifact.binary.Binary.start artifact.binary.Binary.prepared pages artifact.binary.Binary.bytes prefix ())
let (reflection @ total) : (source : D.term) @ immutable -> (layout : Init.layout) @ immutable -> (input : W.t) @ immutable ->
    (memory : B.bytes) @ immutable -> (pages : B.u32) -> (artifact : artifact) @ immutable -> (prefix : C.count) @ immutable ->
    (after : Wasm_global_execution.state) @ immutable -> (registers : Registers.registers) @ immutable -> (word : W.t) @ immutable ->
    {u : unit | correct source layout input memory pages artifact &&
      Execute.run prefix artifact.bytes (C.Succ layout.host_capacity) === Execute.Result (Calls.Finished after) &&
      after.Wasm_global_execution.globals === Registers.globals registers && registers.Registers.status = 1 &&
      registers.Registers.tag === Hmc_tagged_cell.tag (Hmc_tagged_cell.Word word) && registers.Registers.payload === word &&
      after.Wasm_global_execution.execution.Wasm_memory_execution.machine.Wasm_execution.stack ===
        Wasm_scalar.Push (Wasm_scalar.I32 registers.Registers.status, Wasm_scalar.Empty)} ->
    {n : D.index | Hmc_source_semantics.advance n (Hmc_source_semantics.initial (D.Apply (source, D.Word input))) ===
      Hmc_source_semantics.Done (Hm_interpreter_typing.Word word)} @ immutable ghost =
  fun source layout input memory pages artifact prefix after registers word premise -> ghost_ (
    correct_def source layout input memory pages artifact;
      Binary.accepted_def artifact.program layout input memory pages artifact.binary;
      origin_def artifact.program;
      Hmc_monomorphic_simulation.source_start_def artifact.program.Tail.origin.Cfg.origin.Closure.origin input;
    Binary.reflection artifact.program layout input memory artifact.binary.Binary.start artifact.binary.Binary.prepared pages artifact.binary.Binary.bytes prefix after registers word ())
module E = Hmc_wasm_program_execution
module Source = Hmc_wasm_program_source_execution
let (preservation @ total) : (source : D.term) @ immutable -> (layout : Init.layout) @ immutable -> (input : W.t) @ immutable ->
    (memory : B.bytes) @ immutable -> (pages : B.u32) -> (artifact : artifact) @ immutable ->
    (word : W.t) @ immutable -> (source_fuel : D.index) @ immutable ->
    {u : unit | correct source layout input memory pages artifact &&
      Hmc_source_semantics.advance source_fuel (Hmc_source_semantics.initial (D.Apply (source, D.Word input))) ===
        Hmc_source_semantics.Done (Hm_interpreter_typing.Word word)} ->
    {out : E.execution | (Source.returned out.E.endpoint word || Source.exhausted out.E.endpoint) &&
      Execute.run (C.Succ out.E.fuel) artifact.bytes
        (C.Succ layout.host_capacity) ===
        Execute.Result (E.target artifact.binary.Binary.prepared.Init.context out.E.endpoint)} @ immutable ghost =
  fun source layout input memory pages artifact word source_fuel premise -> ghost_ (
    correct_def source layout input memory pages artifact;
      Binary.accepted_def artifact.program layout input memory pages artifact.binary;
      origin_def artifact.program;
      Hmc_monomorphic_simulation.source_start_def artifact.program.Tail.origin.Cfg.origin.Closure.origin input;
    Binary.preservation artifact.program layout input memory artifact.binary.Binary.start artifact.binary.Binary.prepared
      pages artifact.binary.Binary.bytes word source_fuel ())
let[@def] (binary_image @ total) (artifact : artifact @ immutable) (pages : B.u32) = ghost_ (
  Binary.image artifact.program artifact.binary.Binary.prepared.Init.lowered artifact.binary.Binary.prepared.Init.context
    artifact.binary.Binary.prepared.Init.state pages)
let (static_structure @ total) : (source : D.term) @ immutable -> (layout : Init.layout) @ immutable ->
    (input : W.t) @ immutable -> (memory : B.bytes) @ immutable -> (pages : B.u32) -> (artifact : artifact) @ immutable ->
    {u : unit | correct source layout input memory pages artifact} ->
    {u : unit | Wasm_static_module.structure_valid (binary_image artifact pages)} @ ghost =
  fun source layout input memory pages artifact premise -> ghost_ (
    correct_def source layout input memory pages artifact;
    Binary.accepted_def artifact.program layout input memory pages artifact.binary;
    let start = artifact.binary.Binary.start in let prepared = artifact.binary.Binary.prepared in
    Init.correct_def artifact.program layout input memory (Init.Initialized (start, prepared));
    Init.installed_def artifact.program layout input memory start prepared;
    Hmc_wasm_program_static.structure artifact.program start.Hmc_heap_initialize.globals prepared.Init.lowered
      prepared.Init.context prepared.Init.state pages ();
    binary_image_def artifact pages)
let (static_validity @ total) : (source : D.term) @ immutable -> (layout : Init.layout) @ immutable ->
    (input : W.t) @ immutable -> (memory : B.bytes) @ immutable -> (pages : B.u32) -> (artifact : artifact) @ immutable ->
    {u : unit | correct source layout input memory pages artifact} ->
    {u : unit | Wasm_static_module.bytes_valid artifact.bytes} @ ghost =
  fun source layout input memory pages artifact premise -> ghost_ (
    static_structure source layout input memory pages artifact ();
    correct_def source layout input memory pages artifact;
    Binary.accepted_def artifact.program layout input memory pages artifact.binary;
    let start = artifact.binary.Binary.start in let prepared = artifact.binary.Binary.prepared in
    Init.correct_def artifact.program layout input memory (Init.Initialized (start, prepared));
    Init.installed_def artifact.program layout input memory start prepared;
    State.valid_def artifact.program start.Hmc_heap_initialize.globals prepared.Init.lowered prepared.Init.context prepared.Init.state;
    Hmc_wasm_program_static.bodies artifact.program start.Hmc_heap_initialize.globals prepared.Init.lowered prepared.Init.context prepared.Init.state.State.registers ();
    Binary.image_def artifact.program prepared.Init.lowered prepared.Init.context prepared.Init.state pages;
    binary_image_def artifact pages;
    Wasm_static_module.valid_def (binary_image artifact pages);
    Wasm_static_module.bytes_valid_def artifact.binary.Binary.bytes)

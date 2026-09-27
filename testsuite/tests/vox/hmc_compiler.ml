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
type result = Unbound_variable
  | Type_error of Hmc_frontend.inference [@immediate_all_void_constructor]
  | Entry_type_mismatch of Hmc_frontend.inference [@immediate_all_void_constructor]
  | Unsupported_fragment of Hmc_admission.error | Layout_rejected
  | Initialization_exhausted of Hmc_heap_objects.heap * D.index | Encoding_rejected
  | Compiled of artifact
let[@def] (origin @ total) (program : Tail.program @ immutable) =
  T.rebuild program.Tail.origin.Cfg.origin.Closure.origin.M.source.T.globals
    program.Tail.origin.Cfg.origin.Closure.origin.M.source.T.entry
module Input = Hmc_wasm_program_input
module Dispatch = Hmc_wasm_program_dispatch
module Fuel = Wasm_control_compose
(* The artifact does not depend on an input: its module reads the input from
   the exported global [payload] when it runs. *)
let[@def] (correct @ total) (source : D.term @ immutable) (layout : Init.layout @ immutable)
    (memory : B.bytes @ immutable) (pages : B.u32) (artifact : artifact @ immutable) = ghost_ (
  artifact.bytes === artifact.binary.Binary.bytes
  && artifact.binary.Binary.prepared.Init.context.State.host_capacity === layout.host_capacity
  && origin artifact.program === source && Binary.accepted artifact.program layout memory pages artifact.binary &&
  Binary.compilable artifact.program layout memory pages)
let compile : (source : D.term) @ immutable -> (layout : Init.layout) @ immutable ->
    (memory : B.bytes) @ immutable -> (pages : B.u32) -> {u : unit | Init.valid_layout layout memory} ->
    {out : result | match out with
      | Compiled artifact -> correct source layout memory pages artifact
      | Unbound_variable -> not (D.scoped_term D.Z source)
      | Type_error run -> Hmc_frontend.untyped source run
      | Entry_type_mismatch run -> Hmc_frontend.mistyped source run
      | Unsupported_fragment error -> Hmc_admission.meaning source error
      | Layout_rejected | Initialization_exhausted _ | Encoding_rejected -> true} @ immutable =
  fun source layout memory pages premise -> match S.compile source with
  | S.Unbound_variable -> Unbound_variable
  | S.Type_error run -> Type_error run
  | S.Entry_type_mismatch run -> Entry_type_mismatch run
  | S.Unsupported_fragment error -> Unsupported_fragment error
  | S.Compiled monomorphic ->
    let program = Tail.build (Cfg.build (Closure.build monomorphic)) in
    match Binary.compile program layout memory pages () with
    | Binary.Layout_rejected -> Layout_rejected
    | Binary.Initialization_exhausted (heap, code) -> Initialization_exhausted (heap, code)
    | Binary.Encoding_rejected -> Encoding_rejected
    | Binary.Compiled binary ->
      let artifact = {bytes = binary.Binary.bytes; program = ghost_ program; binary = ghost_ binary} in
      ghost_ (Binary.accepted_def program layout memory pages binary;
        Binary.built_def program layout memory binary.Binary.start binary.Binary.prepared pages binary.Binary.bytes;
        Init.correct_def program layout (Input.placeholder ()) memory (Init.Initialized (binary.Binary.start, binary.Binary.prepared));
        Init.installed_def program layout (Input.placeholder ()) memory binary.Binary.start binary.Binary.prepared;
        origin_def program; correct_def source layout memory pages artifact);
      Compiled artifact
(* The state the initializer builds for [input], which the module reaches
   after its prologue. *)
let (target @ total) : (source : D.term) @ immutable -> (layout : Init.layout) @ immutable ->
    (memory : B.bytes) @ immutable -> (pages : B.u32) -> (artifact : artifact) @ immutable -> (input : W.t) @ immutable ->
    {u : unit | correct source layout memory pages artifact} ->
    {target : Init.prepared | Binary.targets artifact.program layout memory artifact.binary.Binary.start
        artifact.binary.Binary.prepared input target
      && Binary.built artifact.program layout memory artifact.binary.Binary.start artifact.binary.Binary.prepared pages artifact.bytes
      && target.Init.context.State.host_capacity === layout.host_capacity} @ immutable ghost =
  fun source layout memory pages artifact input premise -> ghost_ (
    correct_def source layout memory pages artifact;
    Binary.accepted_def artifact.program layout memory pages artifact.binary;
    Binary.built_def artifact.program layout memory artifact.binary.Binary.start artifact.binary.Binary.prepared pages artifact.binary.Binary.bytes;
    let target = Binary.retarget artifact.program layout memory artifact.binary.Binary.start artifact.binary.Binary.prepared input () in
    Binary.targets_def artifact.program layout memory artifact.binary.Binary.start artifact.binary.Binary.prepared input target;
    Input.retargeted_def artifact.binary.Binary.prepared input target;
    target)
let (safe @ total) : (source : D.term) @ immutable -> (layout : Init.layout) @ immutable ->
    (memory : B.bytes) @ immutable -> (pages : B.u32) -> (artifact : artifact) @ immutable ->
    (input : W.t) @ immutable -> (prefix : C.count) @ immutable ->
    {u : unit | correct source layout memory pages artifact} ->
    {u : unit | match Execute.run prefix artifact.bytes input (C.Succ layout.host_capacity) with
      Execute.Result (Calls.Running _) | Execute.Result (Calls.Finished _) -> true | _ -> false} @ ghost =
  fun source layout memory pages artifact input prefix premise -> ghost_ (
    let target = target source layout memory pages artifact input () in
    Binary.safe artifact.program layout memory artifact.binary.Binary.start artifact.binary.Binary.prepared pages
      artifact.bytes input target prefix ())
let (reflection @ total) : (source : D.term) @ immutable -> (layout : Init.layout) @ immutable ->
    (memory : B.bytes) @ immutable -> (pages : B.u32) -> (artifact : artifact) @ immutable -> (input : W.t) @ immutable ->
    (prefix : C.count) @ immutable ->
    (after : Wasm_global_execution.state) @ immutable -> (registers : Registers.registers) @ immutable -> (word : W.t) @ immutable ->
    {u : unit | correct source layout memory pages artifact &&
      Execute.run prefix artifact.bytes input (C.Succ layout.host_capacity) === Execute.Result (Calls.Finished after) &&
      after.Wasm_global_execution.globals === Registers.globals registers && registers.Registers.status = 1 &&
      registers.Registers.tag === Hmc_tagged_cell.tag (Hmc_tagged_cell.Word word) && registers.Registers.payload === word &&
      after.Wasm_global_execution.execution.Wasm_memory_execution.machine.Wasm_execution.stack ===
        Wasm_scalar.Push (Wasm_scalar.I32 registers.Registers.status, Wasm_scalar.Empty)} ->
    {n : D.index | Hmc_source_semantics.advance n (Hmc_source_semantics.initial (D.Apply (source, D.Word input))) ===
      Hmc_source_semantics.Done (Hm_interpreter_typing.Word word)} @ immutable ghost =
  fun source layout memory pages artifact input prefix after registers word premise -> ghost_ (
    let target = target source layout memory pages artifact input () in
    correct_def source layout memory pages artifact;
    origin_def artifact.program;
    Hmc_monomorphic_simulation.source_start_def artifact.program.Tail.origin.Cfg.origin.Closure.origin input;
    Binary.reflection artifact.program layout memory artifact.binary.Binary.start artifact.binary.Binary.prepared pages
      artifact.bytes input target prefix after registers word ())
module E = Hmc_wasm_program_execution
module Source = Hmc_wasm_program_source_execution
let (preservation @ total) : (source : D.term) @ immutable -> (layout : Init.layout) @ immutable ->
    (memory : B.bytes) @ immutable -> (pages : B.u32) -> (artifact : artifact) @ immutable ->
    (input : W.t) @ immutable -> (target : Init.prepared) @ immutable -> (word : W.t) @ immutable -> (source_fuel : D.index) @ immutable ->
    {u : unit | correct source layout memory pages artifact &&
      Binary.targets artifact.program layout memory artifact.binary.Binary.start artifact.binary.Binary.prepared input target &&
      Hmc_source_semantics.advance source_fuel (Hmc_source_semantics.initial (D.Apply (source, D.Word input))) ===
        Hmc_source_semantics.Done (Hm_interpreter_typing.Word word)} ->
    {out : E.execution | (Source.returned out.E.endpoint word || Source.exhausted out.E.endpoint) &&
      Execute.run (Fuel.add (Dispatch.five ()) (C.Succ out.E.fuel)) artifact.bytes input
        (C.Succ layout.host_capacity) ===
        Execute.Result (E.target target.Init.context out.E.endpoint)} @ immutable ghost =
  fun source layout memory pages artifact input target word source_fuel premise -> ghost_ (
    correct_def source layout memory pages artifact;
    Binary.accepted_def artifact.program layout memory pages artifact.binary;
    Binary.targets_def artifact.program layout memory artifact.binary.Binary.start artifact.binary.Binary.prepared input target;
    Input.retargeted_def artifact.binary.Binary.prepared input target;
    origin_def artifact.program;
    Hmc_monomorphic_simulation.source_start_def artifact.program.Tail.origin.Cfg.origin.Closure.origin input;
    Binary.preservation artifact.program layout memory artifact.binary.Binary.start artifact.binary.Binary.prepared
      pages artifact.bytes input target word source_fuel ())
let[@def] (binary_image @ total) (artifact : artifact @ immutable) (pages : B.u32) = ghost_ (
  Binary.image artifact.program artifact.binary.Binary.prepared.Init.lowered artifact.binary.Binary.prepared.Init.context
    artifact.binary.Binary.prepared.Init.state pages)
let (static_structure @ total) : (source : D.term) @ immutable -> (layout : Init.layout) @ immutable ->
    (memory : B.bytes) @ immutable -> (pages : B.u32) -> (artifact : artifact) @ immutable ->
    {u : unit | correct source layout memory pages artifact} ->
    {u : unit | Wasm_static_module.structure_valid (binary_image artifact pages)} @ ghost =
  fun source layout memory pages artifact premise -> ghost_ (
    let input = Input.placeholder () in
    correct_def source layout memory pages artifact;
    Binary.accepted_def artifact.program layout memory pages artifact.binary;
    Binary.built_def artifact.program layout memory artifact.binary.Binary.start artifact.binary.Binary.prepared pages artifact.binary.Binary.bytes;
    let start = artifact.binary.Binary.start in let prepared = artifact.binary.Binary.prepared in
    Init.correct_def artifact.program layout input memory (Init.Initialized (start, prepared));
    Init.installed_def artifact.program layout input memory start prepared;
    Hmc_wasm_program_static.structure artifact.program start.Hmc_heap_initialize.globals prepared.Init.lowered
      prepared.Init.context prepared.Init.state pages ();
    binary_image_def artifact pages)
let (static_validity @ total) : (source : D.term) @ immutable -> (layout : Init.layout) @ immutable ->
    (memory : B.bytes) @ immutable -> (pages : B.u32) -> (artifact : artifact) @ immutable ->
    {u : unit | correct source layout memory pages artifact} ->
    {u : unit | Wasm_static_module.bytes_valid artifact.bytes} @ ghost =
  fun source layout memory pages artifact premise -> ghost_ (
    let input = Input.placeholder () in
    static_structure source layout memory pages artifact ();
    correct_def source layout memory pages artifact;
    Binary.accepted_def artifact.program layout memory pages artifact.binary;
    Binary.built_def artifact.program layout memory artifact.binary.Binary.start artifact.binary.Binary.prepared pages artifact.binary.Binary.bytes;
    let start = artifact.binary.Binary.start in let prepared = artifact.binary.Binary.prepared in
    Init.correct_def artifact.program layout input memory (Init.Initialized (start, prepared));
    Init.installed_def artifact.program layout input memory start prepared;
    State.valid_def artifact.program start.Hmc_heap_initialize.globals prepared.Init.lowered prepared.Init.context prepared.Init.state;
    Hmc_wasm_program_static.bodies artifact.program start.Hmc_heap_initialize.globals prepared.Init.lowered prepared.Init.context prepared.Init.state.State.registers ();
    Binary.image_def artifact.program prepared.Init.lowered prepared.Init.context prepared.Init.state pages;
    binary_image_def artifact pages;
    Wasm_static_module.valid_def (binary_image artifact pages);
    Wasm_static_module.bytes_valid_def artifact.binary.Binary.bytes)

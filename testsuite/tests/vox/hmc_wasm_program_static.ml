module B = Wasm_u32
module I = Hmc_tail_ir
module State = Hmc_wasm_program_state
module Lower = Hmc_wasm_program_lower
module Registers = Hmc_wasm_program_registers
module Assembly = Hmc_wasm_program_functions
module Runtime = Hmc_wasm_program_runtime
module Binary = Hmc_wasm_program_binary
module Static = Wasm_static_module
module G = Wasm_globals
module L = Wasm_locals
module S = Wasm_scalar
let[@def] (tag_index @ total) (unit : unit) : B.u32 = 6
let[@def] (payload_index @ total) (unit : unit) : B.u32 = 7
let (register_exports @ total) : (registers : Registers.registers) @ immutable ->
    {u : unit | G.get (Registers.globals registers) (tag_index ()) === Some (S.I64 registers.Registers.tag)
      && G.get (Registers.globals registers) (payload_index ()) === Some (S.I64 registers.Registers.payload)} @ ghost =
  fun registers -> ghost_ (
    tag_index_def (); payload_index_def ();
    Registers.globals_def registers; Registers.values_def registers;
    let g7 = S.Push (S.I64 registers.Registers.payload, S.Empty) in
    let g6 = S.Push (S.I64 registers.Registers.tag, g7) in
    let g5 = S.Push (S.I32 registers.Registers.status, g6) in
    let g4 = S.Push (S.I32 registers.Registers.stack_limit, g5) in
    let g3 = S.Push (S.I32 registers.Registers.top, g4) in
    let g2 = S.Push (S.I32 registers.Registers.heap_limit, g3) in
    let g1 = S.Push (S.I32 registers.Registers.heap, g2) in
    let g0 = S.Push (S.I32 registers.Registers.frame, g1) in
    G.get_def (Registers.globals registers) 6; G.get_def (Registers.globals registers) 7;
    L.get_def g0 6; L.get_def g1 5; L.get_def g2 4; L.get_def g3 3;
    L.get_def g4 2; L.get_def g5 1; L.get_def g6 0;
    L.get_def g0 7; L.get_def g1 6; L.get_def g2 5; L.get_def g3 4;
    L.get_def g4 3; L.get_def g5 2; L.get_def g6 1; L.get_def g7 0)
let (exports @ total) : (program : I.program) @ immutable -> (globals : Hmc_heap_machine.globals) @ immutable ->
    (lowered : Lower.program) @ immutable -> (context : State.context) @ immutable -> (state : State.running) @ immutable -> (pages : B.u32) ->
    {u : unit | State.valid program globals lowered context state} ->
    {u : unit | Static.exports_valid (Binary.image program lowered context state pages)} @ ghost =
  fun program globals lowered context state pages premise -> ghost_ (
    State.valid_def program globals lowered context state;
    Lower.corresponds_def program globals context.State.max_pc lowered;
    Assembly.source_order globals program.I.origin.Hmc_cfg_program.blocks program.I.code lowered.Lower.blocks
      lowered.Lower.capacity context.State.max_pc context.State.block_count ();
    Assembly.main_correct lowered (Hmc_cfg_ir.size program.I.origin.Hmc_cfg_program.blocks) context.State.block_count
      (Runtime.config context.State.table_base context.State.stack_base) (Runtime.dispatcher ()) ();
    State.module__def program lowered context;
    register_exports state.State.registers; tag_index_def (); payload_index_def ();
    Binary.image_def program lowered context state pages;
    Static.exports_valid_def (Binary.image program lowered context state pages))
let (structure @ total) : (program : I.program) @ immutable -> (globals : Hmc_heap_machine.globals) @ immutable ->
    (lowered : Lower.program) @ immutable -> (context : State.context) @ immutable -> (state : State.running) @ immutable -> (pages : B.u32) ->
    {u : unit | State.valid program globals lowered context state && Binary.emittable program lowered context state pages} ->
    {u : unit | Static.structure_valid (Binary.image program lowered context state pages)} @ ghost =
  fun program globals lowered context state pages premise -> ghost_ (
    exports program globals lowered context state pages ();
    Binary.emittable_def program lowered context state pages;
    Static.structure_from_encoding (Binary.image program lowered context state pages) ())
let (valid_if_bodies @ total) : (program : I.program) @ immutable -> (globals : Hmc_heap_machine.globals) @ immutable ->
    (lowered : Lower.program) @ immutable -> (context : State.context) @ immutable -> (state : State.running) @ immutable ->
    (pages : B.u32) -> (bytes : B.bytes) @ immutable ->
    {u : unit | State.valid program globals lowered context state && Binary.emittable program lowered context state pages
      && Wasm_binary_module.decode bytes === Some (Binary.image program lowered context state pages, B.End)
      && Wasm_static_control.function_bodies (State.module_ program lowered context) (Registers.globals state.State.registers)} ->
    {u : unit | Static.bytes_valid bytes} @ ghost =
  fun program globals lowered context state pages bytes premise -> ghost_ (
    structure program globals lowered context state pages ();
    Binary.image_def program lowered context state pages;
    Static.valid_def (Binary.image program lowered context state pages); Static.bytes_valid_def bytes)
let (dispatcher_typed @ total) : (program : I.program) @ immutable -> (lowered : Lower.program) @ immutable ->
    (context : State.context) @ immutable -> (registers : Registers.registers) @ immutable ->
    {u : unit | Wasm_static_control.function_ (State.module_ program lowered context) (Registers.globals registers) (Runtime.dispatcher ())} @ ghost =
  fun program lowered context registers -> ghost_ (
    let module_ = State.module_ program lowered context in
    State.module__def program lowered context;
    Assembly.assemble_def lowered (Hmc_cfg_ir.size program.I.origin.Hmc_cfg_program.blocks) context.State.block_count
      (Runtime.config context.State.table_base context.State.stack_base) (Runtime.dispatcher ());
    Wasm_functions.signature_def module_.Wasm_functions.signatures 0;
    Registers.globals_def registers; Registers.values_def registers;
    let g7 = S.Push (S.I64 registers.Registers.payload, S.Empty) in
    let g6 = S.Push (S.I64 registers.Registers.tag, g7) in
    let g5 = S.Push (S.I32 registers.Registers.status, g6) in
    let g4 = S.Push (S.I32 registers.Registers.stack_limit, g5) in
    let g3 = S.Push (S.I32 registers.Registers.top, g4) in
    let g2 = S.Push (S.I32 registers.Registers.heap_limit, g3) in
    let g1 = S.Push (S.I32 registers.Registers.heap, g2) in
    let g0 = S.Push (S.I32 registers.Registers.frame, g1) in
    G.get_def (Registers.globals registers) 0; L.get_def g0 0;
    G.get_def (Registers.globals registers) 5;
    L.get_def g0 5; L.get_def g1 4; L.get_def g2 3; L.get_def g3 2; L.get_def g4 1; L.get_def g5 0;
    Wasm_static_types.global_def (Registers.globals registers) 0;
    Wasm_static_types.global_def (Registers.globals registers) 5;
    Hmc_wasm_static_runtime.zero_def (); Hmc_wasm_static_runtime.status_index_def ();
    Hmc_wasm_static_runtime.dispatcher module_ (Registers.globals registers) ())
let (bodies @ total) : (program : I.program) @ immutable -> (globals : Hmc_heap_machine.globals) @ immutable ->
    (lowered : Lower.program) @ immutable -> (context : State.context) @ immutable -> (registers : Registers.registers) @ immutable ->
    {u : unit | Lower.corresponds program globals context.State.max_pc lowered} ->
    {u : unit | Wasm_static_control.function_bodies (State.module_ program lowered context) (Registers.globals registers)} @ ghost =
  fun program globals lowered context registers premise -> ghost_ (
    let module_ = State.module_ program lowered context in
    let dispatcher = Runtime.dispatcher () in
    let tail = Wasm_functions.Function (dispatcher, Wasm_functions.No_functions) in
    dispatcher_typed program lowered context registers;
    Wasm_static_control.functions_def module_ (Registers.globals registers) Wasm_functions.No_functions;
    Wasm_static_control.functions_def module_ (Registers.globals registers) tail;
    Lower.corresponds_def program globals context.State.max_pc lowered;
    Hmc_wasm_static_functions.functions module_ registers lowered globals program.I.origin.Hmc_cfg_program.blocks program.I.code
      lowered.Lower.blocks lowered.Lower.capacity context.State.max_pc context.State.table_base context.State.stack_base tail ();
    State.module__def program lowered context;
    Assembly.assemble_def lowered (Hmc_cfg_ir.size program.I.origin.Hmc_cfg_program.blocks) context.State.block_count
      (Runtime.config context.State.table_base context.State.stack_base) dispatcher;
    Wasm_static_control.function_bodies_def module_ (Registers.globals registers))
let (valid @ total) : (program : I.program) @ immutable -> (globals : Hmc_heap_machine.globals) @ immutable ->
    (lowered : Lower.program) @ immutable -> (context : State.context) @ immutable -> (state : State.running) @ immutable ->
    (pages : B.u32) -> (bytes : B.bytes) @ immutable ->
    {u : unit | State.valid program globals lowered context state && Binary.emittable program lowered context state pages
      && Wasm_binary_module.decode bytes === Some (Binary.image program lowered context state pages, B.End)} ->
    {u : unit | Static.bytes_valid bytes} @ ghost =
  fun program globals lowered context state pages bytes premise -> ghost_ (
    State.valid_def program globals lowered context state;
    bodies program globals lowered context state.State.registers ();
    valid_if_bodies program globals lowered context state pages bytes ())

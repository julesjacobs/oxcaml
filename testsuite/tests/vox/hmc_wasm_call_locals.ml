module B = Wasm_u32
module I = Wasm_instruction
module C = Wasm_code
module T = Wasm_control
module LP = Wasm_local_preservation
module P = Wasm_control_local_preservation
module Lift = Wasm_control_lift
module Table = Hmc_wasm_dispatch_code
module Select = Wasm_local_select
module Probe = Wasm_local_probe
let rec (embed @ total) : (body : C.t) @ immutable -> (tail : T.code) @ immutable -> (local : B.u32) ->
    {u : unit | LP.preserves body local && P.code tail local} ->
    {u : unit | P.code (Lift.embed body tail) local} @ ghost = fun body tail local premise -> ghost_ (
    Lift.embed_def body tail; LP.preserves_def body local;
    match body with
    | C.Empty -> ()
    | C.Next (instruction, rest) -> embed rest tail local (); P.code_def (Lift.embed body tail) local)
let[@def] rec (table @ total) (entries : Table.table @ immutable) (local : B.u32) = match entries with
  | Table.Empty -> true
  | Table.Add (_, body, rest) -> P.code body local && table rest local
let (probe @ total) : (label : B.u32) -> (source : B.u32) -> (local : B.u32) ->
    {u : unit | LP.preserves (Probe.emit label source) local} @ ghost = fun label source local -> ghost_ (
    Probe.emit_def label source; LP.preserves_def C.Empty local;
    LP.instruction_preserves_def (I.Plain I.I32_eq) local;
    LP.preserves_def (C.Next (I.Plain I.I32_eq, C.Empty)) local;
    LP.instruction_preserves_def (I.I32_const label) local;
    LP.preserves_def (C.Next (I.I32_const label, C.Next (I.Plain I.I32_eq, C.Empty))) local;
    LP.instruction_preserves_def (I.Local_get source) local;
    LP.preserves_def (Probe.emit label source) local)
let rec (select @ total) : (entries : Table.table) @ immutable -> (source : B.u32) -> (local : B.u32) ->
    {u : unit | table entries local} -> {u : unit | P.code (Select.emit entries source) local} @ ghost =
  fun entries source local premise -> ghost_ (
    table_def entries local; Select.emit_def entries source; P.code_def T.Empty local;
    match entries with
    | Table.Empty -> Select.trap_def (); LP.instruction_preserves_def (I.Plain I.Unreachable) local;
      P.code_def (Select.trap ()) local
    | Table.Add (label, body, rest) ->
      select rest source local (); probe label source local;
      let tail = T.If (body, Select.emit rest source, T.Empty) in
      P.code_def tail local; embed (Probe.emit label source) tail local ())
module E = Wasm_execution
module Store = Wasm_pointer_store
module Mixed = Wasm_mixed_write
module Copies = Wasm_cross_copy
module Plan = Wasm_parallel_copy
module Common = Hmc_wasm_cons_locals
let (pointer_store @ total) : (offset : B.u32) -> (base : B.u32) -> (source : B.u32) -> (local : B.u32) ->
    {u : unit | LP.preserves (Store.emit offset base source) local} @ ghost = fun offset base source local -> ghost_ (
    Store.emit_def offset base source; LP.preserves_def C.Empty local;
    LP.instruction_preserves_def (I.I64_store (3, offset)) local;
    LP.preserves_def (C.Next (I.I64_store (3, offset), C.Empty)) local;
    LP.instruction_preserves_def (I.Plain I.I64_extend_i32_u) local;
    LP.preserves_def (C.Next (I.Plain I.I64_extend_i32_u, C.Next (I.I64_store (3, offset), C.Empty))) local;
    LP.instruction_preserves_def (I.Local_get source) local;
    LP.preserves_def (C.Next (I.Local_get source, C.Next (I.Plain I.I64_extend_i32_u, C.Next (I.I64_store (3, offset), C.Empty)))) local;
    LP.instruction_preserves_def (I.Local_get base) local; LP.preserves_def (Store.emit offset base source) local)
let rec (mixed @ total) : (writes : Mixed.writes) @ immutable -> (base : B.u32) -> (local : B.u32) ->
    {u : unit | LP.preserves (Mixed.emit writes base) local} @ ghost = fun writes base local -> ghost_ (
    Mixed.emit_def writes base;
    match writes with
    | Mixed.End -> LP.preserves_def C.Empty local
    | Mixed.Write (offset, value, rest) ->
      Mixed.one_def offset value base;
      (match value with
      | Mixed.Constant word -> Common.immediate offset base word local ()
      | Mixed.Word_local source -> Common.writes (Wasm_frame_write.Write (offset, source, Wasm_frame_write.End)) base local
      | Mixed.Pointer_local source -> pointer_store offset base source local);
      mixed rest base local; LP.append (Mixed.one offset value base) (Mixed.emit rest base) local ())
let rec (copies @ total) : (plan : Plan.plan) @ immutable -> (source : B.u32) -> (target : B.u32) -> (local : B.u32) ->
    {u : unit | LP.preserves (Copies.emit plan source target) local} @ ghost = fun plan source target local -> ghost_ (
    Copies.emit_def plan source target;
    match plan with
    | Plan.End -> LP.preserves_def C.Empty local
    | Plan.Copy (offset, destination, rest) ->
      copies rest source target local;
      let store = C.Next (I.I64_store (3, destination), C.Empty) in
      LP.preserves_def C.Empty local; LP.instruction_preserves_def (I.I64_store (3, destination)) local;
      LP.preserves_def store local; LP.append (Copies.emit rest source target) store local ();
      let tail = E.append (Copies.emit rest source target) store in
      LP.instruction_preserves_def (I.I64_load (3, offset)) local;
      LP.preserves_def (C.Next (I.I64_load (3, offset), tail)) local;
      LP.instruction_preserves_def (I.Local_get source) local;
      LP.preserves_def (C.Next (I.Local_get source, C.Next (I.I64_load (3, offset), tail))) local;
      LP.instruction_preserves_def (I.Local_get target) local;
      LP.preserves_def (Copies.emit plan source target) local)
module Captures = Hmc_wasm_call_captures
module Header = Hmc_wasm_dynamic_call_header
module Frame = Hmc_wasm_dynamic_call_frame
module Plans = Hmc_wasm_call_plan_table
let (frame @ total) : (fragment : Captures.fragment) @ immutable -> (pc : B.u32) -> (object_ : B.u32) -> (base : B.u32) ->
    (tag : B.u32) -> (payload : B.u32) -> (local : B.u32) ->
    {u : unit | LP.preserves (Frame.emit fragment pc object_ base tag payload) local} @ ghost =
  fun fragment pc object_ base tag payload local -> ghost_ (
    Frame.emit_def fragment pc object_ base tag payload; Captures.emit_def fragment object_ base;
    copies fragment.Captures.copies object_ base local;
    Header.emit_def fragment.Captures.recursive pc base object_ tag payload;
    mixed (Header.writes fragment.Captures.recursive pc object_ tag payload) base local;
    LP.append (Captures.emit fragment object_ base) (Header.emit fragment.Captures.recursive pc base object_ tag payload) local ())
let rec (prepare @ total) : (plans : Plans.table) @ immutable -> (pc : B.u32) -> (object_ : B.u32) -> (base : B.u32) ->
    (tag : B.u32) -> (payload : B.u32) -> (local : B.u32) ->
    {u : unit | table (Plans.prepare plans pc object_ base tag payload) local} @ ghost =
  fun plans pc object_ base tag payload local -> ghost_ (
    Plans.prepare_def plans pc object_ base tag payload;
    match plans with
    | Plans.Empty -> table_def Table.Empty local
    | Plans.Add (number, fragment, rest) ->
      prepare rest pc object_ base tag payload local; frame fragment pc object_ base tag payload local;
      P.code_def T.Empty local; embed (Frame.emit fragment pc object_ base tag payload) T.Empty local ();
      table_def (Plans.prepare plans pc object_ base tag payload) local)
module Double = Wasm_local_double
module Address = Hmc_wasm_descriptor_address
module Advance = Hmc_wasm_allocation_advance
module Limb = Wasm_limb_read
module Slots = Hmc_wasm_descriptor_load
module Reads = Hmc_wasm_descriptor_reads
module Descriptor = Hmc_wasm_descriptor_select
let (double @ total) : (source : B.u32) -> (destination : B.u32) -> (local : B.u32) ->
    {u : unit | destination <> local} -> {u : unit | LP.preserves (Double.emit source destination) local} @ ghost =
  fun source destination local premise -> ghost_ (
    Double.emit_def source destination; LP.preserves_def C.Empty local;
    LP.instruction_preserves_def (I.Local_set destination) local;
    LP.preserves_def (C.Next (I.Local_set destination, C.Empty)) local;
    LP.instruction_preserves_def (I.Plain I.I32_add) local;
    LP.preserves_def (C.Next (I.Plain I.I32_add, C.Next (I.Local_set destination, C.Empty))) local;
    LP.instruction_preserves_def (I.Local_get source) local;
    LP.preserves_def (C.Next (I.Local_get source, C.Next (I.Plain I.I32_add, C.Next (I.Local_set destination, C.Empty)))) local;
    LP.preserves_def (Double.emit source destination) local)
let (address @ total) : (base : B.u32) -> (source : B.u32) -> (destination : B.u32) -> (local : B.u32) ->
    {u : unit | destination <> local} -> {u : unit | LP.preserves (Address.emit base source destination) local} @ ghost =
  fun base source destination local premise -> ghost_ (
    Address.emit_def base source destination; double source destination local (); double destination destination local ();
    Common.advance base destination local ();
    let d = Double.emit destination destination in
    let a = Advance.emit base destination in
    LP.append d a local (); LP.append d (E.append d a) local ();
    LP.append d (E.append d (E.append d a)) local ();
    LP.append d (E.append d (E.append d (E.append d a))) local ();
    LP.append (Double.emit source destination) (E.append d (E.append d (E.append d (E.append d a)))) local ())
let (limb @ total) : (offset : B.u32) -> (base : B.u32) -> (destination : B.u32) -> (local : B.u32) ->
    {u : unit | destination <> local} -> {u : unit | LP.preserves (Limb.emit offset base destination) local} @ ghost =
  fun offset base destination local premise -> ghost_ (
    Limb.emit_def offset base destination; LP.preserves_def C.Empty local;
    LP.instruction_preserves_def (I.Local_set destination) local;
    LP.preserves_def (C.Next (I.Local_set destination, C.Empty)) local;
    LP.instruction_preserves_def (I.I32_load (2, offset)) local;
    LP.preserves_def (C.Next (I.I32_load (2, offset), C.Next (I.Local_set destination, C.Empty))) local;
    LP.instruction_preserves_def (I.Local_get base) local; LP.preserves_def (Limb.emit offset base destination) local)
let (descriptor @ total) : (base : B.u32) -> (source : B.u32) -> (destination : B.u32) -> (slots : Slots.slots) @ immutable -> (local : B.u32) ->
    {u : unit | destination <> local && slots.Slots.start <> local && slots.Slots.captures <> local && slots.Slots.recursive <> local} ->
    {u : unit | LP.preserves (Descriptor.emit base source destination slots) local} @ ghost =
  fun base source destination slots local premise -> ghost_ (
    Descriptor.emit_def base source destination slots; Slots.emit_def destination slots;
    address base source destination local ();
    limb (Reads.start_offset ()) destination slots.Slots.start local ();
    limb (Reads.captures_offset ()) destination slots.Slots.captures local ();
    limb (Reads.recursive_offset ()) destination slots.Slots.recursive local ();
    LP.append (Limb.emit (Reads.captures_offset ()) destination slots.Slots.captures)
      (Limb.emit (Reads.recursive_offset ()) destination slots.Slots.recursive) local ();
    LP.append (Limb.emit (Reads.start_offset ()) destination slots.Slots.start)
      (E.append (Limb.emit (Reads.captures_offset ()) destination slots.Slots.captures) (Limb.emit (Reads.recursive_offset ()) destination slots.Slots.recursive)) local ();
    LP.append (Address.emit base source destination) (Slots.emit destination slots) local ())
module Capture = Hmc_wasm_cons_capture
module Position = Hmc_wasm_simple_lower
module Target = Hmc_wasm_call_target
module Target_preserve = Hmc_wasm_call_target_preserve
module Operands = Hmc_wasm_call_operands
module Closure = Hmc_wasm_closure_code
module Pointer = Wasm_pointer_local
module Pointer_read = Wasm_pointer_read
let (target @ total) : (env_count : Position.slot) -> (slots : Capture.slots) @ immutable -> (base : B.u32) -> (object_ : B.u32) -> (code : B.u32) -> (local : B.u32) ->
    {u : unit | Capture.separate slots local && object_ <> local && code <> local} ->
    {u : unit | LP.preserves (Target.emit env_count slots base object_ code) local} @ ghost =
  fun env_count slots base object_ code local premise -> ghost_ (
    Capture.layout (Position.slot_tag env_count) (Position.slot_payload env_count) slots local ();
    Target_preserve.snapshot (Capture.reads (Position.slot_tag env_count) (Position.slot_payload env_count) slots) base local ();
    Capture.emit_def (Position.slot_tag env_count) (Position.slot_payload env_count) slots base;
    Target_preserve.pointer slots.Capture.head_payload object_ local ();
    LP.append (Capture.emit (Position.slot_tag env_count) (Position.slot_payload env_count) slots base) (Pointer.emit slots.Capture.head_payload object_) local ();
    Operands.emit_def env_count slots base object_;
    Target_preserve.pointer_read (Hmc_wasm_closure_read.offset ()) object_ code local ();
    Closure.emit_def object_ code;
    LP.append (Operands.emit env_count slots base object_) (Closure.emit object_ code) local ();
    Target.emit_def env_count slots base object_ code)
module Dispatch = Hmc_wasm_call_dispatch
module Plan_select = Hmc_wasm_call_plan_select
module Loaded = Hmc_wasm_loaded_call
let (loaded @ total) : (env_count : Position.slot) -> (capture : Capture.slots) @ immutable -> (plans : Plans.table) @ immutable ->
    (table_base : B.u32) -> (code : B.u32) -> (address_local : B.u32) -> (slots : Slots.slots) @ immutable -> (object_ : B.u32) -> (base : B.u32) -> (local : B.u32) ->
    {u : unit | Capture.separate capture local && object_ <> local && code <> local && address_local <> local
      && slots.Slots.start <> local && slots.Slots.captures <> local && slots.Slots.recursive <> local} ->
    {u : unit | P.code (Loaded.emit env_count capture plans table_base code address_local slots object_ base) local} @ ghost =
  fun env_count capture plans table_base code address_local slots object_ base local premise -> ghost_ (
    prepare plans slots.Slots.start object_ base capture.Capture.tail_tag capture.Capture.tail_payload local;
    select (Plans.prepare plans slots.Slots.start object_ base capture.Capture.tail_tag capture.Capture.tail_payload) code local ();
    Plan_select.emit_def plans code slots.Slots.start object_ base capture.Capture.tail_tag capture.Capture.tail_payload;
    descriptor table_base code address_local slots local ();
    embed (Descriptor.emit table_base code address_local slots)
      (Plan_select.emit plans code slots.Slots.start object_ base capture.Capture.tail_tag capture.Capture.tail_payload) local ();
    Dispatch.emit_def plans table_base code address_local slots object_ base capture.Capture.tail_tag capture.Capture.tail_payload;
    target env_count capture base object_ code local ();
    embed (Target.emit env_count capture base object_ code)
      (Dispatch.emit plans table_base code address_local slots object_ base capture.Capture.tail_tag capture.Capture.tail_payload) local ();
    Loaded.emit_def env_count capture plans table_base code address_local slots object_ base)
module Emit = Hmc_wasm_program_emit
module Registers = Wasm_global_registers
module Round = Hmc_wasm_program_roundtrip
module Assembly = Hmc_wasm_program_functions
module Runtime = Hmc_wasm_program_runtime
module Lower = Hmc_wasm_program_lower
module Block = Hmc_wasm_program_block
module Structured = Hmc_wasm_structured_block
let (status @ total) : (destination : B.u32) -> (value : B.u32) -> (tail : T.code) @ immutable -> (local : B.u32) ->
    {u : unit | destination <> local && P.code tail local} -> {u : unit | P.code (Emit.status destination value tail) local} @ ghost =
  fun destination value tail local premise -> ghost_ (
    Emit.status_def destination value tail; LP.instruction_preserves_def (I.Local_set destination) local;
    P.code_def (T.Instruction (I.Local_set destination, tail)) local;
    LP.instruction_preserves_def (I.I32_const value) local; P.code_def (Emit.status destination value tail) local)
let (protected @ total) : (destination : B.u32) -> (value : B.u32) -> (body : T.code) @ immutable -> (local : B.u32) ->
    {u : unit | destination <> local && P.code body local} -> {u : unit | P.code (Emit.protected destination value body) local} @ ghost =
  fun destination value body local premise -> ghost_ (
    P.code_def T.Empty local; status destination 0 T.Empty local ();
    let inside = T.Block (body, Emit.status destination 0 T.Empty) in
    P.code_def inside local; status destination value inside local ();
    Emit.protected_def destination value body; P.code_def (Emit.protected destination value body) local)
let rec (stores @ total) : (plan : Registers.plan) @ immutable -> (local : B.u32) ->
    {u : unit | LP.preserves (Registers.store_code plan) local} @ ghost = fun plan local -> ghost_ (
    Registers.store_code_def plan;
    match plan with
    | Registers.End -> LP.preserves_def C.Empty local
    | Registers.Binding (global, source, rest) ->
      stores rest local; LP.instruction_preserves_def (I.Global_set global) local;
      LP.preserves_def (C.Next (I.Global_set global, Registers.store_code rest)) local;
      LP.instruction_preserves_def (I.Local_get source) local; LP.preserves_def (Registers.store_code plan) local)
let (labels @ total) : (config : Assembly.config) @ immutable -> (local : B.u32) ->
    {u : unit | P.labels (Round.labels config) local} @ ghost = fun config local -> ghost_ (
    stores config.Assembly.stores local; P.code_def T.Empty local;
    embed (Registers.store_code config.Assembly.stores) T.Empty local ();
    Wasm_register_block.epilogue_def config.Assembly.stores T.Empty;
    Round.labels_def config;
    Wasm_control_branch_continue.labels_def (Wasm_register_block.epilogue config.Assembly.stores T.Empty) T.No_labels;
    P.labels_def T.No_labels local; P.labels_def (Round.labels config) local)
let (tail @ total) : (program : Lower.program) @ immutable -> (env_count : Position.slot) -> (table_base : B.u32) -> (stack_base : B.u32) -> (local : B.u32) ->
    {u : unit | local = 1 || local = 12 || local = 13} ->
    {u : unit | P.code (Emit.emit program (Block.Tail_call env_count) (Runtime.config table_base stack_base).Assembly.locals table_base stack_base) local
      && P.labels (Round.labels (Runtime.config table_base stack_base)) local} @ ghost =
  fun program env_count table_base stack_base local premise -> ghost_ (
    Runtime.config_def table_base stack_base;
    let config = Runtime.config table_base stack_base in
    let locals = config.Assembly.locals in
    let base = locals.Emit.structured in
    Capture.separate_def base.Structured.scratch local;
    loaded env_count base.Structured.scratch program.Lower.calls table_base locals.Emit.code locals.Emit.address locals.Emit.descriptor base.Structured.object_ base.Structured.frame local ();
    protected locals.Emit.status 0 (Loaded.emit env_count base.Structured.scratch program.Lower.calls table_base locals.Emit.code locals.Emit.address locals.Emit.descriptor base.Structured.object_ base.Structured.frame) local ();
    Emit.emit_def program (Block.Tail_call env_count) locals table_base stack_base;
    labels config local)
module Save = Hmc_wasm_call_save
module Padded = Hmc_wasm_call_save_padded
module PC = Hmc_wasm_pc_update
module Push = Hmc_wasm_stack_push
module Guard = Hmc_wasm_allocation_guard
module Select_guard = Hmc_wasm_allocation_select
module Exit = Hmc_wasm_allocation_exit
module Save_guard = Hmc_wasm_call_save_guard
module Ordinary = Hmc_wasm_ordinary_call
let (save @ total) : (fragment : Save.fragment) @ immutable -> (padding : Mixed.writes) @ immutable ->
    (source : B.u32) -> (top : B.u32) -> (width : B.u32) -> (local : B.u32) ->
    {u : unit | top <> local} ->
    {u : unit | LP.preserves (Push.emit (Padded.emit fragment padding source top) width top) local} @ ghost =
  fun fragment padding source top width local premise -> ghost_ (
    copies fragment.Save.copies source top local;
    PC.emit_def fragment.Save.pc top;
    Common.immediate (PC.offset ()) top (Hmc_wasm_header_update.number fragment.Save.pc) local ();
    Save.emit_def fragment source top;
    LP.append (Copies.emit fragment.Save.copies source top) (PC.emit fragment.Save.pc top) local ();
    mixed padding top local; Padded.emit_def fragment padding source top;
    LP.append (Save.emit fragment source top) (Mixed.emit padding top) local ();
    Common.advance width top local (); Push.emit_def (Padded.emit fragment padding source top) width top;
    LP.append (Padded.emit fragment padding source top) (Advance.emit width top) local ())
let (guard @ total) : (width : B.u32) -> (top : B.u32) -> (limit : B.u32) -> (local : B.u32) ->
    {u : unit | LP.preserves (Guard.emit width top limit) local} @ ghost = fun width top limit local -> ghost_ (
    Guard.emit_def width top limit; LP.preserves_def C.Empty local;
    LP.instruction_preserves_def (I.Plain I.I32_eqz) local; LP.preserves_def (C.Next (I.Plain I.I32_eqz, C.Empty)) local;
    LP.instruction_preserves_def (I.Plain I.I64_lt_u) local; LP.preserves_def (C.Next (I.Plain I.I64_lt_u, C.Next (I.Plain I.I32_eqz, C.Empty))) local;
    LP.instruction_preserves_def (I.Plain I.I64_add) local; LP.preserves_def (C.Next (I.Plain I.I64_add, C.Next (I.Plain I.I64_lt_u, C.Next (I.Plain I.I32_eqz, C.Empty)))) local;
    LP.instruction_preserves_def (I.I64_const (Hmc_wasm_header_update.number width)) local; LP.preserves_def (C.Next (I.I64_const (Hmc_wasm_header_update.number width), C.Next (I.Plain I.I64_add, C.Next (I.Plain I.I64_lt_u, C.Next (I.Plain I.I32_eqz, C.Empty))))) local;
    LP.instruction_preserves_def (I.Plain I.I64_extend_i32_u) local; LP.preserves_def (C.Next (I.Plain I.I64_extend_i32_u, C.Next (I.I64_const (Hmc_wasm_header_update.number width), C.Next (I.Plain I.I64_add, C.Next (I.Plain I.I64_lt_u, C.Next (I.Plain I.I32_eqz, C.Empty)))))) local;
    LP.instruction_preserves_def (I.Local_get top) local; LP.preserves_def (C.Next (I.Local_get top, C.Next (I.Plain I.I64_extend_i32_u, C.Next (I.I64_const (Hmc_wasm_header_update.number width), C.Next (I.Plain I.I64_add, C.Next (I.Plain I.I64_lt_u, C.Next (I.Plain I.I32_eqz, C.Empty))))))) local;
    LP.instruction_preserves_def (I.Plain I.I64_extend_i32_u) local; LP.preserves_def (C.Next (I.Plain I.I64_extend_i32_u, C.Next (I.Local_get top, C.Next (I.Plain I.I64_extend_i32_u, C.Next (I.I64_const (Hmc_wasm_header_update.number width), C.Next (I.Plain I.I64_add, C.Next (I.Plain I.I64_lt_u, C.Next (I.Plain I.I32_eqz, C.Empty)))))))) local;
    LP.instruction_preserves_def (I.Local_get limit) local; LP.preserves_def (C.Next (I.Local_get limit, C.Next (I.Plain I.I64_extend_i32_u, C.Next (I.Local_get top, C.Next (I.Plain I.I64_extend_i32_u, C.Next (I.I64_const (Hmc_wasm_header_update.number width), C.Next (I.Plain I.I64_add, C.Next (I.Plain I.I64_lt_u, C.Next (I.Plain I.I32_eqz, C.Empty))))))))) local)
let (ordinary @ total) : (program : Lower.program) @ immutable -> (call : Block.call) @ immutable ->
    (table_base : B.u32) -> (stack_base : B.u32) -> (local : B.u32) ->
    {u : unit | local = 1 || local = 12 || local = 13} ->
    {u : unit | P.code (Emit.emit program (Block.Call call) (Runtime.config table_base stack_base).Assembly.locals table_base stack_base) local
      && P.labels (Round.labels (Runtime.config table_base stack_base)) local} @ ghost =
  fun program call table_base stack_base local premise -> ghost_ (
    Runtime.config_def table_base stack_base;
    let config = Runtime.config table_base stack_base in
    let locals = config.Assembly.locals in
    let base = locals.Emit.structured in
    Capture.separate_def base.Structured.scratch local;
    loaded call.Block.environment base.Structured.scratch program.Lower.calls table_base locals.Emit.code locals.Emit.address locals.Emit.descriptor base.Structured.object_ base.Structured.frame local ();
    let tail = Loaded.emit call.Block.environment base.Structured.scratch program.Lower.calls table_base locals.Emit.code locals.Emit.address locals.Emit.descriptor base.Structured.object_ base.Structured.frame in
    save call.Block.save call.Block.padding base.Structured.frame locals.Emit.top program.Lower.width local ();
    let success = Push.emit (Padded.emit call.Block.save call.Block.padding base.Structured.frame locals.Emit.top) program.Lower.width locals.Emit.top in
    P.code_def T.Empty local; embed success T.Empty local ();
    Exit.escape_def 2; LP.instruction_preserves_def (I.Br 2) local; P.code_def (Exit.escape 2) local;
    let branch = T.If (Lift.embed success T.Empty, Exit.escape 2, tail) in
    P.code_def branch local; guard program.Lower.width locals.Emit.top locals.Emit.stack_limit local;
    embed (Guard.emit program.Lower.width locals.Emit.top locals.Emit.stack_limit) branch local ();
    Select_guard.emit_def program.Lower.width locals.Emit.top locals.Emit.stack_limit (Lift.embed success T.Empty) (Exit.escape 2) tail;
    Exit.emit_def program.Lower.width locals.Emit.top locals.Emit.stack_limit success 2 tail;
    Save_guard.body_def call.Block.save call.Block.padding base.Structured.frame locals.Emit.top program.Lower.width;
    Save_guard.emit_def call.Block.save call.Block.padding base.Structured.frame locals.Emit.top program.Lower.width locals.Emit.stack_limit 2 tail;
    Ordinary.emit_def call.Block.save call.Block.padding base.Structured.frame locals.Emit.top program.Lower.width locals.Emit.stack_limit 2
      call.Block.environment base.Structured.scratch program.Lower.calls table_base locals.Emit.code locals.Emit.address locals.Emit.descriptor base.Structured.object_;
    protected locals.Emit.status 3 (Ordinary.emit call.Block.save call.Block.padding base.Structured.frame locals.Emit.top program.Lower.width locals.Emit.stack_limit 2
      call.Block.environment base.Structured.scratch program.Lower.calls table_base locals.Emit.code locals.Emit.address locals.Emit.descriptor base.Structured.object_) local ();
    Emit.emit_def program (Block.Call call) locals table_base stack_base; labels config local)

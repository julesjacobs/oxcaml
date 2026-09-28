module B = Wasm_u32
module D = Hm_declarative
module G = Hmc_cfg_ir
module H = Hmc_heap_objects
module F = Hmc_heap_frame
module Q = Hmc_heap_state
module V = Hmc_tagged_cell
module Program = Hmc_tail_ir
module Machine = Hmc_heap_machine
module Index = Hmc_u32_index
module Wire = Hmc_heap_wire
module Bytes = Hmc_linear_bytes
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module L = Wasm_locals
module C = Wasm_code
module Func = Wasm_functions
module Calls = Wasm_calls
module Lower = Hmc_wasm_program_lower
module Block = Hmc_wasm_program_block
module Runtime = Hmc_wasm_program_runtime
module Assembly = Hmc_wasm_program_functions
module Registers = Hmc_wasm_program_registers
module Step = Hmc_wasm_program_register_step
module Dispatch = Hmc_wasm_program_dispatch
module Round = Hmc_wasm_program_roundtrip
module Codec = Hmc_pointer_frame_codec
module Header = Hmc_wasm_header_update
module List = Hmc_wasm_program_list
module Source = Hmc_wasm_list_finish_invariant
module Status = Hmc_wasm_program_status
module Structured = Hmc_wasm_structured_block
module Straight = Hmc_wasm_block_lower
module Plan = Hmc_wasm_list_lower
module Model = Hmc_frame_list_branch
module Probe = Hmc_wasm_list_probe
module Capture = Hmc_wasm_list_capture
module Full = Hmc_wasm_list_full
module Relayout = Hmc_wasm_relayout
let[@def] (fragment @ total) (plan : Plan.fragment @ immutable) = Block.Structured (Structured.List_branch plan)
let[@def] (slots @ total) (unit : unit) : Capture.slots @ immutable =
  {Capture.head_tag = 14; head_payload = 15; tail_tag = 16; tail_payload = 17}
let[@def] (next_activation @ total) (activation : F.activation @ immutable) (empty : D.index @ immutable)
    (next : D.index @ immutable) (head : V.value @ immutable) (tail : V.value @ immutable) =
  if Probe.is_nil activation.F.accumulator then {activation with F.pc = empty} else Model.transition activation next head tail
let[@def] (next_signature @ total) (signature : G.signature @ immutable) (element : D.mono @ immutable) (activation : F.activation @ immutable) =
  if Probe.is_nil activation.F.accumulator then signature else Model.successor signature element
let[@def] (next_pc @ total) (plan : Plan.fragment @ immutable) (activation : F.activation @ immutable) : B.u32 =
  if Probe.is_nil activation.F.accumulator then plan.Plan.empty_pc else plan.Plan.full.Relayout.pc
type result = {source : Source.result; activation : F.activation; registers : Registers.registers; fuel : {n : C.count | not (n === C.Zero)}}
let (correct @ total) : (lowered : Lower.program) @ immutable -> (module_ : Func.module_) @ immutable ->
    (program : Program.program) @ immutable -> (globals : Machine.globals) @ immutable -> (heap : H.heap) @ immutable ->
    (signature : G.signature) @ immutable -> (element : D.mono) @ immutable -> (activation : F.activation) @ immutable ->
    (frames : Q.frames) @ immutable -> (stack_capacity : D.index) @ immutable -> (empty : D.index) @ immutable -> (next : D.index) @ immutable ->
    (head : V.value) @ immutable -> (tail : V.value) @ immutable -> (address : B.u32) -> (plan : Plan.fragment) @ immutable ->
    (capacity : Relayout.count) -> (max_pc : B.u32) -> (old_pc : B.u32) -> (cells : H.cells) @ immutable -> (padding : H.cells) @ immutable ->
    (before : Registers.registers) @ immutable -> (memory : B.bytes) @ immutable -> (bytes : B.bytes) @ immutable -> (suffix : B.bytes) @ immutable ->
    (frame_end : B.u32) -> (table_base : B.u32) -> (stack_base : B.u32) -> (host_capacity : C.count) @ immutable -> (function_index : B.u32) ->
    {u : unit | Program.lookup program.Program.code activation.F.pc === Some (Program.Keep (G.List_branch (empty, next)))
      && (activation.F.accumulator === V.Nil || activation.F.accumulator === V.Cons_pointer address)
      && frame_end = before.Registers.frame + 16 + 16 * capacity
      && Hmc_heap_image.related memory heap && Hmc_heap_image_suffix.above heap frame_end
      && H.valid program.Program.origin.Hmc_cfg_program.origin.Hmc_closure_program.table heap
      && (activation.F.accumulator === V.Nil || Hmc_heap_preservation.lookup_object heap address === Some (H.Cons (head, tail)))
      && Plan.matches signature empty next capacity max_pc plan
      && Index.represents activation.F.pc old_pc && Index.represents (H.length cells) capacity
      && Codec.decode signature activation.F.pc cells === Some (activation, padding)
      && before.Registers.frame <= 4294967216
      && Bytes.drop memory before.Registers.frame === Some bytes
      && Wire.decode_cells (D.S (H.length cells)) bytes === Some (H.Cell (V.Word (Header.number old_pc), cells), suffix)
      && Func.signature module_.Func.signatures (Dispatch.void_signature ()) === Some Func.Void
      && Func.element module_.Func.table old_pc === Some function_index
      && Func.lookup module_.Func.functions function_index === Some (Assembly.function_ lowered (fragment plan) (Runtime.config table_base stack_base))} ->
    {out : result | Hmc_linear_preservation.equal_prefix before.Registers.frame memory out.source.Source.memory
      && out.registers === {before with Registers.status = Status.zero ()}
      && out.activation === next_activation activation empty next head tail
      && Machine.step program globals before.Registers.heap_limit stack_capacity {Machine.heap; state = Q.Running (activation, frames)} ===
        Machine.Advanced {Machine.heap; state = Q.Running (out.activation, frames)}
      && Index.represents out.activation.F.pc (next_pc plan activation)
      && Codec.decode (next_signature signature element activation) out.activation.F.pc out.source.Source.cells === Some (out.activation, out.source.Source.padding)
      && H.length out.source.Source.cells === H.length cells
      && Bytes.drop out.source.Source.memory before.Registers.frame === Some out.source.Source.bytes
      && Wire.decode_cells (D.S (H.length out.source.Source.cells)) out.source.Source.bytes ===
        Some (H.Cell (V.Word (Header.number (next_pc plan activation)), out.source.Source.cells), suffix)
      && Calls.run out.fuel module_ (Dispatch.loop (Registers.globals before) memory (C.Succ host_capacity)) ===
        Calls.Running (Dispatch.loop (Registers.globals out.registers) out.source.Source.memory (C.Succ host_capacity))} @ immutable =
  fun lowered module_ program globals heap signature element activation frames stack_capacity empty next head tail address plan capacity max_pc old_pc cells padding
      before memory bytes suffix frame_end table_base stack_base host_capacity function_index premise ->
    let config = Runtime.config table_base stack_base in
    let state = {X.memory; machine = {E.locals = Registers.locals before; stack = S.Empty}} in
    let table = program.Program.origin.Hmc_cfg_program.origin.Hmc_closure_program.table in
    ghost_ (Runtime.config_def table_base stack_base; Registers.local_values before; Registers.matches_def (Registers.locals before) before;
      List.failure_def (); L.can_set_def (Registers.locals before) 11 (S.I32 2); S.same_type_def (S.I32 before.Registers.status) (S.I32 2);
      L.can_set_def (Registers.locals before) 3 (S.I32 address); S.same_type_def (S.I32 0) (S.I32 address);
      slots_def (); Capture.distinct_def (slots ());
      Capture.separate_def (slots ()) 0; Capture.separate_def (slots ()) 1; Capture.separate_def (slots ()) 3; Capture.separate_def (slots ()) 11;
      Capture.separate_def (slots ()) 4; Capture.separate_def (slots ()) 12; Capture.separate_def (slots ()) 13;
      Capture.writable_def (slots ()) (Registers.locals before);
      Capture.word_slot_def (Registers.locals before) 14; Capture.word_slot_def (Registers.locals before) 15;
      Capture.word_slot_def (Registers.locals before) 16; Capture.word_slot_def (Registers.locals before) 17);
    let body = List.correct lowered config.Assembly.locals table_base stack_base (Round.labels config) program globals before.Registers.heap_limit stack_capacity
      frames empty signature element activation next head tail plan capacity max_pc old_pc cells padding table heap address 3 1 frame_end state 0 before.Registers.frame (slots ()) bytes suffix () in
    ghost_ (fragment_def plan);
    let registers = Step.complete lowered (fragment plan) module_ table_base stack_base before memory bytes cells suffix host_capacity old_pc function_index body.List.fuel body.List.state () in
    ghost_ (
      let top = List.correct lowered config.Assembly.locals table_base stack_base (Round.labels config) program globals before.Registers.heap_limit stack_capacity
      frames empty signature element activation next head tail plan capacity max_pc old_pc cells padding table heap address 3 4 frame_end state 0 before.Registers.frame (slots ()) bytes suffix () in
      let tag = List.correct lowered config.Assembly.locals table_base stack_base (Round.labels config) program globals before.Registers.heap_limit stack_capacity
      frames empty signature element activation next head tail plan capacity max_pc old_pc cells padding table heap address 3 12 frame_end state 0 before.Registers.frame (slots ()) bytes suffix () in
      let payload = List.correct lowered config.Assembly.locals table_base stack_base (Round.labels config) program globals before.Registers.heap_limit stack_capacity
      frames empty signature element activation next head tail plan capacity max_pc old_pc cells padding table heap address 3 13 frame_end state 0 before.Registers.frame (slots ()) bytes suffix () in
      Status.zero_def (); Registers.exports_def body.List.state.X.machine.E.locals registers;
      let _ = top in let _ = tag in let _ = payload in
      Plan.matches_def signature empty next capacity max_pc plan;
      Hmc_wasm_list_relayout.matches_def signature next capacity max_pc plan.Plan.full;
      next_activation_def activation empty next head tail; next_pc_def plan activation; next_signature_def signature element activation;
      Model.transition_def activation next head tail);
    {source = body.List.source.Full.frame; activation = next_activation activation empty next head tail;
      registers; fuel = Hmc_wasm_program_cost.dispatch (Round.cost config body.List.fuel) registers.Registers.status}

let rec (heap_objects_above @ total) : (heap : H.heap) @ immutable -> (boundary : B.u32) ->
    {u : unit | Hmc_wasm_heap_suffix.above heap boundary} ->
    {u : unit | Hmc_heap_image_suffix.above heap boundary} @ ghost = fun heap boundary premise -> ghost_ (
    Hmc_wasm_heap_suffix.above_def heap boundary; Hmc_heap_image_suffix.above_def heap boundary;
    match heap with H.Empty_heap _ -> () | H.Allocate (_, rest) -> heap_objects_above rest boundary ())

let (framed @ total) : (lowered : Lower.program) @ immutable -> (module_ : Func.module_) @ immutable ->
    (program : Program.program) @ immutable -> (globals : Machine.globals) @ immutable -> (heap : H.heap) @ immutable ->
    (signature : G.signature) @ immutable -> (element : D.mono) @ immutable -> (activation : F.activation) @ immutable ->
    (frames : Q.frames) @ immutable -> (stack_capacity : D.index) @ immutable -> (empty : D.index) @ immutable -> (next : D.index) @ immutable ->
    (head : V.value) @ immutable -> (tail : V.value) @ immutable -> (address : B.u32) -> (plan : Plan.fragment) @ immutable ->
    (capacity : Relayout.count) -> (max_pc : B.u32) -> (old_pc : B.u32) -> (cells : H.cells) @ immutable -> (padding : H.cells) @ immutable ->
    (before : Registers.registers) @ immutable -> (memory : B.bytes) @ immutable -> (bytes : B.bytes) @ immutable -> (suffix : B.bytes) @ immutable ->
    (frame_end : B.u32) -> (table_base : B.u32) -> (stack_base : B.u32) -> (host_capacity : C.count) @ immutable -> (function_index : B.u32) -> (cell_count : B.u32) -> (abstract : Hmc_cfg_semantics.state) @ immutable ->
    (runtime : Hmc_runtime_closures.table) @ immutable -> (table_count : Hmc_runtime_descriptor_table.count) ->
    {u : unit | Hmc_wasm_program_descriptors.valid program before memory runtime table_base table_count
      && Hmc_wasm_program_resources.valid program globals lowered.Lower.width stack_base frame_end abstract heap activation frames before memory
      && Hmc_wasm_program_frame.valid signature activation before memory frame_end old_pc cells padding bytes suffix cell_count
      && Program.lookup program.Program.code activation.F.pc === Some (Program.Keep (G.List_branch (empty, next)))
      && (activation.F.accumulator === V.Nil || activation.F.accumulator === V.Cons_pointer address)
      && frame_end = before.Registers.frame + 16 + 16 * capacity
      && (activation.F.accumulator === V.Nil || Hmc_heap_preservation.lookup_object heap address === Some (H.Cons (head, tail)))
      && Plan.matches signature empty next capacity max_pc plan
      && Index.represents (H.length cells) capacity
      && before.Registers.frame <= 4294967216
      && Func.signature module_.Func.signatures (Dispatch.void_signature ()) === Some Func.Void
      && Func.element module_.Func.table old_pc === Some function_index
      && Func.lookup module_.Func.functions function_index === Some (Assembly.function_ lowered (fragment plan) (Runtime.config table_base stack_base))} ->
    {out : result | Hmc_wasm_program_descriptors.valid program out.registers out.source.Source.memory runtime table_base table_count
      && Hmc_wasm_program_resources.valid program globals lowered.Lower.width stack_base frame_end (Hmc_tail_semantics.step program abstract)
        heap out.activation frames out.registers out.source.Source.memory
      && Hmc_wasm_program_frame.valid (next_signature signature element activation) out.activation out.registers out.source.Source.memory frame_end (next_pc plan activation)
        out.source.Source.cells out.source.Source.padding out.source.Source.bytes suffix cell_count
      && V.length out.source.Source.memory === V.length memory
      && Hmc_linear_preservation.equal_prefix before.Registers.frame memory out.source.Source.memory
      && out.registers === {before with Registers.status = Status.zero ()}
      && out.activation === next_activation activation empty next head tail
      && Machine.step program globals before.Registers.heap_limit stack_capacity {Machine.heap; state = Q.Running (activation, frames)} ===
        Machine.Advanced {Machine.heap; state = Q.Running (out.activation, frames)}
      && Index.represents out.activation.F.pc (next_pc plan activation)
      && Codec.decode (next_signature signature element activation) out.activation.F.pc out.source.Source.cells === Some (out.activation, out.source.Source.padding)
      && H.length out.source.Source.cells === H.length cells
      && Bytes.drop out.source.Source.memory before.Registers.frame === Some out.source.Source.bytes
      && Wire.decode_cells (D.S (H.length out.source.Source.cells)) out.source.Source.bytes ===
        Some (H.Cell (V.Word (Header.number (next_pc plan activation)), out.source.Source.cells), suffix)
      && Calls.run out.fuel module_ (Dispatch.loop (Registers.globals before) memory (C.Succ host_capacity)) ===
        Calls.Running (Dispatch.loop (Registers.globals out.registers) out.source.Source.memory (C.Succ host_capacity))} @ immutable =
  fun lowered module_ program globals heap signature element activation frames stack_capacity empty next head tail address plan capacity max_pc old_pc cells padding
      before memory bytes suffix frame_end table_base stack_base host_capacity function_index cell_count abstract runtime table_count premise ->
    ghost_ (Hmc_wasm_program_frame.valid_def signature activation before memory frame_end old_pc cells padding bytes suffix cell_count;
      Hmc_wasm_program_resources.valid_def program globals lowered.Lower.width stack_base frame_end abstract heap activation frames before memory;
      Hmc_heap_invariant.valid_def program globals before.Registers.heap_limit {Machine.heap; state = Q.Running (activation, frames)} abstract;
      heap_objects_above heap frame_end ());
    let out = correct lowered module_ program globals heap signature element activation frames stack_capacity empty next head tail address plan capacity max_pc old_pc cells padding
      before memory bytes suffix frame_end table_base stack_base host_capacity function_index () in
    ghost_ (
      Hmc_wasm_program_descriptors.preserve program before out.registers memory out.source.Source.memory runtime table_base table_count ();
      let old_cells = H.Cell (V.Word (Header.number old_pc), cells) in
      let new_cells = H.Cell (V.Word (Header.number (next_pc plan activation)), out.source.Source.cells) in
      H.length_def old_cells; H.length_def new_cells;
      Hmc_wasm_frame_preservation.correct memory out.source.Source.memory before.Registers.frame frame_end bytes out.source.Source.bytes old_cells new_cells suffix cell_count ();
      Hmc_wasm_program_resources.preserve program globals lowered.Lower.width stack_base frame_end abstract heap activation out.activation frames
        stack_capacity before out.registers memory out.source.Source.memory ();
      Hmc_wasm_program_frame.valid_def (next_signature signature element activation) out.activation out.registers out.source.Source.memory frame_end (next_pc plan activation)
        out.source.Source.cells out.source.Source.padding out.source.Source.bytes suffix cell_count);
    out

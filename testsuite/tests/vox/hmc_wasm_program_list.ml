module B = Wasm_u32
module W = Hmc_word64
module D = Hm_declarative
module G = Hmc_cfg_ir
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module Seg = Hmc_frame_segments
module Codec = Hmc_pointer_frame_codec
module Frame = Hmc_heap_frame
module Model = Hmc_frame_list_branch
module Wire = Hmc_heap_wire
module Index = Hmc_u32_index
module Lower = Hmc_wasm_relayout
module Header = Hmc_wasm_header_update
module Capture = Hmc_wasm_list_capture
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module L = Wasm_locals
module Bytes = Hmc_linear_bytes
module View = Hmc_frame_slices
module Cap = Hmc_cell_capacity
module Fits = Hmc_frame_capacity
module Store = Hmc_wasm_list_store
module Invariant = Hmc_wasm_list_finish_invariant
module Full = Hmc_wasm_list_full
module Program = Hmc_tail_ir
module Machine = Hmc_heap_machine
module State = Hmc_heap_state
module Pointer = Hmc_wasm_list_pointer
module T = Wasm_control
module C = Wasm_code
module Select = Wasm_control_select
module Continue = Wasm_control_branch_continue
module Fuel = Wasm_control_compose
module Probe = Hmc_wasm_list_probe
module Entry = Hmc_wasm_list_full_entry
module Conditional = Hmc_wasm_list_conditional
module Block = Hmc_wasm_structured_block
module List = Hmc_wasm_list_lower
module Cons_capture = Hmc_wasm_cons_capture
module Source = Hmc_wasm_list_block_entry
module Status = Hmc_wasm_program_status
module Emit = Hmc_wasm_program_emit
module Program_lower = Hmc_wasm_program_lower
let[@def] (failure @ total) (unit : unit) : B.u32 = 2
type result = {source : Full.result; state : X.state; fuel : C.count}
let (correct @ total) : (lowered : Program_lower.program) @ immutable -> (locals : Emit.locals) @ immutable -> (table_base : B.u32) -> (stack_base : B.u32) -> (outer : T.labels) @ immutable -> (program : Program.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (heap_limit : W.limb) -> (stack_limit : D.index) @ immutable -> (frames : State.frames) @ immutable -> (empty : D.index) @ immutable -> (signature : G.signature) @ immutable -> (element : D.mono) @ immutable ->
    (activation : Frame.activation) @ immutable -> (next : D.index) @ immutable ->
    (head : V.value) @ immutable -> (tail : V.value) @ immutable ->
    (list : List.fragment) @ immutable -> (capacity : Lower.count) -> (max_pc : W.limb) ->
    (old_pc : W.limb) -> (cells : Heap.cells) @ immutable -> (old_padding : Heap.cells) @ immutable ->
    (table : Hmc_closure_ir.table) @ immutable -> (heap : Heap.heap) @ immutable -> (address : B.u32) -> (object_local : B.u32) -> (heap_local : B.u32) -> (frame_stop : B.u32) -> (state : X.state) @ immutable -> (base_local : B.u32) -> (base : B.u32) -> (slots : Capture.slots) @ immutable ->
    (before_frame : B.bytes) @ immutable -> (suffix : B.bytes) @ immutable ->
    {u : unit | base_local = locals.Emit.structured.Block.frame && object_local = locals.Emit.structured.Block.object_
      && slots.Capture.head_tag = locals.Emit.structured.Block.scratch.Cons_capture.head_tag
      && slots.Capture.head_payload = locals.Emit.structured.Block.scratch.Cons_capture.head_payload
      && slots.Capture.tail_tag = locals.Emit.structured.Block.scratch.Cons_capture.tail_tag
      && slots.Capture.tail_payload = locals.Emit.structured.Block.scratch.Cons_capture.tail_payload
      && locals.Emit.status <> base_local && locals.Emit.status <> heap_local
      && L.can_set state.X.machine.E.locals locals.Emit.status (S.I32 (failure ()))
      && Program.lookup program.Program.code activation.Frame.pc === Some (Program.Keep (G.List_branch (empty, next)))
      && (activation.Frame.accumulator === V.Nil || activation.Frame.accumulator === V.Cons_pointer address)
      && state.X.machine.E.stack === S.Empty
      && frame_stop = base + 16 + 16 * capacity
      && Hmc_heap_image.related state.X.memory heap && Hmc_heap_image_suffix.above heap frame_stop
      && List.matches signature empty next capacity max_pc list
      && Index.represents activation.Frame.pc old_pc && Index.represents (Heap.length cells) capacity
      && Codec.decode signature activation.Frame.pc cells === Some (activation, old_padding)
      && base + 16 + 16 * capacity <= 4294967296 && base <= 4294967216
      && L.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && Heap.valid table heap && (activation.Frame.accumulator === V.Nil || Hmc_heap_preservation.lookup_object heap address === Some (Heap.Cons (head, tail)))
      && L.can_set state.X.machine.E.locals object_local (S.I32 address)
      && object_local <> base_local && object_local <> heap_local && object_local <> locals.Emit.status && Capture.separate slots locals.Emit.status
      && Capture.distinct slots && Capture.separate slots object_local && Capture.separate slots base_local && Capture.separate slots heap_local
      && Capture.writable slots state.X.machine.E.locals
      && Bytes.drop state.X.memory base === Some before_frame
      && Wire.decode_cells (D.S (Heap.length cells)) before_frame === Some (Heap.Cell (V.Word (Header.number old_pc), cells), suffix)} ->
    {out : result | Hmc_linear_preservation.equal_prefix base state.X.memory out.state.X.memory
      && L.get out.state.X.machine.E.locals heap_local === L.get state.X.machine.E.locals heap_local
      && out.fuel === Status.cost (Conditional.cost list.List.full list.List.empty_pc base_local object_local slots (Probe.is_nil activation.Frame.accumulator))
      && Machine.step program globals heap_limit stack_limit {Machine.heap; state = State.Running (activation, frames)}
        === Machine.Advanced {Machine.heap; state = State.Running ((if Probe.is_nil activation.Frame.accumulator then {activation with Frame.pc = empty} else Model.transition activation next head tail), frames)}
      && Hmc_heap_image.related out.source.Full.frame.Invariant.memory heap
      && Bytes.drop out.source.Full.frame.Invariant.memory frame_stop === Some suffix
      && out.state.X.memory === out.source.Full.frame.Invariant.memory && out.state.X.machine.E.stack === S.Empty
      && L.get out.state.X.machine.E.locals locals.Emit.status === Some (S.I32 (Status.zero ()))
      && T.run out.fuel {T.code = Emit.emit lowered (Hmc_wasm_program_block.Structured (Block.List_branch list)) locals table_base stack_base; labels = outer; state}
        === T.Running {T.code = T.Empty; labels = outer; state = out.state}
      && Codec.decode (if Probe.is_nil activation.Frame.accumulator then signature else Model.successor signature element) (if Probe.is_nil activation.Frame.accumulator then empty else next) out.source.Full.frame.Invariant.cells === Some ((if Probe.is_nil activation.Frame.accumulator then {activation with Frame.pc = empty} else Model.transition activation next head tail), out.source.Full.frame.Invariant.padding)
      && Heap.length out.source.Full.frame.Invariant.cells === Heap.length cells && Bytes.drop out.source.Full.frame.Invariant.memory base === Some out.source.Full.frame.Invariant.bytes
      && Wire.decode_cells (D.S (Heap.length out.source.Full.frame.Invariant.cells)) out.source.Full.frame.Invariant.bytes === Some (Heap.Cell (V.Word (Header.number (if Probe.is_nil activation.Frame.accumulator then list.List.empty_pc else list.List.full.Lower.pc)), out.source.Full.frame.Invariant.cells), suffix)
      && L.get out.source.Full.locals base_local === Some (S.I32 base)
      && L.get out.source.Full.locals heap_local === L.get state.X.machine.E.locals heap_local
      && L.get out.source.Full.locals locals.Emit.status === Some (S.I32 (failure ()))} @ immutable =
  fun lowered locals table_base stack_base outer program globals heap_limit stack_limit frames empty signature element activation next head tail list capacity max_pc old_pc cells old_padding table heap address object_local heap_local frame_stop state base_local base slots before_frame suffix premise ->
    let local = locals.Emit.status in
    let body = Block.emit (Block.List_branch list) locals.Emit.structured 2 in
    let prepared = Status.prepare local (failure ()) body outer state () in
    ghost_ (Capture.separate_def slots local;
      L.other_local state.X.machine.E.locals local (S.I32 (failure ())) prepared.X.machine.E.locals base_local ();
      L.other_local state.X.machine.E.locals local (S.I32 (failure ())) prepared.X.machine.E.locals heap_local ();
      L.other_local state.X.machine.E.locals local (S.I32 (failure ())) prepared.X.machine.E.locals object_local ();
      L.can_set_def state.X.machine.E.locals object_local (S.I32 address); L.can_set_def prepared.X.machine.E.locals object_local (S.I32 address);
      L.other_local state.X.machine.E.locals local (S.I32 (failure ())) prepared.X.machine.E.locals slots.Capture.head_tag ();
      L.other_local state.X.machine.E.locals local (S.I32 (failure ())) prepared.X.machine.E.locals slots.Capture.head_payload ();
      L.other_local state.X.machine.E.locals local (S.I32 (failure ())) prepared.X.machine.E.locals slots.Capture.tail_tag ();
      L.other_local state.X.machine.E.locals local (S.I32 (failure ())) prepared.X.machine.E.locals slots.Capture.tail_payload ();
      Capture.writable_def slots state.X.machine.E.locals; Capture.writable_def slots prepared.X.machine.E.locals;
      Capture.word_slot_def state.X.machine.E.locals slots.Capture.head_tag; Capture.word_slot_def prepared.X.machine.E.locals slots.Capture.head_tag;
      Capture.word_slot_def state.X.machine.E.locals slots.Capture.head_payload; Capture.word_slot_def prepared.X.machine.E.locals slots.Capture.head_payload;
      Capture.word_slot_def state.X.machine.E.locals slots.Capture.tail_tag; Capture.word_slot_def prepared.X.machine.E.locals slots.Capture.tail_tag;
      Capture.word_slot_def state.X.machine.E.locals slots.Capture.tail_payload; Capture.word_slot_def prepared.X.machine.E.locals slots.Capture.tail_payload);
    let source = Source.correct (Status.scope local outer) 2 local program globals heap_limit stack_limit frames empty signature element activation next head tail list capacity max_pc old_pc cells old_padding table heap address object_local heap_local frame_stop prepared base_local base slots before_frame suffix () in
    let after_body = {X.memory = source.Full.frame.Invariant.memory; machine = {E.locals = source.Full.locals; stack = S.Empty}} in
    let body_fuel = Conditional.cost list.List.full list.List.empty_pc base_local object_local slots (Probe.is_nil activation.Frame.accumulator) in
    ghost_ (Source.locals_def base_local heap_local local object_local slots;
      Block.emit_def (Block.List_branch list) (Source.locals base_local heap_local local object_local slots) 2;
      Block.emit_def (Block.List_branch list) locals.Emit.structured 2;
      L.can_set_def source.Full.locals local (S.I32 (Status.zero ()));
      S.same_type_def (S.I32 (failure ())) (S.I32 (Status.zero ())));
    let after = Status.normal local (failure ()) body outer state prepared after_body body_fuel () in
    ghost_ (L.other_local source.Full.locals local (S.I32 (Status.zero ())) after.X.machine.E.locals heap_local ();
      failure_def (); Emit.emit_def lowered (Hmc_wasm_program_block.Structured (Block.List_branch list)) locals table_base stack_base);
    {source; state = after; fuel = Status.cost body_fuel}

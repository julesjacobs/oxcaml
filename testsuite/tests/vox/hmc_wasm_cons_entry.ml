module B = Wasm_u32
module W = Hmc_word64
module D = Hm_declarative
module G = Hmc_cfg_ir
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module Seg = Hmc_frame_segments
module Codec = Hmc_pointer_frame_codec
module Frame = Hmc_heap_frame
module State = Hmc_heap_state
module Simple = Hmc_heap_simple
module Model = Hmc_frame_value_pop
module Wire = Hmc_heap_wire
module Index = Hmc_u32_index
module Lower = Hmc_wasm_value_pop
module Relayout = Hmc_wasm_relayout
module Geometry = Hmc_wasm_relayout_geometry
module Header = Hmc_wasm_header_update
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module Bytes = Hmc_linear_bytes
module Invariant = Hmc_wasm_cons_finish_invariant
module Allocate = Hmc_wasm_cons_allocate
module A = Hmc_heap_allocate
module Machine = Hmc_heap_machine
module K = Hmc_closure_ir
module Image = Hmc_heap_image
module Above = Hmc_heap_image_suffix
module P = Hmc_linear_preservation
module L = Wasm_locals
module Transport = Hmc_wasm_frame_transport
module Guarded = Hmc_wasm_cons_guarded
module Success = Hmc_wasm_cons_success
module T = Wasm_control
module Exit = Hmc_wasm_allocation_exit
module Continue = Wasm_control_branch_continue
module Block = Hmc_wasm_structured_block
module Capture = Hmc_wasm_cons_capture
module Slots = Hmc_wasm_simple_lower
module Branch = Wasm_control_branch_target
module C = Wasm_code
module Fuel = Wasm_control_compose
module Lift = Wasm_control_lift
let[@def] (locals @ total) (base : B.u32) (heap : B.u32) (limit : B.u32) (slots : Capture.slots @ immutable) : Block.locals @ immutable =
  {Block.frame = base; heap; limit; object_ = limit; scratch = slots}
let[@def] (cost @ total) (fragment : Lower.fragment @ immutable) (base : B.u32) (heap : B.u32) (limit : B.u32)
    (slots : Capture.slots @ immutable) (cursor : B.u32) (bound : B.u32) =
  Fuel.add (C.length (Capture.emit fragment.Lower.head_tag fragment.Lower.head_payload slots base))
    (Exit.cost (Wasm_four_words.width ()) heap limit
      (Success.emit fragment base heap slots.Capture.head_tag slots.Capture.head_payload slots.Capture.tail_tag slots.Capture.tail_payload) cursor bound)
type result = {result : Guarded.result; captured : X.state; target : T.configuration}
let (correct @ total) : (outer : T.labels) @ immutable -> (depth : B.u32) -> (env_count : Slots.slot) ->
    (program : Hmc_tail_ir.program) @ immutable -> (globals : Machine.globals) @ immutable -> (stack_limit : D.index) @ immutable -> (signature : G.signature) @ immutable -> (next_signature : G.signature) @ immutable ->
    (activation : Frame.activation) @ immutable -> (next_activation : Frame.activation) @ immutable -> (frames : State.frames) @ immutable ->
    (result_type : D.mono) @ immutable -> (head_type : D.mono) @ immutable -> (next : D.index) @ immutable ->
    (context : D.context) @ immutable -> (schema : G.temporaries) @ immutable -> (fragment : Lower.fragment) @ immutable -> (capacity : Relayout.count) -> (max_pc : W.limb) ->
    (old_pc : W.limb) -> (left : V.value) @ immutable -> (right : V.value) @ immutable ->
    (heap_base : B.u32) -> (heap_local : B.u32) -> (limit_local : B.u32) -> (cells : Heap.cells) @ immutable -> (old_padding : Heap.cells) @ immutable ->
    (table : K.table) @ immutable -> (heap : Heap.heap) @ immutable -> (limit : B.u32) ->
    (slots : Capture.slots) @ immutable -> (frame_stop : B.u32) -> (state : X.state) @ immutable -> (base_local : B.u32) -> (base : B.u32) -> (before_frame : B.bytes) @ immutable -> (tail : B.bytes) @ immutable ->
    {u : unit | Branch.valid depth (Continue.labels T.Empty outer)
      && Index.represents (Codec.locals_size signature.G.locals) env_count
      && base + 48 + 16 * env_count <= 4294967280
      && Capture.distinct slots && Capture.separate slots base_local && Capture.separate slots heap_local && Capture.separate slots limit_local
      && Capture.writable slots state.X.machine.E.locals
      && Hmc_tail_ir.lookup program.Hmc_tail_ir.code activation.Frame.pc === Some (Hmc_tail_ir.Keep (G.Cons next))
      && state.X.machine.E.stack === S.Empty
      && L.get state.X.machine.E.locals limit_local === Some (S.I32 limit)
      && Heap.valid table heap && Heap.object_valid table (Heap.view heap) (Heap.Cons (left, right))
      && Heap.used heap = heap_base && heap_base <= limit && Hmc_linear_bounds.covers state.X.memory limit
      && frame_stop <= heap_base && base_local <> heap_local
      && frame_stop = base + 16 + 16 * capacity
      && Hmc_heap_image.related state.X.memory heap && Hmc_heap_image_suffix.above heap frame_stop
      && Lower.matches signature next capacity max_pc fragment
      && signature.G.temporaries === G.Value (context, head_type, schema)
      && activation.Frame.accumulator === right
      && (match activation.Frame.temporaries with Frame.Value (w, _, _) -> w === left | _ -> false)
      && Index.represents activation.Frame.pc old_pc && Index.represents (Heap.length cells) capacity
      && Model.successor signature result_type === Some next_signature
      && Model.transition activation next (V.Cons_pointer heap_base) === Some next_activation
      && Codec.decode signature activation.Frame.pc cells === Some (activation, old_padding)
      && base + 16 + 16 * capacity <= 4294967296
      && Wasm_locals.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && Wasm_locals.get state.X.machine.E.locals heap_local === Some (S.I32 heap_base)
      && Bytes.drop state.X.memory base === Some before_frame
      && Wire.decode_cells (D.S (Heap.length cells)) before_frame === Some (Heap.Cell (V.Word (Header.number old_pc), cells), tail)} ->
    {out : result | out.captured.X.machine.E.stack === S.Empty
      && L.get out.captured.X.machine.E.locals limit_local === Some (S.I32 limit)
      && Machine.step program globals limit stack_limit {Machine.heap; state = State.Running (activation, frames)} === out.result.Guarded.source
      && L.get out.captured.X.machine.E.locals heap_local === Some (S.I32 heap_base)
      && X.run (Capture.emit fragment.Lower.head_tag fragment.Lower.head_payload slots base_local) state === X.Done out.captured
      && T.run (cost fragment base_local heap_local limit_local slots heap_base limit)
        {T.code = Block.emit (Block.Cons fragment) (locals base_local heap_local limit_local slots) depth; labels = outer; state} === T.Running out.target
      && (match out.result.Guarded.allocation with
        | A.Exhausted -> T.branch depth (Continue.labels T.Empty outer) out.captured === T.Running out.target
        | A.Allocated _ -> out.target === {T.code = T.Empty; labels = outer; state = out.result.Guarded.state})
      && (match out.result.Guarded.allocation with
        | A.Exhausted -> heap_base + 32 > limit
          && out.result.Guarded.source === Machine.Exhausted Machine.Heap && out.result.Guarded.state === out.captured && out.captured.X.memory === state.X.memory
        | A.Allocated allocation -> X.run (Success.emit fragment base_local heap_local slots.Capture.head_tag slots.Capture.head_payload slots.Capture.tail_tag slots.Capture.tail_payload) out.captured === X.Done out.result.Guarded.state
          && A.correct table heap limit (Heap.Cons (left, right)) out.result.Guarded.allocation
          && P.equal_prefix heap_base state.X.memory out.result.Guarded.allocated
          && P.equal_prefix base state.X.memory out.result.Guarded.state.X.memory
          && Bytes.drop out.result.Guarded.allocated frame_stop === Some out.result.Guarded.tail && Bytes.drop out.result.Guarded.state.X.memory frame_stop === Some out.result.Guarded.tail
          && Hmc_linear_bounds.covers out.result.Guarded.allocated frame_stop && Hmc_linear_bounds.covers out.result.Guarded.state.X.memory frame_stop
          && V.length out.result.Guarded.state.X.memory === V.length state.X.memory
          && out.result.Guarded.source === Machine.Advanced {Machine.heap = allocation.A.heap; state = State.Running (next_activation, frames)}
          && L.get out.result.Guarded.state.X.machine.E.locals heap_local === Some (S.I32 (Heap.used allocation.A.heap))
          && out.result.Guarded.state.X.machine.E.stack === S.Empty
          && Image.related out.result.Guarded.state.X.memory allocation.A.heap
          && (match out.result.Guarded.frame with
            | None -> false
            | Some frame -> out.result.Guarded.state.X.memory === frame.Invariant.memory
              && Codec.decode next_signature next_activation.Frame.pc frame.Invariant.cells === Some (next_activation, frame.Invariant.padding)
              && Heap.length frame.Invariant.cells === Heap.length cells
              && Bytes.drop out.result.Guarded.state.X.memory base === Some frame.Invariant.bytes
              && Wire.decode_cells (D.S (Heap.length frame.Invariant.cells)) frame.Invariant.bytes ===
                Some (Heap.Cell (V.Word (Header.number fragment.Lower.pc), frame.Invariant.cells), out.result.Guarded.tail)))} @ immutable =
  fun outer depth env_count program globals stack_limit signature next_signature activation next_activation frames result_type head_type next context schema fragment capacity max_pc old_pc left right heap_base heap_local limit_local cells old_padding table heap limit slots frame_stop state base_local base before_frame tail premise ->
    let captured_locals = Hmc_wasm_cons_operands.correct signature activation context head_type schema left cells old_padding old_pc env_count state base base_local heap_local limit_local slots before_frame tail () in
    let captured = {X.memory = state.X.memory; machine = {E.locals = captured_locals; stack = state.X.machine.E.stack}} in
    let exhausted = Branch.target depth (Continue.labels T.Empty outer) captured () in
    let out = Hmc_wasm_cons_continue.correct T.Empty outer depth exhausted program globals stack_limit signature next_signature activation next_activation frames result_type head_type next context schema fragment capacity max_pc old_pc left right heap_base heap_local limit_local cells old_padding table heap limit slots.Capture.head_tag slots.Capture.head_payload slots.Capture.tail_tag slots.Capture.tail_payload frame_stop captured base_local base before_frame tail () in
    let capture_code = Capture.emit fragment.Lower.head_tag fragment.Lower.head_payload slots base_local in
    let body = Success.emit fragment base_local heap_local slots.Capture.head_tag slots.Capture.head_payload slots.Capture.tail_tag slots.Capture.tail_payload in
    let guarded = Exit.emit (Wasm_four_words.width ()) heap_local limit_local body depth T.Empty in
    let target = match out.Guarded.allocation with
      | A.Exhausted -> exhausted
      | A.Allocated _ -> {T.code = T.Empty; labels = outer; state = out.Guarded.state} in
    ghost_ (Lower.matches_def signature next capacity max_pc fragment;
      Geometry.size_represents (Codec.locals_size signature.G.locals) env_count ();
      Slots.slot_tag_def env_count; Slots.slot_payload_def env_count;
      Wasm_control_success.straight capture_code state captured ();
      Lift.correct capture_code guarded outer state captured ();
      locals_def base_local heap_local limit_local slots;
      Block.emit_def (Block.Cons fragment) (locals base_local heap_local limit_local slots) depth;
      cost_def fragment base_local heap_local limit_local slots heap_base limit;
      Fuel.correct (C.length capture_code) (Exit.cost (Wasm_four_words.width ()) heap_local limit_local body heap_base limit)
        {T.code = Block.emit (Block.Cons fragment) (locals base_local heap_local limit_local slots) depth; labels = outer; state});
    {result = out; captured; target}

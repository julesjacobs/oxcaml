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
let (correct @ total) : (continuation : T.code) @ immutable -> (outer : T.labels) @ immutable -> (depth : B.u32) -> (exhausted : T.configuration) @ immutable ->
    (program : Hmc_tail_ir.program) @ immutable -> (globals : Machine.globals) @ immutable -> (stack_limit : D.index) @ immutable -> (signature : G.signature) @ immutable -> (next_signature : G.signature) @ immutable ->
    (activation : Frame.activation) @ immutable -> (next_activation : Frame.activation) @ immutable -> (frames : State.frames) @ immutable ->
    (result_type : D.mono) @ immutable -> (head_type : D.mono) @ immutable -> (next : D.index) @ immutable ->
    (context : D.context) @ immutable -> (schema : G.temporaries) @ immutable -> (fragment : Lower.fragment) @ immutable -> (capacity : Relayout.count) -> (max_pc : W.limb) ->
    (old_pc : W.limb) -> (left : V.value) @ immutable -> (right : V.value) @ immutable ->
    (heap_base : B.u32) -> (heap_local : B.u32) -> (limit_local : B.u32) -> (cells : Heap.cells) @ immutable -> (old_padding : Heap.cells) @ immutable ->
    (table : K.table) @ immutable -> (heap : Heap.heap) @ immutable -> (limit : B.u32) ->
    (head_tag : B.u32) -> (head_payload : B.u32) -> (tail_tag : B.u32) -> (tail_payload : B.u32) -> (frame_stop : B.u32) -> (state : X.state) @ immutable -> (base_local : B.u32) -> (base : B.u32) -> (before_frame : B.bytes) @ immutable -> (tail : B.bytes) @ immutable ->
    {u : unit | T.branch depth (Continue.labels continuation outer) state === T.Running exhausted
      && Hmc_tail_ir.lookup program.Hmc_tail_ir.code activation.Frame.pc === Some (Hmc_tail_ir.Keep (G.Cons next))
      && state.X.machine.E.stack === S.Empty
      && L.get state.X.machine.E.locals limit_local === Some (S.I32 limit)
      && Heap.valid table heap && Heap.object_valid table (Heap.view heap) (Heap.Cons (left, right))
      && Heap.used heap = heap_base && heap_base <= limit && Hmc_linear_bounds.covers state.X.memory limit
      && frame_stop <= heap_base && base_local <> heap_local
      && L.get state.X.machine.E.locals head_tag === Some (S.I64 (V.tag left))
      && L.get state.X.machine.E.locals head_payload === Some (S.I64 (V.payload left))
      && L.get state.X.machine.E.locals tail_tag === Some (S.I64 (V.tag right))
      && L.get state.X.machine.E.locals tail_payload === Some (S.I64 (V.payload right))
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
    {out : Guarded.result | Machine.step program globals limit stack_limit {Machine.heap; state = State.Running (activation, frames)} === out.Guarded.source
      && T.run (Exit.cost (Wasm_four_words.width ()) heap_local limit_local
          (Success.emit fragment base_local heap_local head_tag head_payload tail_tag tail_payload) heap_base limit)
        {T.code = Exit.emit (Wasm_four_words.width ()) heap_local limit_local
          (Success.emit fragment base_local heap_local head_tag head_payload tail_tag tail_payload) depth continuation;
          labels = outer; state}
        === T.Running (match out.Guarded.allocation with
          | A.Exhausted -> exhausted
          | A.Allocated _ -> {T.code = continuation; labels = outer; state = out.Guarded.state})
      && (match out.Guarded.allocation with
        | A.Exhausted -> heap_base + 32 > limit && out.Guarded.source === Machine.Exhausted Machine.Heap && out.Guarded.state === state
        | A.Allocated allocation -> X.run (Success.emit fragment base_local heap_local head_tag head_payload tail_tag tail_payload) state === X.Done out.Guarded.state
          && A.correct table heap limit (Heap.Cons (left, right)) out.Guarded.allocation
          && P.equal_prefix heap_base state.X.memory out.Guarded.allocated
          && P.equal_prefix base state.X.memory out.Guarded.state.X.memory
          && Bytes.drop out.Guarded.allocated frame_stop === Some out.Guarded.tail && Bytes.drop out.Guarded.state.X.memory frame_stop === Some out.Guarded.tail
          && Hmc_linear_bounds.covers out.Guarded.allocated frame_stop && Hmc_linear_bounds.covers out.Guarded.state.X.memory frame_stop
          && V.length out.Guarded.state.X.memory === V.length state.X.memory
          && out.Guarded.source === Machine.Advanced {Machine.heap = allocation.A.heap; state = State.Running (next_activation, frames)}
          && L.get out.Guarded.state.X.machine.E.locals heap_local === Some (S.I32 (Heap.used allocation.A.heap))
          && out.Guarded.state.X.machine.E.stack === S.Empty
          && Image.related out.Guarded.state.X.memory allocation.A.heap
          && (match out.Guarded.frame with
            | None -> false
            | Some frame -> out.Guarded.state.X.memory === frame.Invariant.memory
              && Codec.decode next_signature next_activation.Frame.pc frame.Invariant.cells === Some (next_activation, frame.Invariant.padding)
              && Heap.length frame.Invariant.cells === Heap.length cells
              && Bytes.drop out.Guarded.state.X.memory base === Some frame.Invariant.bytes
              && Wire.decode_cells (D.S (Heap.length frame.Invariant.cells)) frame.Invariant.bytes ===
                Some (Heap.Cell (V.Word (Header.number fragment.Lower.pc), frame.Invariant.cells), out.Guarded.tail)))} @ immutable =
  fun continuation outer depth exhausted program globals stack_limit signature next_signature activation next_activation frames result_type head_type next context schema fragment capacity max_pc old_pc left right heap_base heap_local limit_local cells old_padding table heap limit head_tag head_payload tail_tag tail_payload frame_stop state base_local base before_frame tail premise ->
    let out = Guarded.correct program globals stack_limit signature next_signature activation next_activation frames result_type head_type next context schema fragment capacity max_pc old_pc left right heap_base heap_local limit_local cells old_padding table heap limit head_tag head_payload tail_tag tail_payload frame_stop state base_local base before_frame tail () in
    ghost_ (Wasm_four_words.width_def ();
      (match out.Guarded.allocation with
      | A.Exhausted -> ()
      | A.Allocated allocation ->
        A.correct_def table heap limit (Heap.Cons (left, right)) out.Guarded.allocation;
        Heap.slots_def (Heap.Cons (left, right));
        Index.represents_def (D.S (D.S D.Z)) 2; Index.represents_def (D.S D.Z) 1; Index.represents_def D.Z 0;
        Hmc_wasm_reservation.span (D.S (D.S D.Z)) 2 heap_base (Heap.used allocation.A.heap) ());
      Exit.correct 32 heap_local limit_local (Success.emit fragment base_local heap_local head_tag head_payload tail_tag tail_payload)
        depth continuation outer heap_base limit state out.Guarded.state ());
    out

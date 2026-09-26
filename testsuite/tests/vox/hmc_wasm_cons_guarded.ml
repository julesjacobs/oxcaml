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
module Success = Hmc_wasm_cons_success
module T = Wasm_control
module C = Wasm_code
module Lift = Wasm_control_lift
module Fuel = Wasm_control_compose
module Select = Hmc_wasm_allocation_select
module Finish = Wasm_control_branch_finish
let[@def] (emit @ total) (fragment : Lower.fragment @ immutable) (base_local : B.u32) (heap_local : B.u32) (limit_local : B.u32)
    (head_tag : B.u32) (head_payload : B.u32) (tail_tag : B.u32) (tail_payload : B.u32) =
  Select.emit (Wasm_four_words.width ()) heap_local limit_local
    (Lift.embed (Success.emit fragment base_local heap_local head_tag head_payload tail_tag tail_payload) T.Empty) T.Empty T.Empty
let[@def] (cost @ total) (fragment : Lower.fragment @ immutable) (base_local : B.u32) (heap_local : B.u32) (limit_local : B.u32)
    (head_tag : B.u32) (head_payload : B.u32) (tail_tag : B.u32) (tail_payload : B.u32) (base : B.u32) (limit : B.u32) =
  Fuel.add (Select.cost (Wasm_four_words.width ()) heap_local limit_local)
    (Finish.cost (if base + 32 <= limit then Success.emit fragment base_local heap_local head_tag head_payload tail_tag tail_payload else C.Empty))
type result = {allocated : B.bytes; source : Machine.result; allocation : A.result; state : X.state; frame : Invariant.result option; tail : B.bytes}
let (correct @ total) : (program : Hmc_tail_ir.program) @ immutable -> (globals : Machine.globals) @ immutable -> (stack_limit : D.index) @ immutable -> (signature : G.signature) @ immutable -> (next_signature : G.signature) @ immutable ->
    (activation : Frame.activation) @ immutable -> (next_activation : Frame.activation) @ immutable -> (frames : State.frames) @ immutable ->
    (result_type : D.mono) @ immutable -> (head_type : D.mono) @ immutable -> (next : D.index) @ immutable ->
    (context : D.context) @ immutable -> (schema : G.temporaries) @ immutable -> (fragment : Lower.fragment) @ immutable -> (capacity : Relayout.count) -> (max_pc : W.limb) ->
    (old_pc : W.limb) -> (left : V.value) @ immutable -> (right : V.value) @ immutable ->
    (heap_base : B.u32) -> (heap_local : B.u32) -> (limit_local : B.u32) -> (cells : Heap.cells) @ immutable -> (old_padding : Heap.cells) @ immutable ->
    (table : K.table) @ immutable -> (heap : Heap.heap) @ immutable -> (limit : B.u32) ->
    (head_tag : B.u32) -> (head_payload : B.u32) -> (tail_tag : B.u32) -> (tail_payload : B.u32) -> (frame_stop : B.u32) -> (state : X.state) @ immutable -> (base_local : B.u32) -> (base : B.u32) -> (before_frame : B.bytes) @ immutable -> (tail : B.bytes) @ immutable ->
    {u : unit | Hmc_tail_ir.lookup program.Hmc_tail_ir.code activation.Frame.pc === Some (Hmc_tail_ir.Keep (G.Cons next))
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
    {out : result | Machine.step program globals limit stack_limit {Machine.heap; state = State.Running (activation, frames)} === out.source
      && Machine.allocate heap limit (Heap.Cons (left, right)) === out.allocation
      && T.run (cost fragment base_local heap_local limit_local head_tag head_payload tail_tag tail_payload heap_base limit)
        {T.code = emit fragment base_local heap_local limit_local head_tag head_payload tail_tag tail_payload; labels = T.No_labels; state} === T.Finished out.state
      && (match out.allocation, out.frame with
        | A.Exhausted, None -> out.state === state && heap_base + 32 > limit && out.source === Machine.Exhausted Machine.Heap
        | A.Allocated allocation, Some frame ->
          out.source === Machine.Advanced {Machine.heap = allocation.A.heap; state = State.Running (next_activation, frames)}
          && P.equal_prefix heap_base state.X.memory out.allocated
          && P.equal_prefix base state.X.memory out.state.X.memory
          && Bytes.drop out.allocated frame_stop === Some out.tail && Bytes.drop out.state.X.memory frame_stop === Some out.tail
          && Hmc_linear_bounds.covers out.allocated frame_stop && Hmc_linear_bounds.covers out.state.X.memory frame_stop
          && V.length out.state.X.memory === V.length state.X.memory
          && A.correct table heap limit (Heap.Cons (left, right)) out.allocation
          && Image.related out.state.X.memory allocation.A.heap && allocation.A.reference === V.Cons_pointer heap_base
          && X.run (Success.emit fragment base_local heap_local head_tag head_payload tail_tag tail_payload) state === X.Done out.state
          && out.state.X.memory === frame.Invariant.memory
          && L.get out.state.X.machine.E.locals heap_local === Some (S.I32 (Heap.used allocation.A.heap))
          && L.get out.state.X.machine.E.locals base_local === Some (S.I32 base)
          && out.state.X.machine.E.stack === S.Empty
          && Codec.decode next_signature next_activation.Frame.pc frame.Invariant.cells === Some (next_activation, frame.Invariant.padding)
          && Heap.length frame.Invariant.cells === Heap.length cells
          && Bytes.drop out.state.X.memory base === Some frame.Invariant.bytes
          && Wire.decode_cells (D.S (Heap.length frame.Invariant.cells)) frame.Invariant.bytes ===
            Some (Heap.Cell (V.Word (Header.number fragment.Lower.pc), frame.Invariant.cells), out.tail)
        | _ -> false)} @ immutable =
  fun program globals stack_limit signature next_signature activation next_activation frames result_type head_type next context schema fragment capacity max_pc old_pc left right heap_base heap_local limit_local cells old_padding table heap limit head_tag head_payload tail_tag tail_payload frame_stop state base_local base before_frame tail premise ->
    let body = Success.emit fragment base_local heap_local head_tag head_payload tail_tag tail_payload in
    let code = emit fragment base_local heap_local limit_local head_tag head_payload tail_tag tail_payload in
    let start = {T.code; labels = T.No_labels; state} in
    ghost_ (Wasm_four_words.width_def ();
      emit_def fragment base_local heap_local limit_local head_tag head_payload tail_tag tail_payload;
      cost_def fragment base_local heap_local limit_local head_tag head_payload tail_tag tail_payload heap_base limit;
      Select.correct 32 heap_local limit_local heap_base limit state T.No_labels (Lift.embed body T.Empty) T.Empty T.Empty ();
      Finish.labels_def ());
    if heap_base + 32 <= limit then (
      let heap_end : B.u32 = heap_base + 32 in
      let out = Success.correct signature next_signature activation next_activation frames result_type head_type next context schema fragment capacity max_pc old_pc left right heap_base heap_end heap_local cells old_padding table heap limit head_tag head_payload tail_tag tail_payload frame_stop state base_local base before_frame tail () in
      ghost_ (A.correct_def table heap limit (Heap.Cons (left, right)) (A.Allocated out.Success.allocation);
        Heap.slots_def (Heap.Cons (left, right));
        Index.represents_def (D.S (D.S D.Z)) 2; Index.represents_def (D.S D.Z) 1; Index.represents_def D.Z 0;
        Hmc_wasm_reservation.span (D.S (D.S D.Z)) 2 heap_base (Heap.used out.Success.allocation.A.heap) ();
        Hmc_heap_cons_transition.success program globals heap limit stack_limit activation frames next left out.Success.allocation next_activation ();
        Finish.correct body state out.Success.state ();
        Fuel.correct (Select.cost 32 heap_local limit_local) (Finish.cost body) start);
      {allocated = out.Success.allocated; source = Machine.Advanced {Machine.heap = out.Success.allocation.A.heap; state = State.Running (next_activation, frames)}; allocation = A.Allocated out.Success.allocation; state = out.Success.state; frame = Some out.Success.frame; tail = out.Success.tail})
    else (
      ghost_ (Allocate.exhausted heap left right heap_base limit ();
        Hmc_heap_cons_transition.exhausted program globals heap limit stack_limit activation frames next left ();
        X.run_def C.Empty state; Lift.embed_def C.Empty T.Empty;
        Finish.correct C.Empty state state ();
        Fuel.correct (Select.cost 32 heap_local limit_local) (Finish.cost C.Empty) start);
      {allocated = state.X.memory; source = Machine.Exhausted Machine.Heap; allocation = A.Exhausted; state; frame = None; tail})

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
let[@def] (emit @ total) (fragment : Lower.fragment @ immutable) (base_local : B.u32) (heap_local : B.u32)
    (head_tag : B.u32) (head_payload : B.u32) (tail_tag : B.u32) (tail_payload : B.u32) =
  E.append (Allocate.emit heap_local head_tag head_payload tail_tag tail_payload)
    (Hmc_wasm_cons_finish.emit fragment base_local heap_local)
let rec (prefix_transitive @ total) : (a : B.bytes) @ immutable -> (b : B.bytes) @ immutable -> (c : B.bytes) @ immutable ->
    (base : B.u32) -> {u : unit | P.equal_prefix base a b && P.equal_prefix base b c} ->
    {u : unit | P.equal_prefix base a c} @ ghost = fun a b c base premise -> ghost_ (
    P.equal_prefix_def base a b; P.equal_prefix_def base b c; P.equal_prefix_def base a c;
    if base = 0 then () else match a, b, c with
    | B.Byte (_, x), B.Byte (_, y), B.Byte (_, z) -> prefix_transitive x y z (base - 1) ()
    | _ -> ())
type result = {allocated : B.bytes; allocation : A.allocation; state : X.state; frame : Invariant.result; tail : B.bytes}
let (correct @ total) : (signature : G.signature) @ immutable -> (next_signature : G.signature) @ immutable ->
    (activation : Frame.activation) @ immutable -> (next_activation : Frame.activation) @ immutable -> (frames : State.frames) @ immutable ->
    (result_type : D.mono) @ immutable -> (head_type : D.mono) @ immutable -> (next : D.index) @ immutable ->
    (context : D.context) @ immutable -> (schema : G.temporaries) @ immutable -> (fragment : Lower.fragment) @ immutable -> (capacity : Relayout.count) -> (max_pc : W.limb) ->
    (old_pc : W.limb) -> (left : V.value) @ immutable -> (right : V.value) @ immutable ->
    (heap_base : B.u32) -> (heap_end : B.u32) -> (heap_local : B.u32) -> (cells : Heap.cells) @ immutable -> (old_padding : Heap.cells) @ immutable ->
    (table : K.table) @ immutable -> (heap : Heap.heap) @ immutable -> (limit : B.u32) ->
    (head_tag : B.u32) -> (head_payload : B.u32) -> (tail_tag : B.u32) -> (tail_payload : B.u32) -> (frame_stop : B.u32) -> (state : X.state) @ immutable -> (base_local : B.u32) -> (base : B.u32) -> (before_frame : B.bytes) @ immutable -> (tail : B.bytes) @ immutable ->
    {u : unit | Heap.valid table heap && Heap.object_valid table (Heap.view heap) (Heap.Cons (left, right))
      && Heap.used heap = heap_base && heap_end <= limit && Hmc_linear_bounds.covers state.X.memory limit
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
      && heap_end = heap_base + 32 && Wasm_locals.get state.X.machine.E.locals heap_local === Some (S.I32 heap_base)
      && Bytes.drop state.X.memory base === Some before_frame
      && Wire.decode_cells (D.S (Heap.length cells)) before_frame === Some (Heap.Cell (V.Word (Header.number old_pc), cells), tail)} ->
    {out : result | P.equal_prefix heap_base state.X.memory out.allocated
      && P.equal_prefix base state.X.memory out.state.X.memory
      && Bytes.drop out.allocated frame_stop === Some out.tail && Bytes.drop out.state.X.memory frame_stop === Some out.tail
      && Hmc_linear_bounds.covers out.allocated frame_stop && Hmc_linear_bounds.covers out.state.X.memory frame_stop
      && V.length out.state.X.memory === V.length state.X.memory
      && Machine.allocate heap limit (Heap.Cons (left, right)) === A.Allocated out.allocation
      && A.correct table heap limit (Heap.Cons (left, right)) (A.Allocated out.allocation)
      && Image.related out.state.X.memory out.allocation.A.heap
      && out.allocation.A.reference === V.Cons_pointer heap_base
      && X.run (emit fragment base_local heap_local head_tag head_payload tail_tag tail_payload) state === X.Done out.state
      && out.state.X.memory === out.frame.Invariant.memory
      && L.get out.state.X.machine.E.locals heap_local === Some (S.I32 heap_end)
      && L.get out.state.X.machine.E.locals base_local === Some (S.I32 base)
      && out.state.X.machine.E.stack === state.X.machine.E.stack
      && Codec.decode next_signature next_activation.Frame.pc out.frame.Invariant.cells === Some (next_activation, out.frame.Invariant.padding)
      && Heap.length out.frame.Invariant.cells === Heap.length cells
      && Bytes.drop out.state.X.memory base === Some out.frame.Invariant.bytes
      && Wire.decode_cells (D.S (Heap.length out.frame.Invariant.cells)) out.frame.Invariant.bytes ===
        Some (Heap.Cell (V.Word (Header.number fragment.Lower.pc), out.frame.Invariant.cells), out.tail)} @ immutable =
  fun signature next_signature activation next_activation frames result_type head_type next context schema fragment capacity max_pc old_pc left right heap_base heap_end heap_local cells old_padding table heap limit head_tag head_payload tail_tag tail_payload frame_stop state base_local base before_frame tail premise ->
    let allocated = Allocate.correct table heap left right state heap_base limit heap_local head_tag head_payload tail_tag tail_payload () in
    let full = Heap.Cell (V.Word (Header.number old_pc), cells) in
    ghost_ (Heap.length_def full; Index.represents_def (D.S (Heap.length cells)) (capacity + 1);
      P.shrink heap_base frame_stop state.X.memory allocated.Allocate.state.X.memory ());
    let transported = Transport.correct state.X.memory allocated.Allocate.state.X.memory base frame_stop (capacity + 1) full before_frame tail () in
    ghost_ (A.correct_def table heap limit (Heap.Cons (left, right)) (A.Allocated allocated.Allocate.allocation);
      Above.above_def allocated.Allocate.allocation.A.heap frame_stop;
      L.other_local state.X.machine.E.locals heap_local (S.I32 heap_end) allocated.Allocate.state.X.machine.E.locals base_local ());
    let frame = Hmc_wasm_cons_finish_heap.correct signature next_signature activation next_activation frames result_type head_type next context schema fragment capacity max_pc old_pc left right heap_base heap_end heap_local cells old_padding allocated.Allocate.allocation.A.heap frame_stop allocated.Allocate.state base_local base transported.Transport.bytes transported.Transport.tail () in
    let final = {X.memory = frame.Invariant.memory; machine = allocated.Allocate.state.X.machine} in
    ghost_ (
      Hmc_wasm_frame_preservation.suffix allocated.Allocate.state.X.memory base frame_stop transported.Transport.bytes full transported.Transport.tail (capacity + 1) ();
      Hmc_linear_bounds.covers_def frame.Invariant.memory frame_stop;
      Hmc_wasm_frame_preservation.same_length allocated.Allocate.state.X.memory frame.Invariant.memory frame_stop transported.Transport.tail ();
      P.shrink heap_base base state.X.memory allocated.Allocate.state.X.memory ();
      prefix_transitive state.X.memory allocated.Allocate.state.X.memory frame.Invariant.memory base ();
      emit_def fragment base_local heap_local head_tag head_payload tail_tag tail_payload;
      X.append_correct (Allocate.emit heap_local head_tag head_payload tail_tag tail_payload) (Hmc_wasm_cons_finish.emit fragment base_local heap_local) state);
    {allocated = allocated.Allocate.state.X.memory; allocation = allocated.Allocate.allocation; state = final; frame; tail = transported.Transport.tail}

module B = Wasm_u32
module W = Hmc_word64
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module Wire = Hmc_heap_wire
module Seg = Hmc_frame_segments
module Copy = Hmc_wasm_call_captures
module Captures = Hmc_wasm_call_capture_memory
module Header = Hmc_wasm_call_header
module Layout = Hmc_wasm_call_header_layout
module Header_memory = Hmc_wasm_call_header_memory
module Memory = Wasm_mixed_memory
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module L = Wasm_locals
module Bytes = Hmc_linear_bytes
module Bounds = Hmc_linear_bounds
let[@def] (emit @ total) (fragment : Copy.fragment @ immutable) (pc : W.limb) (object_local : B.u32) (frame_local : B.u32) (argument_tag : B.u32) (argument_payload : B.u32) =
  E.append (Copy.emit fragment object_local frame_local) (Header.emit fragment.Copy.recursive pc frame_local object_local argument_tag argument_payload)
type result = {memory : B.bytes; bytes : B.bytes; suffix : B.bytes}
let (correct @ total) : (captures : Heap.cells) @ immutable -> (count : Hmc_wasm_relayout.count) -> (fragment : Copy.fragment) @ immutable ->
    (pc : W.limb) -> (argument : V.value) @ immutable -> (state : X.state) @ immutable ->
    (object_base : B.u32) -> (frame_base : B.u32) -> (limit : B.u32) -> (object_local : B.u32) -> (frame_local : B.u32) -> (argument_tag : B.u32) -> (argument_payload : B.u32) ->
    {u : unit | Hmc_u32_index.represents (Heap.length captures) count && Copy.position fragment.Copy.recursive + count <= 268435452
      && frame_base + Layout.width fragment.Copy.recursive + 16 * count <= limit
      && Hmc_wasm_relayout.range_is fragment.Copy.copies 0 (Copy.position fragment.Copy.recursive) (Heap.length captures) Wasm_parallel_copy.End
      && Hmc_wasm_range_copy.reads state.X.memory object_base (Hmc_wasm_closure_read.position ()) captures
      && Bounds.covers state.X.memory limit
      && L.get state.X.machine.E.locals object_local === Some (S.I32 object_base)
      && L.get state.X.machine.E.locals frame_local === Some (S.I32 frame_base)
      && L.get state.X.machine.E.locals argument_tag === Some (S.I64 (V.tag argument))
      && L.get state.X.machine.E.locals argument_payload === Some (S.I64 (V.payload argument))} ->
    {out : result | X.run (emit fragment pc object_local frame_local argument_tag argument_payload) state === X.Done {X.memory = out.memory; machine = state.X.machine}
      && V.length out.memory === V.length state.X.memory && Bounds.covers out.memory limit
      && Bytes.drop out.memory frame_base === Some out.bytes
      && Wire.decode_cells (Heap.length (Seg.append (Layout.cells fragment.Copy.recursive pc object_base argument) captures)) out.bytes ===
        Some (Seg.append (Layout.cells fragment.Copy.recursive pc object_base argument) captures, out.suffix)
      && Bytes.drop state.X.memory (S.add32 frame_base (S.add32 (Layout.width fragment.Copy.recursive) (Captures.capture_bytes count))) === Some out.suffix
      && Bytes.drop out.memory (S.add32 frame_base (S.add32 (Layout.width fragment.Copy.recursive) (Captures.capture_bytes count))) === Some out.suffix} @ immutable =
  fun captures count fragment pc argument state object_base frame_base limit object_local frame_local argument_tag argument_payload premise ->
    let header = Layout.cells fragment.Copy.recursive pc object_base argument in
    let width = Layout.width fragment.Copy.recursive in
    ghost_ (Layout.correct fragment.Copy.recursive pc object_base argument object_local argument_tag argument_payload state.X.machine.E.locals ();
      Layout.width_def fragment.Copy.recursive; Copy.position_def fragment.Copy.recursive);
    let copied = Captures.correct captures count fragment (Hmc_wire_word_sequence.words header) width state object_base frame_base limit object_local frame_local () in
    let middle = {X.memory = copied.Captures.memory; machine = state.X.machine} in
    let finished = Header_memory.correct fragment.Copy.recursive pc object_base argument middle frame_base limit frame_local object_local argument_tag argument_payload () in
    ghost_ (Captures.capture_bytes_def count;
      S.add32_def frame_base width; S.add32_def width (16 * count); S.add32_def frame_base (width + 16 * count);
      Hmc_heap_image_suffix.seek copied.Captures.memory finished.Memory.memory (frame_base + width) (frame_base + width + 16 * count) ();
      Hmc_wire_cells_join.correct header captures finished.Memory.bytes copied.Captures.captures copied.Captures.suffix ();
      emit_def fragment pc object_local frame_local argument_tag argument_payload;
      X.append_correct (Copy.emit fragment object_local frame_local) (Header.emit fragment.Copy.recursive pc frame_local object_local argument_tag argument_payload) state);
    {memory = finished.Memory.memory; bytes = finished.Memory.bytes; suffix = copied.Captures.suffix}

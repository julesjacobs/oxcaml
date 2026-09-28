module B = Wasm_u32
module W = Hmc_word64
module D = Hm_declarative
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module Seg = Hmc_frame_segments
module Index = Hmc_u32_index
module Simple = Hmc_heap_simple
module Wire = Hmc_heap_wire
module Header = Hmc_wasm_header_update
module Bytes = Hmc_linear_bytes
module M = Wasm_memory
module S = Wasm_scalar
let (correct @ total) : (memory : B.bytes) @ immutable -> (base : B.u32) -> (frame : B.bytes) @ immutable ->
    (pc : W.limb) -> (current : V.value) @ immutable -> (right : V.value) @ immutable -> (left : V.value) @ immutable ->
    (body : Heap.cells) @ immutable -> (saved : Heap.cells) @ immutable -> (tail : B.bytes) @ immutable ->
    (env_size : D.index) @ immutable -> (env_count : Hmc_wasm_relayout.count) -> (left_tag : B.u32) -> (left_offset : B.u32) ->
    {u : unit | Index.represents env_size env_count && left_tag = 48 + 16 * env_count && left_offset = left_tag + 8
      && base + 48 + 16 * env_count <= 4294967280
      && Seg.drop env_size body === Some (Heap.Cell (left, saved))
      && Bytes.drop memory base === Some frame
      && Wire.decode_cells (D.S (D.S (D.S (Heap.length body)))) frame ===
        Some (Heap.Cell (V.Word (Header.number pc), Heap.Cell (current, Heap.Cell (right, body))), tail)} ->
    {u : unit | M.load memory base left_tag M.W64 === Some (S.I64 (V.tag left))
      && M.load memory base left_offset M.W64 === Some (S.I64 (V.payload left))
      && M.load memory base (Hmc_wasm_primitive_write.tag_offset ()) M.W64 === Some (S.I64 (V.tag right))
      && M.load memory base (Hmc_wasm_primitive_payload.offset ()) M.W64 === Some (S.I64 (V.payload right))} @ ghost =
  fun memory base frame pc current right left body saved tail env_size env_count left_tag left_offset premise -> ghost_ (
    let cells = Heap.Cell (V.Word (Header.number pc), Heap.Cell (current, Heap.Cell (right, body))) in
    let index = D.S (D.S (D.S env_size)) in
    Hmc_frame_header_patch.drop env_size (V.Word (Header.number pc)) current right body;
    Hmc_cell_slice.lookup index cells (Heap.Cell (left, saved)) ();
    Index.represents_def (D.S env_size) (env_count + 1);
    Index.represents_def (D.S (D.S env_size)) (env_count + 2);
    Index.represents_def index (env_count + 3);
    Hmc_wasm_cells_read.correct memory base frame (D.S (D.S (D.S (Heap.length body)))) cells tail
      index (env_count + 3) (48 + 16 * env_count) left_offset left ();
    Simple.lookup_def cells (D.S (D.S D.Z));
    Simple.lookup_def (Heap.Cell (current, Heap.Cell (right, body))) (D.S D.Z);
    Simple.lookup_def (Heap.Cell (right, body)) D.Z;
    Index.represents_def D.Z 0; Index.represents_def (D.S D.Z) 1; Index.represents_def (D.S (D.S D.Z)) 2;
    Hmc_wasm_cells_read.correct memory base frame (D.S (D.S (D.S (Heap.length body)))) cells tail
      (D.S (D.S D.Z)) 2 32 40 right ();
    Hmc_wasm_primitive_write.tag_offset_def (); Hmc_wasm_primitive_payload.offset_def ())

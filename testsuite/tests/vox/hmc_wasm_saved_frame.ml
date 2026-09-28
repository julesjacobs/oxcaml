module B = Wasm_u32
module D = Hm_declarative
module G = Hmc_cfg_ir
module H = Hmc_heap_objects
module V = Hmc_tagged_cell
module F = Hmc_heap_frame
module Codec = Hmc_pointer_frame_codec
module Seg = Hmc_frame_segments
module Cap = Hmc_frame_capacity
module Index = Hmc_u32_index
module Header = Hmc_wasm_header_update
module Wire = Hmc_heap_wire
module Saved = Hmc_memory_saved_frame
module Bytes = Hmc_linear_bytes
let (correct @ total) : (blocks : G.table) @ immutable -> (block : G.block) @ immutable -> (activation : F.activation) @ immutable ->
    (code : B.u32) -> (cells : H.cells) @ immutable -> (padding : H.cells) @ immutable ->
    (capacity : Hmc_wasm_relayout.count) -> (memory : B.bytes) @ immutable -> (base : B.u32) ->
    (bytes : B.bytes) @ immutable -> (suffix : B.bytes) @ immutable ->
    {u : unit | G.lookup blocks activation.F.pc === Some block && Index.represents activation.F.pc code
      && Index.represents (Cap.capacity blocks) capacity && H.length (Seg.append cells padding) === Cap.capacity blocks
      && Codec.decode block.G.signature activation.F.pc cells === Some (activation, H.Empty)
      && Bytes.drop memory base === Some bytes
      && Wire.decode_cells (H.length (H.Cell (V.Word (Header.number code), Seg.append cells padding))) bytes ===
        Some (H.Cell (V.Word (Header.number code), Seg.append cells padding), suffix)} ->
    {u : unit | Saved.load blocks memory base === Some activation} @ ghost =
  fun blocks block activation code cells padding capacity memory base bytes suffix premise -> ghost_ (
    Hmc_frame_decode_suffix.correct block.G.signature activation.F.pc cells activation H.Empty padding ();
    Seg.append_def H.Empty padding;
    H.length_def (H.Cell (V.Word (Header.number code), Seg.append cells padding));
    Wire.decode_cells_def (D.S (H.length (Seg.append cells padding))) bytes;
    Wire.decode_def (Wire.Closure_schema (H.length (Seg.append cells padding))) bytes;
    Header.number_def code;
    Hmc_wasm_closure_stored.correct memory base bytes suffix code (Seg.append cells padding) capacity ();
    Hmc_memory_block_lookup.correct blocks activation.F.pc code ();
    Saved.load_def blocks memory base)

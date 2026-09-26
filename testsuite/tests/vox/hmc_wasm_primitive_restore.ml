module B = Wasm_u32
module W = Hmc_word64
module D = Hm_declarative
module G = Hmc_cfg_ir
module Heap = Hmc_heap_objects
module Seg = Hmc_frame_segments
module Patch = Hmc_cell_patch
module Codec = Hmc_pointer_frame_codec
module Wire = Hmc_heap_wire
module Index = Hmc_u32_index
module Lower = Hmc_wasm_primitive_lower
module Relayout = Hmc_wasm_relayout
module Geometry = Hmc_wasm_relayout_geometry
module Copy = Wasm_parallel_copy
module Bytes = Hmc_linear_bytes
module P = Hmc_linear_preservation
let (correct @ total) : (signature : G.signature) @ immutable -> (next : D.index) @ immutable -> (operation : D.word_operation) @ immutable ->
    (context : D.context) @ immutable -> (schema : G.temporaries) @ immutable ->
    (fragment : Lower.fragment) @ immutable -> (capacity : Relayout.count) -> (max_pc : W.limb) ->
    (saved : Heap.cells) @ immutable -> (old : Heap.cells) @ immutable ->
    (env_count : Relayout.count) -> (saved_count : Relayout.count) -> (old_count : Relayout.count) ->
    (saved_start : Heap.cells) @ immutable -> (old_start : Heap.cells) @ immutable ->
    (before : B.bytes) @ immutable -> (after : B.bytes) @ immutable -> (base : B.u32) ->
    (before_frame : B.bytes) @ immutable -> (after_frame : B.bytes) @ immutable ->
    (before_cells : Heap.cells) @ immutable -> (after_cells : Heap.cells) @ immutable -> (tail : B.bytes) @ immutable ->
    {u : unit | Lower.matches signature (G.Primitive (operation, next)) capacity max_pc fragment
      && signature.G.temporaries === G.Value (context, D.Word64, schema)
      && Heap.length saved === Codec.locals_size context && Heap.length old === Codec.temporaries_size schema
      && Index.represents (Codec.locals_size signature.G.locals) env_count
      && Index.represents (Heap.length saved) saved_count && Index.represents (Heap.length old) old_count
      && base + 16 + 16 * capacity <= 4294967296
      && Seg.drop (D.S (D.S (D.S (D.S (Codec.locals_size signature.G.locals))))) before_cells === Some saved_start
      && Seg.take (Heap.length saved) saved_start === Some saved
      && Seg.drop (D.S (D.S (D.S (D.S (D.add (Codec.locals_size signature.G.locals) (Heap.length saved)))))) before_cells === Some old_start
      && Seg.take (Heap.length old) old_start === Some old
      && Patch.write (D.S (D.S (D.S D.Z))) (Seg.append saved old) before_cells === Some after_cells
      && P.equal_prefix base before after && Bytes.drop before base === Some before_frame && Bytes.drop after base === Some after_frame
      && Wire.decode_cells (Heap.length before_cells) before_frame === Some (before_cells, tail)
      && Wire.decode_cells (Heap.length after_cells) after_frame === Some (after_cells, tail)} ->
    {u : unit | Copy.apply fragment.Lower.copies before base === Some after} @ ghost =
  fun signature next operation context schema fragment capacity max_pc saved old env_count saved_count old_count saved_start old_start before after base
      before_frame after_frame before_cells after_cells tail premise -> ghost_ (
    Lower.matches_def signature (G.Primitive (operation, next)) capacity max_pc fragment;
    Geometry.size_represents (Codec.locals_size signature.G.locals) env_count ();
    Geometry.size_represents (Heap.length saved) saved_count (); Geometry.size_represents (Heap.length old) old_count ();
    Index.represents_def D.Z 0; Index.represents_def (D.S D.Z) 1; Index.represents_def (D.S (D.S D.Z)) 2;
    Index.represents_def (D.S (Codec.locals_size signature.G.locals)) (env_count + 1);
    Index.represents_def (D.S (D.S (Codec.locals_size signature.G.locals))) (env_count + 2);
    Hmc_u32_index_sum.correct (Codec.locals_size signature.G.locals) (Heap.length saved) env_count saved_count (env_count + saved_count) ();
    Index.represents_def (D.S (D.add (Codec.locals_size signature.G.locals) (Heap.length saved))) (env_count + saved_count + 1);
    Index.represents_def (D.S (D.S (D.add (Codec.locals_size signature.G.locals) (Heap.length saved)))) (env_count + saved_count + 2);
    Index.represents_def (D.S (D.S (D.S (Codec.locals_size signature.G.locals)))) (env_count + 3);
    Index.represents_def (D.S (D.S (D.S (D.add (Codec.locals_size signature.G.locals) (Heap.length saved))))) (env_count + saved_count + 3);
    Hmc_wasm_range_read.correct saved (3 + env_count) saved_count (D.S (D.S (D.S (Codec.locals_size signature.G.locals))))
      before_cells saved_start before base before_frame tail ();
    Hmc_wasm_range_read.correct old (3 + env_count + saved_count) old_count
      (D.S (D.S (D.S (D.add (Codec.locals_size signature.G.locals) (Heap.length saved))))) before_cells old_start before base before_frame tail ();
    Hmc_wasm_range_geometry.correct saved old fragment.Lower.copies (3 + env_count) (3 + env_count + saved_count) 2
      saved_count old_count (D.S (D.S D.Z)) before before after base before_frame after_frame before_cells after_cells tail ())

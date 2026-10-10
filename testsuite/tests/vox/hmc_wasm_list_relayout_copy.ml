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
module Lower = Hmc_wasm_relayout
module Geometry = Hmc_wasm_relayout_geometry
module Copy = Wasm_parallel_copy
module Bytes = Hmc_linear_bytes
module P = Hmc_linear_preservation
let (correct @ total) : (signature : G.signature) @ immutable -> (next : D.index) @ immutable ->
    (fragment : Lower.fragment) @ immutable -> (capacity : Lower.count) -> (max_pc : W.limb) ->
    (env : Heap.cells) @ immutable -> (old : Heap.cells) @ immutable -> (env_count : Lower.count) -> (old_count : Lower.count) ->
    (env_start : Heap.cells) @ immutable -> (old_start : Heap.cells) @ immutable ->
    (before : B.bytes) @ immutable -> (after : B.bytes) @ immutable -> (base : B.u32) ->
    (before_frame : B.bytes) @ immutable -> (after_frame : B.bytes) @ immutable ->
    (before_cells : Heap.cells) @ immutable -> (after_cells : Heap.cells) @ immutable -> (tail : B.bytes) @ immutable ->
    {u : unit | Hmc_wasm_list_relayout.matches signature next capacity max_pc fragment
      && Heap.length env === Codec.locals_size signature.G.locals && Heap.length old === Codec.temporaries_size signature.G.temporaries
      && Index.represents (Heap.length env) env_count && Index.represents (Heap.length old) old_count
      && base + 16 + 16 * capacity <= 4294967296
      && Seg.drop (D.S (D.S (D.S D.Z))) before_cells === Some env_start && Seg.take (Heap.length env) env_start === Some env
      && Seg.drop (D.S (D.S (D.S (Heap.length env)))) before_cells === Some old_start && Seg.take (Heap.length old) old_start === Some old
      && Patch.write (D.S (D.S (D.S (D.S (D.S D.Z))))) (Seg.append (Seg.append env env) (Seg.append old Heap.Empty)) before_cells === Some after_cells
      && P.equal_prefix base before after && Bytes.drop before base === Some before_frame && Bytes.drop after base === Some after_frame
      && Wire.decode_cells (Heap.length before_cells) before_frame === Some (before_cells, tail)
      && Wire.decode_cells (Heap.length after_cells) after_frame === Some (after_cells, tail)} ->
    {u : unit | Copy.apply fragment.Lower.copies before base === Some after} @ ghost =
  fun signature next fragment capacity max_pc env old env_count old_count env_start old_start before after base
      before_frame after_frame before_cells after_cells tail premise -> ghost_ (
    Hmc_wasm_list_relayout.matches_def signature next capacity max_pc fragment;
    Geometry.size_represents (Heap.length env) env_count (); Geometry.size_represents (Heap.length old) old_count ();
    Index.represents_def D.Z 0; Index.represents_def (D.S D.Z) 1; Index.represents_def (D.S (D.S D.Z)) 2;
    Index.represents_def (D.S (Heap.length env)) (env_count + 1);
    Index.represents_def (D.S (D.S (Heap.length env))) (env_count + 2);
    Hmc_wasm_range_read.correct env 2 env_count (D.S (D.S D.Z)) before_cells env_start before base before_frame tail ();
    Hmc_wasm_range_read.correct old (2 + env_count) old_count (D.S (D.S (Heap.length env))) before_cells old_start before base before_frame tail ();
    Heap.length_def Heap.Empty; Wasm_parallel_copy.apply_def Copy.End before base;
    Hmc_wasm_range_copy.reads_def before base 0 Heap.Empty;
    Index.represents_def (D.S (D.S (D.S D.Z))) 3;
    Index.represents_def (D.S (D.S (D.S (D.S D.Z)))) 4;
    Hmc_wasm_range_four.correct env env old Heap.Empty fragment.Lower.copies 2 2 (2 + env_count) 0 4
      env_count env_count old_count 0 (D.S (D.S (D.S (D.S D.Z)))) before after base before_frame after_frame before_cells after_cells tail ())

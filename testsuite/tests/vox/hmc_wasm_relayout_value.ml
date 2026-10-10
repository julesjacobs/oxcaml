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
let (correct @ total) : (signature : G.signature) @ immutable -> (bind : bool) -> (next : D.index) @ immutable ->
    (context : D.context) @ immutable -> (schema : G.temporaries) @ immutable ->
    (fragment : Lower.fragment) @ immutable -> (capacity : Lower.count) -> (max_pc : W.limb) ->
    (saved : Heap.cells) @ immutable -> (old : Heap.cells) @ immutable ->
    (accumulator : Hmc_tagged_cell.value) @ immutable -> (accumulator_start : Heap.cells) @ immutable ->
    (env_count : Lower.count) -> (saved_count : Lower.count) -> (old_count : Lower.count) ->
    (saved_start : Heap.cells) @ immutable -> (old_start : Heap.cells) @ immutable ->
    (before : B.bytes) @ immutable -> (after : B.bytes) @ immutable -> (base : B.u32) ->
    (before_frame : B.bytes) @ immutable -> (after_frame : B.bytes) @ immutable ->
    (before_cells : Heap.cells) @ immutable -> (after_cells : Heap.cells) @ immutable -> (tail : B.bytes) @ immutable ->
    {u : unit | Geometry.matches signature (if bind then G.Bind next else G.Save_value next) capacity max_pc fragment.Lower.copies fragment.Lower.pc fragment.Lower.required
      && signature.G.temporaries === G.Environment (context, schema)
      && Heap.length saved === Codec.locals_size context && Heap.length old === Codec.temporaries_size schema
      && Index.represents (Codec.locals_size signature.G.locals) env_count
      && Index.represents (Heap.length saved) saved_count && Index.represents (Heap.length old) old_count
      && base + 16 + 16 * capacity <= 4294967296
      && Seg.drop (D.S (D.S (D.S (Codec.locals_size signature.G.locals)))) before_cells === Some saved_start
      && Seg.take (Heap.length saved) saved_start === Some saved
      && Seg.drop (D.S (D.S (D.S (D.add (Codec.locals_size signature.G.locals) (Heap.length saved))))) before_cells === Some old_start
      && Seg.take (Heap.length old) old_start === Some old
      && Seg.drop (D.S (D.S D.Z)) before_cells === Some accumulator_start
      && Seg.take (D.S D.Z) accumulator_start === Some (Heap.Cell (accumulator, Heap.Empty))
      && Patch.write (D.S (D.S (D.S D.Z)))
        (if bind then Heap.Cell (accumulator, Seg.append saved (Seg.append saved old))
         else Seg.append saved (Heap.Cell (accumulator, Seg.append saved old))) before_cells === Some after_cells
      && P.equal_prefix base before after && Bytes.drop before base === Some before_frame && Bytes.drop after base === Some after_frame
      && Wire.decode_cells (Heap.length before_cells) before_frame === Some (before_cells, tail)
      && Wire.decode_cells (Heap.length after_cells) after_frame === Some (after_cells, tail)} ->
    {u : unit | Copy.apply fragment.Lower.copies before base === Some after} @ ghost =
  fun signature bind next context schema fragment capacity max_pc saved old accumulator accumulator_start env_count saved_count old_count saved_start old_start before after base
      before_frame after_frame before_cells after_cells tail premise -> ghost_ (
    Geometry.matches_def signature (if bind then G.Bind next else G.Save_value next) capacity max_pc fragment.Lower.copies fragment.Lower.pc fragment.Lower.required;
    Geometry.size_represents (Codec.locals_size signature.G.locals) env_count ();
    Geometry.size_represents (Heap.length saved) saved_count (); Geometry.size_represents (Heap.length old) old_count ();
    Index.represents_def D.Z 0; Index.represents_def (D.S D.Z) 1; Index.represents_def (D.S (D.S D.Z)) 2;
    Index.represents_def (D.S (Codec.locals_size signature.G.locals)) (env_count + 1);
    Index.represents_def (D.S (D.S (Codec.locals_size signature.G.locals))) (env_count + 2);
    Hmc_u32_index_sum.correct (Codec.locals_size signature.G.locals) (Heap.length saved) env_count saved_count (env_count + saved_count) ();
    Index.represents_def (D.S (D.add (Codec.locals_size signature.G.locals) (Heap.length saved))) (env_count + saved_count + 1);
    Index.represents_def (D.S (D.S (D.add (Codec.locals_size signature.G.locals) (Heap.length saved)))) (env_count + saved_count + 2);
    Hmc_wasm_range_read.correct saved (2 + env_count) saved_count (D.S (D.S (Codec.locals_size signature.G.locals)))
      before_cells saved_start before base before_frame tail ();
    Hmc_wasm_range_read.correct old (2 + env_count + saved_count) old_count
      (D.S (D.S (D.add (Codec.locals_size signature.G.locals) (Heap.length saved)))) before_cells old_start before base before_frame tail ();
    let singleton = Heap.Cell (accumulator, Heap.Empty) in
    Heap.length_def singleton; Heap.length_def Heap.Empty;
    Hmc_wasm_range_read.correct singleton 1 1 (D.S D.Z) before_cells accumulator_start before base before_frame tail ();
    let first = if bind then singleton else saved in
    let second = if bind then saved else singleton in
    let first_count = if bind then 1 else saved_count in
    let second_count = if bind then saved_count else 1 in
    let source_first = if bind then 1 else 2 + env_count in
    let source_second = if bind then 2 + env_count else 1 in
    Hmc_frame_relayout_patch.associate first second (Seg.append saved old);
    Seg.append_def singleton (Seg.append saved old); Seg.append_def Heap.Empty (Seg.append saved old);
    Seg.append_def singleton (Seg.append saved (Seg.append saved old));
    Seg.append_def Heap.Empty (Seg.append saved (Seg.append saved old));
    Hmc_wasm_range_four.correct first second saved old fragment.Lower.copies source_first source_second (2 + env_count) (2 + env_count + saved_count) 2
      first_count second_count saved_count old_count (D.S (D.S D.Z)) before after base before_frame after_frame before_cells after_cells tail ())

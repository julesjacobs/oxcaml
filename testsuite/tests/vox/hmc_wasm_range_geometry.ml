module B = Wasm_u32
module D = Hm_declarative
module Heap = Hmc_heap_objects
module Wire = Hmc_heap_wire
module Seg = Hmc_frame_segments
module Patch = Hmc_cell_patch
module Index = Hmc_u32_index
module Lower = Hmc_wasm_relayout
module Copy = Wasm_parallel_copy
module Range = Hmc_wasm_range_copy
module P = Hmc_linear_preservation
module Bytes = Hmc_linear_bytes
module Splice = Wasm_memory_splice
module Geometry = Hmc_wasm_relayout_geometry
module Split = Hmc_wasm_range_split
let (correct @ total) : (first : Heap.cells) @ immutable -> (second : Heap.cells) @ immutable ->
    (plan : Copy.plan) @ immutable ->
    (source_first : Lower.count) -> (source_second : Lower.count) -> (destination_position : Lower.count) ->
    (first_count : Lower.count) -> (second_count : Lower.count) -> (destination : D.index) @ immutable ->
    (source : B.bytes) @ immutable -> (before : B.bytes) @ immutable -> (after : B.bytes) @ immutable -> (base : B.u32) ->
    (before_frame : B.bytes) @ immutable -> (after_frame : B.bytes) @ immutable ->
    (before_cells : Heap.cells) @ immutable -> (after_cells : Heap.cells) @ immutable -> (tail : B.bytes) @ immutable ->
    {u : unit | Index.represents (Heap.length first) first_count && Index.represents (Heap.length second) second_count
      && Index.represents destination destination_position
      && source_first + first_count <= 268435452 && source_second + second_count <= 268435452
      && destination_position + first_count + second_count <= 268435452
      && base + 16 + 16 * (destination_position + first_count + second_count) <= 4294967296
      && Geometry.two plan source_first destination_position (Heap.length first)
        source_second (destination_position + first_count) (Heap.length second)
      && Range.reads source base source_first first && Range.reads source base source_second second
      && source === before
      && Patch.write (D.S destination) (Seg.append first second) before_cells === Some after_cells
      && P.equal_prefix base before after && Bytes.drop before base === Some before_frame && Bytes.drop after base === Some after_frame
      && Wire.decode_cells (Heap.length before_cells) before_frame === Some (before_cells, tail)
      && Wire.decode_cells (Heap.length after_cells) after_frame === Some (after_cells, tail)} ->
    {u : unit | Copy.apply plan source base === Some after} @ ghost =
  fun first second plan source_first source_second destination_position first_count second_count destination
      source before after base before_frame after_frame before_cells after_cells tail premise -> ghost_ (
    Geometry.two_def plan source_first destination_position (Heap.length first) source_second (destination_position + first_count) (Heap.length second);
    match Geometry.split plan source_first destination_position (Heap.length first) with
    | None -> ()
    | Some second_plan ->
      Split.split_def plan source_first destination_position (Heap.length first);
      Split.split_def second_plan source_second (destination_position + first_count) (Heap.length second);
      Split.correct plan source_first destination_position (Heap.length first) second_plan;
      Split.correct second_plan source_second (destination_position + first_count) (Heap.length second) Copy.End;
      Copy.apply_def Copy.End source base;
      Hmc_wasm_range_pair.correct first second plan second_plan Copy.End source_first source_second destination_position
        first_count second_count destination source before after base before_frame after_frame before_cells after_cells tail ())

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
module Geometry = Hmc_wasm_relayout_geometry
module Split = Hmc_wasm_range_split
module P = Hmc_linear_preservation
module Bytes = Hmc_linear_bytes
module Splice = Wasm_memory_splice
let rec (length_append @ total) : (first : Heap.cells) @ immutable -> (second : Heap.cells) @ immutable ->
    {u : unit | Heap.length (Seg.append first second) === D.add (Heap.length first) (Heap.length second)} @ ghost =
  fun first second -> ghost_ (
    Seg.append_def first second; Heap.length_def first; Heap.length_def (Seg.append first second);
    D.add_def (Heap.length first) (Heap.length second);
    match first with Heap.Empty -> () | Heap.Cell (_, rest) -> length_append rest second)
let (correct @ total) : (first : Heap.cells) @ immutable -> (second : Heap.cells) @ immutable ->
    (third : Heap.cells) @ immutable -> (fourth : Heap.cells) @ immutable -> (plan : Copy.plan) @ immutable ->
    (source_first : Lower.count) -> (source_second : Lower.count) -> (source_third : Lower.count) -> (source_fourth : Lower.count) ->
    (destination_position : Lower.count) -> (first_count : Lower.count) -> (second_count : Lower.count) ->
    (third_count : Lower.count) -> (fourth_count : Lower.count) -> (destination : D.index) @ immutable ->
    (source : B.bytes) @ immutable -> (after : B.bytes) @ immutable -> (base : B.u32) ->
    (before_frame : B.bytes) @ immutable -> (after_frame : B.bytes) @ immutable ->
    (before_cells : Heap.cells) @ immutable -> (after_cells : Heap.cells) @ immutable -> (tail : B.bytes) @ immutable ->
    {u : unit | Index.represents (Heap.length first) first_count && Index.represents (Heap.length second) second_count
      && Index.represents (Heap.length third) third_count && Index.represents (Heap.length fourth) fourth_count
      && Index.represents destination destination_position
      && source_first + first_count <= 268435452 && source_second + second_count <= 268435452
      && source_third + third_count <= 268435452 && source_fourth + fourth_count <= 268435452
      && destination_position + first_count + second_count + third_count + fourth_count <= 268435452
      && base + 16 + 16 * (destination_position + first_count + second_count + third_count + fourth_count) <= 4294967296
      && Geometry.four plan source_first destination_position (Heap.length first)
        source_second (destination_position + first_count) (Heap.length second)
        source_third (destination_position + first_count + second_count) (Heap.length third)
        source_fourth (destination_position + first_count + second_count + third_count) (Heap.length fourth)
      && Range.reads source base source_first first && Range.reads source base source_second second
      && Range.reads source base source_third third && Range.reads source base source_fourth fourth
      && Patch.write (D.S destination) (Seg.append (Seg.append first second) (Seg.append third fourth)) before_cells === Some after_cells
      && P.equal_prefix base source after && Bytes.drop source base === Some before_frame && Bytes.drop after base === Some after_frame
      && Wire.decode_cells (Heap.length before_cells) before_frame === Some (before_cells, tail)
      && Wire.decode_cells (Heap.length after_cells) after_frame === Some (after_cells, tail)} ->
    {u : unit | Copy.apply plan source base === Some after} @ ghost =
  fun first second third fourth plan source_first source_second source_third source_fourth destination_position
      first_count second_count third_count fourth_count destination source after base before_frame after_frame before_cells after_cells tail premise -> ghost_ (
    Geometry.four_def plan source_first destination_position (Heap.length first)
      source_second (destination_position + first_count) (Heap.length second)
      source_third (destination_position + first_count + second_count) (Heap.length third)
      source_fourth (destination_position + first_count + second_count + third_count) (Heap.length fourth);
    match Geometry.split plan source_first destination_position (Heap.length first) with
    | None -> ()
    | Some second_plan -> (match Geometry.split second_plan source_second (destination_position + first_count) (Heap.length second) with
      | None -> ()
      | Some third_plan ->
        Split.split_def plan source_first destination_position (Heap.length first);
        Split.split_def second_plan source_second (destination_position + first_count) (Heap.length second);
        Split.correct plan source_first destination_position (Heap.length first) second_plan;
        Split.correct second_plan source_second (destination_position + first_count) (Heap.length second) third_plan;
        let pair = Seg.append first second in
        let last = Seg.append third fourth in
        length_append first second;
        Hmc_u32_index_sum.correct (Heap.length first) (Heap.length second) first_count second_count (first_count + second_count) ();
        Hmc_u32_index_sum.correct destination (Heap.length pair) destination_position (first_count + second_count)
          (destination_position + first_count + second_count) ();
        Hmc_cell_patch_algebra.append pair last (D.S destination) before_cells;
        D.add_def (D.S destination) (Heap.length pair);
        match Patch.write (D.add (D.S destination) (Heap.length pair)) last before_cells with
        | None -> ()
        | Some middle_cells ->
          let middle_frame = Wire.encode_cells middle_cells tail in
          let middle = Splice.replace source base before_frame middle_frame () in
          Hmc_wasm_range_geometry.correct third fourth third_plan source_third source_fourth (destination_position + first_count + second_count)
            third_count fourth_count (D.add destination (Heap.length pair)) source source middle base before_frame middle_frame before_cells middle_cells tail ();
          Splice.shared source middle after base ();
          Hmc_wasm_range_pair.correct first second plan second_plan third_plan source_first source_second destination_position
            first_count second_count destination source middle after base middle_frame after_frame middle_cells after_cells tail ()))

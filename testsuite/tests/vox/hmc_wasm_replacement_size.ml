module D = Hm_declarative
module G = Hmc_cfg_ir
module Heap = Hmc_heap_objects
module Seg = Hmc_frame_segments
module Patch = Hmc_frame_relayout_patch
module Index = Hmc_u32_index
module Lower = Hmc_wasm_relayout
let (correct @ total) : (instruction : G.instruction) @ immutable -> (saved : Heap.cells) @ immutable -> (old : Heap.cells) @ immutable ->
    (accumulator : Hmc_tagged_cell.value) @ immutable -> (values : Heap.cells) @ immutable ->
    (saved_count : Lower.count) -> (old_count : Lower.count) -> (required : Lower.count) ->
    {u : unit | Patch.replacement instruction saved accumulator old === Some values
      && Index.represents (Heap.length saved) saved_count && Index.represents (Heap.length old) old_count
      && required = (match instruction with G.Restore _ -> 2 + saved_count + old_count | _ -> 3 + 2 * saved_count + old_count)} ->
    {u : unit | Index.represents (D.S (D.S (Heap.length values))) required} @ ghost =
  fun instruction saved old accumulator values saved_count old_count required premise -> ghost_ (
    Patch.replacement_def instruction saved accumulator old;
    Hmc_wasm_range_four.length_append saved old;
    Hmc_u32_index_sum.correct (Heap.length saved) (Heap.length old) saved_count old_count (saved_count + old_count) ();
    (match instruction with
    | G.Restore _ -> ()
    | G.Save_value _ ->
      let rest = Heap.Cell (accumulator, Seg.append saved old) in
      Heap.length_def rest;
      Index.represents_def (Heap.length rest) (1 + saved_count + old_count);
      Hmc_wasm_range_four.length_append saved rest;
      Hmc_u32_index_sum.correct (Heap.length saved) (Heap.length rest) saved_count (1 + saved_count + old_count) (1 + 2 * saved_count + old_count) ()
    | G.Bind _ ->
      Hmc_wasm_range_four.length_append saved (Seg.append saved old);
      Hmc_u32_index_sum.correct (Heap.length saved) (Heap.length (Seg.append saved old)) saved_count (saved_count + old_count) (2 * saved_count + old_count) ();
      Heap.length_def values; Index.represents_def (Heap.length values) (1 + 2 * saved_count + old_count)
    | _ -> ());
    Index.represents_def (D.S (Heap.length values)) (required - 1);
    Index.represents_def (D.S (D.S (Heap.length values))) required)

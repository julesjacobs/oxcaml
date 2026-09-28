module B = Wasm_u32
module D = Hm_declarative
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module Seg = Hmc_frame_segments
module Wire = Hmc_heap_wire
module Index = Hmc_u32_index
module P = Hmc_linear_preservation
module Bytes = Hmc_linear_bytes
module M = Wasm_memory
module S = Wasm_scalar
module Copy = Wasm_parallel_copy
module Lower = Hmc_wasm_relayout
module Patch = Hmc_cell_patch
module Splice = Wasm_memory_splice
let[@def] (tag @ total) (position : Lower.count) : B.u32 = 16 + 16 * position
let[@def] (payload @ total) (position : Lower.count) : B.u32 = 24 + 16 * position
let[@def] rec (reads @ total) (memory : B.bytes @ immutable) (base : B.u32) (position : Lower.count) (values : Heap.cells @ immutable) = ghost_ (
  match values with
  | Heap.Empty -> true
  | Heap.Cell (value, rest) -> if position < 268435452 then
    let next : Lower.count = position + 1 in
    M.load memory base (tag position) M.W64 === Some (S.I64 (V.tag value))
    && M.load memory base (payload position) M.W64 === Some (S.I64 (V.payload value)) && reads memory base next rest
    else false)
let rec (unique @ total) : (before : B.bytes) @ immutable -> (after : B.bytes) @ immutable ->
    (base : B.u32) -> (frame : B.bytes) @ immutable ->
    {u : unit | P.equal_prefix base before after && Bytes.drop before base === Some frame && Bytes.drop after base === Some frame} ->
    {u : unit | before === after} @ ghost = fun before after base frame premise -> ghost_ (
    P.equal_prefix_def base before after; Bytes.drop_def before base; Bytes.drop_def after base;
    if base = 0 then () else match before, after with
    | B.Byte (_, a), B.Byte (_, b) -> unique a b (base - 1) frame () | _ -> ())
let rec (correct @ total) : (values : Heap.cells) @ immutable -> (plan : Copy.plan) @ immutable -> (tail_plan : Copy.plan) @ immutable ->
    (source_position : Lower.count) -> (destination_position : Lower.count) -> (count : Lower.count) -> (destination : D.index) @ immutable ->
    (source : B.bytes) @ immutable -> (before : B.bytes) @ immutable -> (after : B.bytes) @ immutable -> (base : B.u32) ->
    (before_frame : B.bytes) @ immutable -> (after_frame : B.bytes) @ immutable ->
    (before_cells : Heap.cells) @ immutable -> (after_cells : Heap.cells) @ immutable -> (tail : B.bytes) @ immutable ->
    {u : unit | Index.represents (Heap.length values) count && Index.represents destination destination_position
      && source_position + count <= 268435452 && destination_position + count <= 268435452
      && base + 16 + 16 * (destination_position + count) <= 4294967296
      && Lower.range_is plan source_position destination_position (Heap.length values) tail_plan
      && reads source base source_position values && Copy.apply tail_plan source base === Some before
      && Patch.write (D.S destination) values before_cells === Some after_cells
      && P.equal_prefix base before after && Bytes.drop before base === Some before_frame && Bytes.drop after base === Some after_frame
      && Wire.decode_cells (Heap.length before_cells) before_frame === Some (before_cells, tail)
      && Wire.decode_cells (Heap.length after_cells) after_frame === Some (after_cells, tail)} ->
    {u : unit | Copy.apply plan source base === Some after} @ ghost =
  fun values plan tail_plan source_position destination_position count destination source before after base before_frame after_frame before_cells after_cells tail premise -> ghost_ (
    Heap.length_def values; Index.represents_def (Heap.length values) count;
    Lower.range_is_def plan source_position destination_position (Heap.length values) tail_plan;
    reads_def source base source_position values;
    match values with
    | Heap.Empty ->
      Patch.write_def (D.S destination) values before_cells;
      Hmc_wasm_wire_words.cells_unique (Heap.length before_cells) before_frame after_frame before_cells tail ();
      unique before after base before_frame ()
    | Heap.Cell (value, rest) ->
      Patch.cons (D.S destination) value rest before_cells;
      (match Patch.write (D.S (D.S destination)) rest before_cells with
      | None -> ()
      | Some middle_cells ->
        let split = Patch.split (D.S destination) middle_cells value after_cells () in
        let middle_frame = Wire.encode_cells middle_cells tail in
        let middle = Splice.replace before base before_frame middle_frame () in
        (match plan with
        | Copy.Copy (_, _, Copy.Copy (_, _, remaining)) ->
          Index.represents_def (D.S destination) (destination_position + 1);
          correct rest remaining tail_plan (source_position + 1) (destination_position + 1) (count - 1) (D.S destination)
            source before middle base before_frame middle_frame before_cells middle_cells tail ();
          Splice.shared before middle after base ();
          tag_def source_position; payload_def source_position;
          let _intermediate = Hmc_wasm_cell_update.correct split.Patch.prefix split.Patch.rest split.Patch.old value (destination_position + 1)
            middle after base (16 + 16 * destination_position) (24 + 16 * destination_position) middle_frame after_frame tail
            source (tag source_position) (payload source_position) remaining () in ()
        | _ -> ())))

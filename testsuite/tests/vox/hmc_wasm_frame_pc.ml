module B = Wasm_u32
module W = Hmc_word64
module D = Hm_declarative
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module Wire = Hmc_heap_wire
module Header = Hmc_wasm_header_update
module Bytes = Hmc_linear_bytes
module M = Wasm_memory
module S = Wasm_scalar
let (correct @ total) : (memory : B.bytes) @ immutable -> (base : B.u32) -> (frame : B.bytes) @ immutable ->
    (pc : W.limb) -> (cells : Heap.cells) @ immutable -> (tail : B.bytes) @ immutable ->
    {u : unit | base <= 4294967280 && Bytes.drop memory base === Some frame
      && Wire.decode_cells (D.S (Heap.length cells)) frame === Some (Heap.Cell (V.Word (Header.number pc), cells), tail)} ->
    {u : unit | M.load memory base (Hmc_wasm_pc_update.offset ()) M.W64 === Some (S.I64 (Header.number pc))} @ ghost =
  fun memory base frame pc cells tail premise -> ghost_ (
    Wire.decode_cells_def (D.S (Heap.length cells)) frame;
    match V.decode frame with
    | None -> ()
    | Some (_, rest) ->
      Wasm_cell.cell memory base frame (V.Word (Header.number pc)) rest ();
      Wasm_cell.payload_def memory base; V.payload_def (V.Word (Header.number pc)); Hmc_wasm_pc_update.offset_def ())

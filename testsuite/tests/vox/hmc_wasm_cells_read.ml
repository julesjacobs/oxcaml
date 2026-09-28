module B = Wasm_u32
module W = Hmc_word64
module D = Hm_declarative
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module Wire = Hmc_heap_wire
module L = Hmc_linear_bytes
module M = Wasm_memory
module S = Wasm_scalar
let (correct @ total) : (memory : B.bytes) @ immutable -> (base : B.u32) -> (bytes : B.bytes) @ immutable ->
    (count : D.index) @ immutable -> (cells : Heap.cells) @ immutable -> (tail : B.bytes) @ immutable ->
    (index : D.index) @ immutable -> (number : W.limb) -> (offset : B.u32) -> (payload_offset : B.u32) -> (value : V.value) @ immutable ->
    {u : unit | offset = 16 * number && payload_offset = offset + 8 && base + offset <= 4294967280
      && L.drop memory base === Some bytes && Hmc_u32_index.represents index number
      && Hmc_heap_simple.lookup cells index === Some value && Wire.decode_cells count bytes === Some (cells, tail)} ->
    {u : unit | M.load memory base offset M.W64 === Some (S.I64 (V.tag value))
      && M.load memory base payload_offset M.W64 === Some (S.I64 (V.payload value))} @ ghost =
  fun memory base bytes count cells tail index number offset payload_offset value premise -> ghost_ (
    let suffix = Hmc_wasm_environment_lookup.find count bytes cells tail index number offset value () in
    Wasm_cell.shift memory base offset (base + offset) bytes ();
    match V.decode suffix with
    | None -> ()
    | Some (_, rest) ->
      Wasm_cell.cell memory (base + offset) suffix value rest ();
      Wasm_cell.tag_def memory (base + offset); Wasm_cell.payload_def memory (base + offset);
      M.load_def memory base offset M.W64; M.load_def memory base payload_offset M.W64;
      M.load_def memory (base + offset) 0 M.W64; M.load_def memory (base + offset) 8 M.W64;
      M.address_def base offset M.W64; M.address_def base payload_offset M.W64;
      M.address_def (base + offset) 0 M.W64; M.address_def (base + offset) 8 M.W64; M.size_def M.W64)

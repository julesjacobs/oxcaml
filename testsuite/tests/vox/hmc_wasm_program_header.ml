module B = Wasm_u32
module D = Hm_declarative
module H = Hmc_heap_objects
module V = Hmc_tagged_cell
module W = Hmc_word64
module Wire = Hmc_heap_wire
module Bytes = Hmc_linear_bytes
module Header = Hmc_wasm_header_update
module Codec = Wasm_word_memory
module Prefix = Hmc_memory_prefix
module Cell = Wasm_cell
module M = Wasm_memory
module S = Wasm_scalar
module Dispatch = Hmc_wasm_program_dispatch
let (correct @ total) : (memory : B.bytes) @ immutable -> (base : B.u32) -> (bytes : B.bytes) @ immutable ->
    (pc : B.u32) -> (cells : H.cells) @ immutable -> (suffix : B.bytes) @ immutable ->
    {u : unit | base <= 4294967280 && Bytes.drop memory base === Some bytes
      && Wire.decode_cells (D.S (H.length cells)) bytes === Some (H.Cell (V.Word (Header.number pc), cells), suffix)} ->
    {u : unit | M.load memory base (Dispatch.pc_offset ()) M.W32 === Some (S.I32 pc)} @ ghost =
  fun memory base bytes pc cells suffix premise -> ghost_ (
    Wire.decode_cells_def (D.S (H.length cells)) bytes; V.decode_def bytes; Header.number_def pc;
    Dispatch.pc_offset_def (); M.load_def memory base 8 M.W32; M.address_def base 8 M.W32;
    M.size_def M.W32; M.count_def M.W32;
    match Codec.decode bytes with
    | None -> ()
    | Some (tag, middle) ->
      Cell.word_suffix bytes tag middle (); Cell.eight_def (); Cell.shift memory base 8 (base + 8) bytes ();
      Codec.decode_def middle;
      (match Codec.decode_limb middle with
      | None -> ()
      | Some (lo, rest) ->
        Bytes.take_def D.Z rest;
        let prefix = Prefix.limb middle D.Z lo rest B.End () in
        Bytes.load_def memory (base + 8) (V.four D.Z);
        M.decode_def M.W32 prefix))

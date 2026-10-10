module D = Hm_declarative
module B = Wasm_u32
module W = Hmc_word64
module V = Hmc_tagged_cell
module H = Hmc_heap_objects
module F = Hmc_heap_frame
module G = Hmc_cfg_ir
module Wire = Hmc_heap_wire
module Codec = Hmc_pointer_frame_codec
module L = Hmc_linear_bytes
module S = Wasm_scalar
module M = Wasm_memory
module Cell = Wasm_cell
module Suffix = Wasm_cell_suffix

let[@def] (number @ total) (code : W.limb) : W.t @ immutable = {W.lo = code; hi = 0}
let[@def] (pc @ total) (memory : B.bytes @ immutable) (base : B.u32) = M.load memory base 8 M.W64
let[@def] (current_tag @ total) (memory : B.bytes @ immutable) (base : B.u32) = M.load memory base 16 M.W64
let[@def] (current_payload @ total) (memory : B.bytes @ immutable) (base : B.u32) = M.load memory base 24 M.W64
let[@def] (accumulator_tag @ total) (memory : B.bytes @ immutable) (base : B.u32) = M.load memory base 32 M.W64
let[@def] (accumulator_payload @ total) (memory : B.bytes @ immutable) (base : B.u32) = M.load memory base 40 M.W64

let (correct @ total) : (memory : B.bytes) @ immutable -> (base : B.u32) -> (bytes : B.bytes) @ immutable ->
    (capacity : D.index) @ immutable -> (code : W.limb) -> (cells : H.cells) @ immutable -> (tail : B.bytes) @ immutable ->
    (signature : G.signature) @ immutable -> (activation : F.activation) @ immutable -> (padding : H.cells) @ immutable ->
    {u : unit | base <= 4294967247 && L.drop memory base === Some bytes
      && Wire.decode (Wire.Closure_schema capacity) bytes === Some (Wire.Closure (code, cells), tail)
      && Codec.decode signature activation.F.pc cells === Some (activation, padding)} ->
    {u : unit | pc memory base === Some (S.I64 (number code))
      && current_tag memory base === Some (S.I64 (V.tag activation.F.current))
      && current_payload memory base === Some (S.I64 (V.payload activation.F.current))
      && accumulator_tag memory base === Some (S.I64 (V.tag activation.F.accumulator))
      && accumulator_payload memory base === Some (S.I64 (V.payload activation.F.accumulator))} @ ghost =
  fun memory base bytes capacity code cells tail signature activation padding premise -> ghost_ (
    number_def code; Wire.decode_def (Wire.Closure_schema capacity) bytes; Codec.decode_def signature activation.F.pc cells;
    match V.decode bytes with
    | Some (V.Word word, after_header) ->
      Cell.cell memory base bytes (V.Word word) after_header ();
      Cell.payload_def memory base; V.payload_def (V.Word word); pc_def memory base;
      let current_address : B.u32 = base + 16 in
      let accumulator_address : B.u32 = base + 32 in
      Suffix.suffix memory base bytes (V.Word word) after_header current_address ();
      Wire.decode_cells_def capacity after_header;
      (match capacity with
      | D.Z -> ()
      | D.S rest -> (match V.decode after_header with
        | None -> ()
        | Some (current, after_current) ->
          Cell.cell memory current_address after_header current after_current ();
          Suffix.suffix memory current_address after_header current after_current accumulator_address ();
          Wire.decode_cells_def rest after_current;
          (match rest with
          | D.Z -> ()
          | D.S remaining -> (match V.decode after_current with
            | None -> ()
            | Some (accumulator, after_accumulator) ->
              Cell.cell memory accumulator_address after_current accumulator after_accumulator ();
              current_tag_def memory base; current_payload_def memory base;
              accumulator_tag_def memory base; accumulator_payload_def memory base;
              Cell.tag_def memory current_address; Cell.payload_def memory current_address;
              Cell.tag_def memory accumulator_address; Cell.payload_def memory accumulator_address;
              M.load_def memory base 16 M.W64; M.load_def memory base 24 M.W64;
              M.load_def memory base 32 M.W64; M.load_def memory base 40 M.W64;
              M.load_def memory current_address 0 M.W64; M.load_def memory current_address 8 M.W64;
              M.load_def memory accumulator_address 0 M.W64; M.load_def memory accumulator_address 8 M.W64;
              M.address_def base 16 M.W64; M.address_def base 24 M.W64;
              M.address_def base 32 M.W64; M.address_def base 40 M.W64;
              M.address_def current_address 0 M.W64; M.address_def current_address 8 M.W64;
              M.address_def accumulator_address 0 M.W64; M.address_def accumulator_address 8 M.W64;
              M.size_def M.W64))))
    | _ -> ())

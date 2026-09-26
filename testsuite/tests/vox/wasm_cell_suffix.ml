module B = Wasm_u32
module C = Hmc_tagged_cell
module L = Hmc_linear_bytes
module Codec = Wasm_word_memory
module Cell = Wasm_cell
let[@def] (sixteen @ total) (unit : unit) : B.u32 = 16
let (suffix @ total) : (memory : B.bytes) @ immutable -> (base : B.u32) ->
    (bytes : B.bytes) @ immutable -> (value : C.value) @ immutable -> (tail : B.bytes) @ immutable ->
    (next : B.u32) ->
    {u : unit | next = base + 16 && L.drop memory base === Some bytes && C.decode bytes === Some (value, tail)} ->
    {u : unit | L.drop memory next === Some tail} @ ghost = fun memory base bytes value tail next premise -> ghost_ (
  C.decode_def bytes; sixteen_def (); Cell.eight_def ();
  match Codec.decode bytes with
  | None -> ()
  | Some (tag, middle) ->
    Cell.word_suffix bytes tag middle ();
    (match Codec.decode middle with
    | None -> ()
    | Some (payload, rest) ->
      Cell.word_suffix middle payload rest ();
      Cell.shift bytes 8 8 16 middle ();
      Cell.shift memory base 16 next bytes ()))

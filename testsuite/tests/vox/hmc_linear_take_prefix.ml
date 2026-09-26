module B = Wasm_u32
module D = Hm_declarative
module L = Hmc_linear_bytes
module P = Hmc_linear_preservation
module Index = Hmc_u32_index
let rec (correct @ total) : (count : D.index) @ immutable -> (width : B.u32) ->
    (bytes : B.bytes) @ immutable -> (prefix : B.bytes) @ immutable ->
    {u : unit | Index.represents count width && L.take count bytes === Some prefix} ->
    {u : unit | P.equal_prefix width prefix bytes} @ ghost = fun count width bytes prefix premise -> ghost_ (
    Index.represents_def count width; L.take_def count bytes; P.equal_prefix_def width prefix bytes;
    match count, bytes, prefix with
    | D.S rest, B.Byte (_, tail), B.Byte (_, remaining) -> correct rest (width - 1) tail remaining ()
    | _ -> ())

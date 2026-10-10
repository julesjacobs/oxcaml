module B = Wasm_u32
module W = Hmc_word64
module L = Hmc_linear_bytes
module Bounds = Hmc_linear_bounds
module Codec = Wasm_word_memory
type width = {n : B.u32 | n >= 8}
let[@def] (remaining @ total) (width : width) : B.u32 = width - 8
type result = {word : W.t; tail : B.bytes}
let (read @ total) : (input : B.bytes) @ immutable -> (width : width) ->
    {u : unit | Bounds.covers input width} ->
    {out : result | Codec.decode input === Some (out.word, out.tail) && Bounds.covers out.tail (remaining width)} @ immutable =
  fun input width premise ->
    ghost_ (Bounds.covers_def input width; remaining_def width);
    ghost_ (L.drop_def input (width - 0));
    match input with B.End -> unreachable_ () | B.Byte (a0, b1) ->
    ghost_ (L.drop_def b1 (width - 1));
    match b1 with B.End -> unreachable_ () | B.Byte (a1, b2) ->
    ghost_ (L.drop_def b2 (width - 2));
    match b2 with B.End -> unreachable_ () | B.Byte (a2, b3) ->
    ghost_ (L.drop_def b3 (width - 3));
    match b3 with B.End -> unreachable_ () | B.Byte (a3, b4) ->
    ghost_ (L.drop_def b4 (width - 4));
    match b4 with B.End -> unreachable_ () | B.Byte (a4, b5) ->
    ghost_ (L.drop_def b5 (width - 5));
    match b5 with B.End -> unreachable_ () | B.Byte (a5, b6) ->
    ghost_ (L.drop_def b6 (width - 6));
    match b6 with B.End -> unreachable_ () | B.Byte (a6, b7) ->
    ghost_ (L.drop_def b7 (width - 7));
    match b7 with B.End -> unreachable_ () | B.Byte (a7, b8) ->
    let word = {W.lo = a0 + 256 * a1 + 65536 * a2 + 16777216 * a3;
      hi = a4 + 256 * a5 + 65536 * a6 + 16777216 * a7} in
    ghost_ (Codec.decode_def input; Codec.decode_limb_def input; Codec.decode_limb_def b4;
      Bounds.covers_def b8 (width - 8));
    {word; tail = b8}

module B = Wasm_u32
module D = Hm_declarative
module G = Hmc_cfg_ir
module H = Hmc_heap_objects
module F = Hmc_heap_frame
module Index = Hmc_u32_index
module Codec = Hmc_pointer_frame_codec
let (extent @ total) : (cells : H.cells) @ immutable -> (capacity : B.u32) -> (count : B.u32) ->
    (base : B.u32) -> (frame_end : B.u32) ->
    {u : unit | Index.represents (H.length cells) capacity && Index.represents (D.S (H.length cells)) count
      && frame_end = base + 16 * count} ->
    {u : unit | frame_end = base + 16 + 16 * capacity} @ ghost =
  fun cells capacity count base frame_end premise -> ghost_ (
    Index.represents_def (D.S (H.length cells)) count; Index.unique (H.length cells) capacity (count - 1) ())
let (decode_signature @ total) : (before : G.signature) @ immutable -> (after : G.signature) @ immutable ->
    (activation : F.activation) @ immutable -> (cells : H.cells) @ immutable -> (padding : H.cells) @ immutable ->
    {u : unit | before.G.locals === after.G.locals && before.G.temporaries === after.G.temporaries
      && Codec.decode before activation.F.pc cells === Some (activation, padding)} ->
    {u : unit | Codec.decode after activation.F.pc cells === Some (activation, padding)} @ ghost =
  fun before after activation cells padding premise -> ghost_ (
    Codec.decode_def before activation.F.pc cells; Codec.decode_def after activation.F.pc cells)

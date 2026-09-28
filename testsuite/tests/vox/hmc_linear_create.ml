module D = Hm_declarative
module W = Hmc_word64
module B = Wasm_u32
module C = Hmc_tagged_cell
module I = Hmc_u32_index
module L = Hmc_linear_bytes
module Bounds = Hmc_linear_bounds

let rec (zeroed @ total) : (size : D.index) @ immutable -> (width : W.limb) ->
    {u : unit | I.represents size width} ->
    {out : B.bytes | C.length out === size && L.drop out width === Some B.End && Bounds.covers out width} @ immutable = fun size width premise ->
  ghost_ (I.represents_def size width);
  match size with
  | D.Z -> ghost_ (C.length_def B.End; L.drop_def B.End width; Bounds.covers_def B.End width); B.End
  | D.S rest ->
    let tail = zeroed rest (width - 1) () in
    let out = B.Byte (0, tail) in
    ghost_ (C.length_def out; L.drop_def out width; Bounds.covers_def out width); out

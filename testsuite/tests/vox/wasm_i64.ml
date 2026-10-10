module B = Wasm_u32
module W = Hmc_word64
let[@def] (decode @ total) (input : B.bytes @ immutable) : (W.t * B.bytes) option @ immutable =
  match input with
  | B.Byte (b0, B.Byte (b1, B.Byte (b2, B.Byte (b3, B.Byte (b4, B.Byte (b5, B.Byte (b6, B.Byte (b7, B.Byte (b8, B.Byte (b9, tail)))))))))) ->
    if b0 >= 128 && b1 >= 128 && b2 >= 128 && b3 >= 128 && b4 >= 128 && b5 >= 128 && b6 >= 128 && b7 >= 128 && b8 >= 128
      && (b9 = 0 || b9 = 127) then
      let middle = b4 - 128 in
      let lo = b0 - 128 + 128 * (b1 - 128) + 16384 * (b2 - 128) + 2097152 * (b3 - 128) + 268435456 * B.remainder middle 16 in
      let hi = B.divide middle 16 + 8 * (b5 - 128) + 1024 * (b6 - 128) + 131072 * (b7 - 128) + 16777216 * (b8 - 128)
        + (if b9 = 0 then 0 else 2147483648) in
      Some ({W.lo; hi}, tail)
    else None
  | _ -> None
type low_parts = {b0 : B.byte; b1 : B.byte; b2 : B.byte; b3 : B.byte; upper : B.byte}
type high_parts = {lower : B.byte; b5 : B.byte; b6 : B.byte; b7 : B.byte; b8 : B.byte; sign : B.byte}
let (split_low @ total) : (number : W.limb) ->
    {out : low_parts | out.b0 >= 128 && out.b1 >= 128 && out.b2 >= 128 && out.b3 >= 128 && out.upper < 16
      && number = out.b0 - 128 + 128 * (out.b1 - 128) + 16384 * (out.b2 - 128) + 2097152 * (out.b3 - 128) + 268435456 * out.upper} @ immutable = fun number ->
  let b0 = B.remainder number 128 + 128 in
  let q1 = B.divide number 128 in
  let b1 = B.remainder q1 128 + 128 in
  let q2 = B.divide q1 128 in
  let b2 = B.remainder q2 128 + 128 in
  let q3 = B.divide q2 128 in
  let b3 = B.remainder q3 128 + 128 in
  let upper = B.divide q3 128 in
  {b0; b1; b2; b3; upper}
let (split_high @ total) : (number : W.limb) ->
    {out : high_parts | out.lower < 8 && out.b5 >= 128 && out.b6 >= 128 && out.b7 >= 128 && out.b8 >= 128
      && (out.sign = 0 || out.sign = 127)
      && number = out.lower + 8 * (out.b5 - 128) + 1024 * (out.b6 - 128) + 131072 * (out.b7 - 128) + 16777216 * (out.b8 - 128)
        + (if out.sign = 0 then 0 else 2147483648)} @ immutable = fun number ->
  let lower = B.remainder number 8 in
  let q5 = B.divide number 8 in
  let b5 = B.remainder q5 128 + 128 in
  let q6 = B.divide q5 128 in
  let b6 = B.remainder q6 128 + 128 in
  let q7 = B.divide q6 128 in
  let b7 = B.remainder q7 128 + 128 in
  let q8 = B.divide q7 128 in
  let b8 = B.remainder q8 128 + 128 in
  let high = B.divide q8 128 in
  let sign = if high = 0 then 0 else 127 in
  {lower; b5; b6; b7; b8; sign}
let (encode @ total) (word : W.t @ immutable) (tail : B.bytes @ immutable) :
    {out : B.bytes | decode out === Some (word, tail)} @ immutable =
  let low = split_low word.W.lo in
  let high = split_high word.W.hi in
  let b4 = low.upper + 16 * high.lower + 128 in
  let out = B.Byte (low.b0, B.Byte (low.b1, B.Byte (low.b2, B.Byte (low.b3, B.Byte (b4,
    B.Byte (high.b5, B.Byte (high.b6, B.Byte (high.b7, B.Byte (high.b8, B.Byte (high.sign, tail)))))))))) in
  ghost_ (decode_def out); out

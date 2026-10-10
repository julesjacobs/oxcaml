module B = Wasm_u32
let[@def] (decode @ total) (input : B.bytes @ immutable) : (B.u32 * B.bytes) option @ immutable =
  match input with
  | B.Byte (b0, B.Byte (b1, B.Byte (b2, B.Byte (b3, B.Byte (b4, tail))))) ->
    if b0 >= 128 && b1 >= 128 && b2 >= 128 && b3 >= 128 && (b4 < 8 || (120 <= b4 && b4 < 128)) then
      let high = if b4 < 8 then b4 else b4 - 112 in
      Some (b0 - 128 + 128 * (b1 - 128) + 16384 * (b2 - 128) + 2097152 * (b3 - 128) + 268435456 * high, tail)
    else None
  | _ -> None
let (encode @ total) (n : B.u32) (tail : B.bytes @ immutable) :
    {out : B.bytes | decode out === Some (n, tail)} @ immutable =
  let b0 = B.remainder n 128 + 128 in
  let q1 = B.divide n 128 in
  let b1 = B.remainder q1 128 + 128 in
  let q2 = B.divide q1 128 in
  let b2 = B.remainder q2 128 + 128 in
  let q3 = B.divide q2 128 in
  let b3 = B.remainder q3 128 + 128 in
  let high = B.divide q3 128 in
  let b4 = if high < 8 then high else high + 112 in
  let out = B.Byte (b0, B.Byte (b1, B.Byte (b2, B.Byte (b3, B.Byte (b4, tail))))) in
  ghost_ (decode_def out); out

open Wasm_u32

let[@def] (decode_limb @ total) (input : bytes @ immutable) :
    (Hmc_word64.limb * bytes) option @ immutable =
  match input with
  | Byte (a, Byte (b, Byte (c, Byte (d, tail)))) ->
    Some (a + 256 * b + 65536 * c + 16777216 * d, tail)
  | _ -> None

let (encode_limb @ total) (n : Hmc_word64.limb) (tail : bytes @ immutable) :
    {out : bytes | decode_limb out === Some (n, tail)} @ immutable =
  let a = remainder n 256 in let q = divide n 256 in
  let b = remainder q 256 in let q = divide q 256 in
  let c = remainder q 256 in let d = divide q 256 in
  let out = Byte (a, Byte (b, Byte (c, Byte (d, tail)))) in
  ghost_ (decode_limb_def out);
  out

let[@def] (decode @ total) (input : bytes @ immutable) :
    (Hmc_word64.t * bytes) option @ immutable =
  match decode_limb input with
  | None -> None
  | Some (lo, rest) ->
    match decode_limb rest with
    | None -> None
    | Some (hi, tail) -> Some ({Hmc_word64.lo; hi}, tail)

let (encode @ total) (word : Hmc_word64.t @ immutable) (tail : bytes @ immutable) :
    {out : bytes | decode out === Some (word, tail)} @ immutable =
  let high = encode_limb word.hi tail in
  let out = encode_limb word.lo high in
  ghost_ (decode_def out);
  out

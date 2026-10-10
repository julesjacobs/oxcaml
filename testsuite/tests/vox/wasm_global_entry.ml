module B = Wasm_u32
module I = Wasm_instruction
module S = Wasm_scalar

type t = {mutable_ : bool; value : S.value}
let[@def] (decode @ total) (bytes : B.bytes @ immutable) : (t * B.bytes) option @ immutable =
  match bytes with
  | B.Byte (ty, B.Byte (mutability, rest)) ->
    if mutability > 1 then None else
      (match I.decode rest with
      | Some (instruction, end_bytes) -> (match I.decode end_bytes with
        | Some (I.Plain I.End, tail) ->
          (match ty, instruction with
          | 127, I.I32_const value -> Some ({mutable_ = mutability = 1; value = S.I32 value}, tail)
          | 126, I.I64_const value -> Some ({mutable_ = mutability = 1; value = S.I64 value}, tail)
          | _ -> None)
        | _ -> None)
      | None -> None)
  | _ -> None
let (encode @ total) : (entry : t) @ immutable -> (tail : B.bytes) @ immutable ->
    {bytes : B.bytes | decode bytes === Some (entry, tail)} @ immutable = fun entry tail ->
  let end_bytes = I.encode (I.Plain I.End) tail in
  let mutability = if entry.mutable_ then 1 else 0 in
  let ty, instruction = match entry.value with
    | S.I32 value -> 127, I.I32_const value
    | S.I64 value -> 126, I.I64_const value in
  let bytes = B.Byte (ty, B.Byte (mutability, I.encode instruction end_bytes)) in
  ghost_ (decode_def bytes); bytes

module B = Wasm_u32
module F = Wasm_functions
let[@def] rec (count @ total) (types : F.signatures @ immutable) : B.u32 option =
  match types with
  | F.No_signatures -> Some 0
  | F.Signature (_, rest) -> match count rest with
    | None -> None | Some n -> if n = 4294967295 then None else Some (n + 1)
let[@def] rec (decode_entries @ total) (number : B.u32) (bytes : B.bytes @ immutable) : (F.signatures * B.bytes) option @ immutable =
  if number = 0 then Some (F.No_signatures, bytes) else
  match bytes with
  | B.Byte (96, B.Byte (0, B.Byte (0, rest))) ->
    (match decode_entries (number - 1) rest with None -> None | Some (types, tail) -> Some (F.Signature (F.Void, types), tail))
  | B.Byte (96, B.Byte (0, B.Byte (1, B.Byte (127, rest)))) ->
    (match decode_entries (number - 1) rest with None -> None | Some (types, tail) -> Some (F.Signature (F.I32, types), tail))
  | B.Byte (96, B.Byte (0, B.Byte (1, B.Byte (126, rest)))) ->
    (match decode_entries (number - 1) rest with None -> None | Some (types, tail) -> Some (F.Signature (F.I64, types), tail))
  | _ -> None
let[@def] (decode @ total) (bytes : B.bytes @ immutable) =
  match B.decode_5 bytes with None -> None | Some (number, rest) -> decode_entries number rest
let rec (encode_entries @ total) : (types : F.signatures) @ immutable -> (tail : B.bytes) @ immutable -> (number : B.u32) ->
    {u : unit | count types === Some number} ->
    {bytes : B.bytes | decode_entries number bytes === Some (types, tail)} @ immutable =
  fun types tail number premise ->
    ghost_ (count_def types);
    match types with
    | F.No_signatures -> ghost_ (decode_entries_def number tail); tail
    | F.Signature (result, rest) ->
      let suffix = encode_entries rest tail (number - 1) () in
      let bytes = match result with
        | F.Void -> B.Byte (96, B.Byte (0, B.Byte (0, suffix)))
        | F.I32 -> B.Byte (96, B.Byte (0, B.Byte (1, B.Byte (127, suffix))))
        | F.I64 -> B.Byte (96, B.Byte (0, B.Byte (1, B.Byte (126, suffix)))) in
      ghost_ (decode_entries_def number bytes); bytes
let (encode @ total) : (types : F.signatures) @ immutable -> (tail : B.bytes) @ immutable ->
    {out : B.bytes option | match out with None -> count types === None | Some bytes -> not (count types === None) && decode bytes === Some (types, tail)} @ immutable =
  fun types tail -> match count types with
  | None -> None
  | Some number ->
    let bytes = B.encode_u32 number (encode_entries types tail number ()) in
    ghost_ (decode_def bytes); Some bytes

module B = Wasm_u32
module F = Wasm_functions
let[@def] (decode @ total) (bytes : B.bytes @ immutable) =
  match bytes with
  | B.Byte (96, parameters) -> (match B.decode_5 parameters with
    | Some (0, results) -> (match B.decode_5 results with
      | Some (0, tail) -> Some (F.Void, tail)
      | Some (1, B.Byte (127, tail)) -> Some (F.I32, tail)
      | Some (1, B.Byte (126, tail)) -> Some (F.I64, tail)
      | _ -> None)
    | _ -> None)
  | _ -> None
let (encode @ total) : (result : F.result_type) @ immutable -> (tail : B.bytes) @ immutable ->
    {bytes : B.bytes | decode bytes === Some (result, tail)} @ immutable = fun result tail ->
  let results = match result with
    | F.Void -> B.encode_u32 0 tail
    | F.I32 -> B.encode_u32 1 (B.Byte (127, tail))
    | F.I64 -> B.encode_u32 1 (B.Byte (126, tail)) in
  let bytes = B.Byte (96, B.encode_u32 0 results) in
  ghost_ (decode_def bytes); bytes

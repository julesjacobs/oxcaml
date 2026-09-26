module B = Wasm_u32
module I = Wasm_instruction
module Frame = Wasm_framing
let[@def] (decode @ total) (bytes : B.bytes @ immutable) =
  match B.decode_5 bytes with
  | Some (1, segments) ->
    (match B.decode_5 segments with
    | Some (0, offset) ->
      (match I.decode offset with
      | Some (I.I32_const 0, stop) ->
        (match I.decode stop with Some (I.Plain I.End, payload) -> Frame.decode payload | _ -> None)
      | _ -> None)
    | _ -> None)
  | _ -> None
let (encode @ total) : (data : B.bytes) @ immutable -> (tail : B.bytes) @ immutable ->
    {out : B.bytes option | match out with None -> Frame.size data === None | Some bytes -> not (Frame.size data === None) && decode bytes === Some (data, tail)} @ immutable =
  fun data tail ->
    match Frame.encode data tail with
    | None -> None
    | Some payload ->
      let stop = I.encode (I.Plain I.End) payload in
      let offset = I.encode (I.I32_const 0) stop in
      let bytes = B.encode_u32 1 (B.encode_u32 0 offset) in
      ghost_ (decode_def bytes); Some bytes

module B = Wasm_u32

let[@def] rec (size @ total) (bytes : B.bytes @ immutable) : B.u32 option =
  match bytes with
  | B.End -> Some 0
  | B.Byte (_, rest) ->
    match size rest with
    | None -> None
    | Some n -> if n = 4294967295 then None else Some (n + 1)

let[@def] rec (split @ total) (bytes : B.bytes @ immutable) (count : B.u32)
    : (B.bytes * B.bytes) option @ immutable =
  if count = 0 then Some (B.End, bytes) else
    match bytes with
    | B.End -> None
    | B.Byte (head, rest) ->
      match split rest (count - 1) with
      | None -> None
      | Some (prefix, suffix) -> Some (B.Byte (head, prefix), suffix)

let rec (append @ total) : (payload : B.bytes) @ immutable ->
    (tail : B.bytes) @ immutable -> (count : B.u32) ->
    {u : unit | size payload === Some count} ->
    {bytes : B.bytes | split bytes count === Some (payload, tail)} @ immutable =
  fun payload tail count premise ->
    ghost_ (size_def payload);
    match payload with
    | B.End -> ghost_ (split_def tail count); tail
    | B.Byte (head, rest) ->
      let bytes = B.Byte (head, append rest tail (count - 1) ()) in
      ghost_ (split_def bytes count); bytes

let[@def] (decode @ total) (bytes : B.bytes @ immutable) =
  match B.decode_5 bytes with
  | None -> None
  | Some (count, rest) -> split rest count

let (encode @ total) : (payload : B.bytes) @ immutable ->
    (tail : B.bytes) @ immutable ->
    {out : B.bytes option | match out with
      | None -> size payload === None
      | Some bytes -> not (size payload === None) && decode bytes === Some (payload, tail)} @ immutable =
  fun payload tail ->
    match size payload with
    | None -> None
    | Some count ->
      let bytes = B.encode_u32 count (append payload tail count ()) in
      ghost_ (decode_def bytes); Some bytes

module B = Wasm_u32
module F = Wasm_functions
let[@def] rec (count @ total) (locals : F.local_types @ immutable) : B.u32 option =
  match locals with
  | F.No_locals -> Some 0
  | F.Local32 rest | F.Local64 rest ->
    match count rest with None -> None | Some n -> if n = 4294967295 then None else Some (n + 1)
let[@def] rec (decode_entries @ total) (count : B.u32) (bytes : B.bytes @ immutable) : (F.local_types * B.bytes) option @ immutable =
  if count = 0 then Some (F.No_locals, bytes) else
  match bytes with
  | B.Byte (1, B.Byte (kind, rest)) ->
    (match decode_entries (count - 1) rest with
    | None -> None
    | Some (locals, tail) ->
      if kind = 127 then Some (F.Local32 locals, tail)
      else if kind = 126 then Some (F.Local64 locals, tail) else None)
  | _ -> None
let[@def] (decode @ total) (bytes : B.bytes @ immutable) =
  match B.decode_5 bytes with None -> None | Some (count, rest) -> decode_entries count rest
let rec (encode_entries @ total) : (locals : F.local_types) @ immutable -> (tail : B.bytes) @ immutable -> (number : B.u32) ->
    {u : unit | count locals === Some number} ->
    {bytes : B.bytes | decode_entries number bytes === Some (locals, tail)} @ immutable =
  fun locals tail number premise ->
    ghost_ (count_def locals);
    match locals with
    | F.No_locals -> ghost_ (decode_entries_def number tail); tail
    | F.Local32 rest ->
      let suffix = encode_entries rest tail (number - 1) () in
      let bytes = B.Byte (1, B.Byte (127, suffix)) in
      ghost_ (decode_entries_def number bytes); bytes
    | F.Local64 rest ->
      let suffix = encode_entries rest tail (number - 1) () in
      let bytes = B.Byte (1, B.Byte (126, suffix)) in
      ghost_ (decode_entries_def number bytes); bytes
let (encode @ total) : (locals : F.local_types) @ immutable -> (tail : B.bytes) @ immutable ->
    {out : B.bytes option | match out with None -> count locals === None | Some bytes -> decode bytes === Some (locals, tail)} @ immutable =
  fun locals tail ->
    match count locals with
    | None -> None
    | Some number ->
      let payload = encode_entries locals tail number () in
      let bytes = B.encode_u32 number payload in
      ghost_ (decode_def bytes); Some bytes

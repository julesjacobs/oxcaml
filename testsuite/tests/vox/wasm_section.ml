module B = Wasm_u32
module F = Wasm_framing

type id = {n : int | 0 <= n && n <= 11}
type t = {id : id; payload : B.bytes}
let[@def] (decode @ total) (bytes : B.bytes @ immutable) =
  match bytes with
  | B.Byte (id, rest) ->
    if id > 11 then None else
      (match F.decode rest with
       | None -> None
       | Some (payload, tail) -> Some ({id; payload}, tail))
  | B.End -> None
let (encode @ total) : (section : t) @ immutable ->
    (tail : B.bytes) @ immutable ->
    {out : B.bytes option | match out with
      | None -> F.size section.payload === None
      | Some bytes -> not (F.size section.payload === None)
        && decode bytes === Some (section, tail)} @ immutable =
  fun section tail ->
    match F.encode section.payload tail with
    | None -> None
    | Some rest ->
      let bytes = B.Byte (section.id, rest) in
      ghost_ (decode_def bytes); Some bytes

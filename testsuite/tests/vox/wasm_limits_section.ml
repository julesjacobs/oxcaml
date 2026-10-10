module B = Wasm_u32
type limits = {minimum : B.u32; maximum : B.u32}
let[@def] (decode_limits @ total) (bytes : B.bytes @ immutable) =
  match bytes with
  | B.Byte (1, bounds) ->
    (match B.decode_5 bounds with
    | None -> None
    | Some (minimum, rest) -> match B.decode_5 rest with
      | Some (maximum, tail) -> if minimum <= maximum then Some ({minimum; maximum}, tail) else None
      | None -> None)
  | _ -> None
let (encode_limits @ total) : (limits : limits) @ immutable -> (tail : B.bytes) @ immutable ->
    {u : unit | limits.minimum <= limits.maximum} ->
    {bytes : B.bytes | decode_limits bytes === Some (limits, tail)} @ immutable =
  fun limits tail premise ->
    let bytes = B.Byte (1, B.encode_u32 limits.minimum (B.encode_u32 limits.maximum tail)) in
    ghost_ (decode_limits_def bytes); bytes
let[@def] (decode_table @ total) (bytes : B.bytes @ immutable) =
  match B.decode_5 bytes with Some (1, B.Byte (112, rest)) -> decode_limits rest | _ -> None
let[@def] (decode_memory @ total) (bytes : B.bytes @ immutable) =
  match B.decode_5 bytes with
  | Some (1, rest) ->
    (match decode_limits rest with Some (limits, tail) -> if limits.maximum <= 65536 then Some (limits, tail) else None | None -> None)
  | _ -> None
let (encode_table @ total) : (limits : limits) @ immutable -> (tail : B.bytes) @ immutable ->
    {u : unit | limits.minimum <= limits.maximum} ->
    {bytes : B.bytes | decode_table bytes === Some (limits, tail)} @ immutable =
  fun limits tail premise ->
    let bytes = B.encode_u32 1 (B.Byte (112, encode_limits limits tail ())) in
    ghost_ (decode_table_def bytes); bytes
let (encode_memory @ total) : (limits : limits) @ immutable -> (tail : B.bytes) @ immutable ->
    {u : unit | limits.minimum <= limits.maximum && limits.maximum <= 65536} ->
    {bytes : B.bytes | decode_memory bytes === Some (limits, tail)} @ immutable =
  fun limits tail premise ->
    let bytes = B.encode_u32 1 (encode_limits limits tail ()) in
    ghost_ (decode_memory_def bytes); bytes

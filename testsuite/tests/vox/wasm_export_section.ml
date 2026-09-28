module B = Wasm_u32
type exports = {run : B.u32; memory : B.u32; tag : B.u32; payload : B.u32}
let[@def] (decode_run @ total) (bytes : B.bytes @ immutable) : (B.u32 * B.bytes) option @ immutable =
  match bytes with B.Byte (3, B.Byte (114, B.Byte (117, B.Byte (110, B.Byte (0, rest))))) -> (match B.decode_5 rest with None -> None | Some (index, tail) -> Some (index, tail)) | _ -> None
let (encode_run @ total) : (index : B.u32) -> (tail : B.bytes) @ immutable ->
    {bytes : B.bytes | decode_run bytes === Some (index, tail)} @ immutable =
  fun index tail -> let bytes = B.Byte (3, B.Byte (114, B.Byte (117, B.Byte (110, B.Byte (0, B.encode_u32 index tail))))) in
    ghost_ (decode_run_def bytes); bytes
let[@def] (decode_memory @ total) (bytes : B.bytes @ immutable) : (B.u32 * B.bytes) option @ immutable =
  match bytes with B.Byte (6, B.Byte (109, B.Byte (101, B.Byte (109, B.Byte (111, B.Byte (114, B.Byte (121, B.Byte (2, rest)))))))) -> (match B.decode_5 rest with None -> None | Some (index, tail) -> Some (index, tail)) | _ -> None
let (encode_memory @ total) : (index : B.u32) -> (tail : B.bytes) @ immutable ->
    {bytes : B.bytes | decode_memory bytes === Some (index, tail)} @ immutable =
  fun index tail -> let bytes = B.Byte (6, B.Byte (109, B.Byte (101, B.Byte (109, B.Byte (111, B.Byte (114, B.Byte (121, B.Byte (2, B.encode_u32 index tail)))))))) in
    ghost_ (decode_memory_def bytes); bytes
let[@def] (decode_tag @ total) (bytes : B.bytes @ immutable) : (B.u32 * B.bytes) option @ immutable =
  match bytes with B.Byte (3, B.Byte (116, B.Byte (97, B.Byte (103, B.Byte (3, rest))))) -> (match B.decode_5 rest with None -> None | Some (index, tail) -> Some (index, tail)) | _ -> None
let (encode_tag @ total) : (index : B.u32) -> (tail : B.bytes) @ immutable ->
    {bytes : B.bytes | decode_tag bytes === Some (index, tail)} @ immutable =
  fun index tail -> let bytes = B.Byte (3, B.Byte (116, B.Byte (97, B.Byte (103, B.Byte (3, B.encode_u32 index tail))))) in
    ghost_ (decode_tag_def bytes); bytes
let[@def] (decode_payload @ total) (bytes : B.bytes @ immutable) : (B.u32 * B.bytes) option @ immutable =
  match bytes with B.Byte (7, B.Byte (112, B.Byte (97, B.Byte (121, B.Byte (108, B.Byte (111, B.Byte (97, B.Byte (100, B.Byte (3, rest))))))))) -> (match B.decode_5 rest with None -> None | Some (index, tail) -> Some (index, tail)) | _ -> None
let (encode_payload @ total) : (index : B.u32) -> (tail : B.bytes) @ immutable ->
    {bytes : B.bytes | decode_payload bytes === Some (index, tail)} @ immutable =
  fun index tail -> let bytes = B.Byte (7, B.Byte (112, B.Byte (97, B.Byte (121, B.Byte (108, B.Byte (111, B.Byte (97, B.Byte (100, B.Byte (3, B.encode_u32 index tail))))))))) in
    ghost_ (decode_payload_def bytes); bytes
let[@def] (decode @ total) (bytes : B.bytes @ immutable) =
  match B.decode_5 bytes with
  | Some (4, r0) ->
    (match decode_run r0 with None -> None | Some (run, r1) ->
      match decode_memory r1 with None -> None | Some (memory, r2) ->
      match decode_tag r2 with None -> None | Some (tag, r3) ->
      match decode_payload r3 with None -> None | Some (payload, tail) -> Some ({run; memory; tag; payload}, tail))
  | _ -> None
let (encode @ total) : (exports : exports) @ immutable -> (tail : B.bytes) @ immutable ->
    {bytes : B.bytes | decode bytes === Some (exports, tail)} @ immutable =
  fun exports tail ->
    let bytes = B.encode_u32 4 (encode_run exports.run (encode_memory exports.memory (encode_tag exports.tag (encode_payload exports.payload tail)))) in
    ghost_ (decode_def bytes); bytes

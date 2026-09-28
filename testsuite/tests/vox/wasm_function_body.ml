module B = Wasm_u32
module C = Wasm_code
module I = Wasm_instruction
module T = Wasm_control
module Codec = Wasm_control_codec
module N = Wasm_nesting
module Locals = Wasm_local_declarations
module Stream = Wasm_instruction_stream
module Frame = Wasm_framing
type t = {locals : Wasm_functions.local_types; code : T.code}
let[@def] (decode_code @ total) (tokens : C.t @ immutable) =
  match Codec.parse tokens Codec.Empty (Codec.In_block (Codec.Empty, Codec.Root)) with
  | Some (T.Block (body, T.Empty)) -> Some body | _ -> None
let (code_roundtrip @ total) : (code : T.code) @ immutable -> {u : unit | N.structured code} ->
    {u : unit | decode_code (T.flatten code (C.Next (I.Plain I.End, C.Empty))) === Some code} @ ghost =
  fun code premise -> ghost_ (
    let close = C.Next (I.Plain I.End, C.Empty) in
    let frames = Codec.In_block (Codec.Empty, Codec.Root) in
    Codec.splice code close Codec.Empty frames ();
    Codec.finish_push code Codec.Empty; Codec.finish_def Codec.Empty code;
    Codec.parse_def close (Codec.push code Codec.Empty) frames;
    Codec.parse_def C.Empty (Codec.Block (code, Codec.Empty)) Codec.Root;
    Codec.finish_def (Codec.Block (code, Codec.Empty)) T.Empty;
    Codec.finish_def Codec.Empty (T.Block (code, T.Empty));
    decode_code_def (T.flatten code close))
let[@def] (decode_payload @ total) (bytes : B.bytes @ immutable) =
  match Locals.decode bytes with
  | None -> None
  | Some (locals, instructions) ->
    match Stream.decode instructions with
    | None -> None
    | Some tokens -> match decode_code tokens with None -> None | Some code -> Some {locals; code}
let[@def] (decode @ total) (bytes : B.bytes @ immutable) =
  match Frame.decode bytes with
  | None -> None
  | Some (payload, tail) -> match decode_payload payload with None -> None | Some body -> Some (body, tail)
let[@def] (payload @ total) : (body : t) @ immutable ->
    {out : B.bytes option | match out with None -> Locals.count body.locals === None
      | Some bytes -> not (N.structured body.code) || decode_payload bytes === Some body} @ immutable =
  fun body ->
    let instructions = Stream.encode (T.flatten body.code (C.Next (I.Plain I.End, C.Empty))) in
    ghost_ (if N.structured body.code then code_roundtrip body.code () else ());
    match Locals.encode body.locals instructions with
    | None -> None
    | Some bytes -> ghost_ (decode_payload_def bytes); Some bytes
let (encode_payload @ total) : (body : t) @ immutable -> {u : unit | N.structured body.code} ->
    {out : B.bytes option | match out with None -> Locals.count body.locals === None | Some bytes -> decode_payload bytes === Some body} @ immutable =
  fun body premise -> payload body
let[@def] (encodable @ total) (body : t @ immutable) = ghost_ (
  match payload body with None -> false | Some bytes -> not (Frame.size bytes === None))
let (encode @ total) : (body : t) @ immutable -> (tail : B.bytes) @ immutable -> {u : unit | N.structured body.code} ->
    {out : B.bytes option | match out with None -> not (encodable body)
      | Some bytes -> encodable body && decode bytes === Some (body, tail)} @ immutable =
  fun body tail premise ->
    ghost_ (encodable_def body);
    match payload body with
    | None -> None
    | Some payload -> match Frame.encode payload tail with
      | None -> None
      | Some bytes -> ghost_ (decode_def bytes); Some bytes

module B = Wasm_u32
module F = Wasm_functions
module G = Wasm_globals
module Binary = Wasm_binary_module
module Limits = Wasm_limits_section
module Exports = Wasm_export_section
module Bodies = Wasm_static_control
module Typing = Wasm_static_typing
let[@def] (exports_valid @ total) (image : Binary.image @ immutable) =
  image.Binary.exports.Exports.memory = 0 &&
  (match F.lookup image.Binary.module_.F.functions image.Binary.exports.Exports.run with None -> false | Some _ -> true) &&
  (match G.get image.Binary.globals image.Binary.exports.Exports.tag with None -> false | Some _ -> true) &&
  (match G.get image.Binary.globals image.Binary.exports.Exports.payload with None -> false | Some _ -> true)
let[@def] (structure_valid @ total) (image : Binary.image @ immutable) =
  image.Binary.table_limits.Limits.minimum <= image.Binary.table_limits.Limits.maximum &&
  image.Binary.memory_limits.Limits.minimum <= image.Binary.memory_limits.Limits.maximum &&
  image.Binary.memory_limits.Limits.maximum <= 65536 &&
  (match Wasm_global_section.count image.Binary.globals.G.values image.Binary.globals.G.permissions with None -> false | Some _ -> true) &&
  Wasm_function_sections.supported image.Binary.module_.F.signatures image.Binary.module_.F.functions &&
  Wasm_binary_execution.table_targets image.Binary.module_.F.functions image.Binary.module_.F.table &&
  exports_valid image
let[@def] (valid @ total) (image : Binary.image @ immutable) =
  structure_valid image && Bodies.function_bodies image.Binary.module_ image.Binary.globals
let[@def] (bytes_valid @ total) (bytes : B.bytes @ immutable) =
  match Binary.decode bytes with Some (image, B.End) -> valid image | _ -> false
let (body_derivations @ total) : (image : Binary.image) @ immutable -> {u : unit | valid image} ->
    {proof : Typing.module_derivation | Typing.functions_typed image.Binary.module_ image.Binary.globals image.Binary.module_.F.functions proof} @ immutable ghost =
  fun image premise -> ghost_ (valid_def image; Typing.function_bodies_sound image.Binary.module_ image.Binary.globals ())
type result = Invalid | Too_large | Encoded of B.bytes [@@inductive]
let (encode @ total) : (image : Binary.image) @ immutable ->
    {out : result | match out with
      | Invalid -> not (valid image)
      | Too_large -> valid image && not (Binary.encodable image)
      | Encoded bytes -> valid image && Binary.encodable image && bytes_valid bytes
        && Binary.decode bytes === Some (image, B.End)} @ immutable =
  fun image ->
    if not (valid image) then Invalid else
    match Binary.encode image B.End with
    | None -> Too_large
    | Some bytes -> ghost_ (bytes_valid_def bytes); Encoded bytes
let (decoded_valid @ total) : (bytes : B.bytes) @ immutable -> (image : Binary.image) @ immutable ->
    {u : unit | bytes_valid bytes && Binary.decode bytes === Some (image, B.End)} ->
    {u : unit | valid image} @ ghost = fun bytes image premise -> ghost_ (bytes_valid_def bytes)

let (structure_from_encoding @ total) : (image : Binary.image) @ immutable ->
    {u : unit | Binary.encodable image && Wasm_binary_execution.materialized image && exports_valid image} ->
    {u : unit | structure_valid image} @ ghost = fun image premise -> ghost_ (
  Binary.encodable_def image;
  match Binary.encode_payloads image with
  | None -> ()
  | Some _ ->
    Binary.payloads_encodable_def image;
    Wasm_binary_execution.materialized_def image;
    Wasm_function_sections.encodable_def image.Binary.module_.F.signatures image.Binary.module_.F.functions;
    (match Wasm_function_sections.split image.Binary.module_.F.signatures image.Binary.module_.F.functions with
    | None -> ()
    | Some _ -> structure_valid_def image))
let (binary_valid @ total) : (image : Binary.image) @ immutable -> (bytes : B.bytes) @ immutable ->
    {u : unit | Binary.encodable image && Wasm_binary_execution.materialized image && exports_valid image
      && Bodies.function_bodies image.Binary.module_ image.Binary.globals && Binary.decode bytes === Some (image, B.End)} ->
    {u : unit | valid image && bytes_valid bytes} @ ghost = fun image bytes premise -> ghost_ (
      structure_from_encoding image (); valid_def image; bytes_valid_def bytes)

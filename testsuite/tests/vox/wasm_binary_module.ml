module B = Wasm_u32
module F = Wasm_functions
module G = Wasm_globals
module Limits = Wasm_limits_section
module Exports = Wasm_export_section
module Signatures = Wasm_signature_section
module Functions = Wasm_function_sections
module Globals = Wasm_global_section
module Indices = Wasm_index_vector
module Data = Wasm_data_section
module M = Wasm_module
type image = {module_ : F.module_; globals : G.t; table_limits : Limits.limits;
  memory_limits : Limits.limits; exports : Exports.exports; data : B.bytes}
type decoded = Invalid | Decoded of image [@@inductive]
let[@def] (decode_payloads @ total) (sections : M.t @ immutable) : decoded @ immutable =
  match Signatures.decode sections.M.types with
  | Some (signatures, B.End) ->
    (match Functions.decode signatures {Functions.types = sections.M.functions; code = sections.M.code} with
    | None -> Invalid
    | Some functions ->
      match Limits.decode_table sections.M.table with
      | Some (table_limits, B.End) ->
        (match Limits.decode_memory sections.M.memory with
        | Some (memory_limits, B.End) ->
          (match Globals.decode sections.M.globals with
          | Some (globals, B.End) ->
            (match Exports.decode sections.M.exports with
            | Some (exports, B.End) ->
              (match Indices.decode_element sections.M.elements with
              | Some (table, B.End) ->
                (match Data.decode sections.M.data with
                | Some (data, B.End) -> Decoded {module_ = {F.functions; signatures; table}; globals;
                    table_limits; memory_limits; exports; data}
                | _ -> Invalid)
              | _ -> Invalid)
            | _ -> Invalid)
          | _ -> Invalid)
        | _ -> Invalid)
      | _ -> Invalid)
  | _ -> Invalid
let[@def] (decode @ total) (bytes : B.bytes @ immutable) : (image * B.bytes) option @ immutable =
  match M.decode bytes with None -> None | Some (sections, tail) ->
    match decode_payloads sections with Invalid -> None | Decoded image -> Some (image, tail)
let[@def] (payloads_encodable @ total) (image : image @ immutable) = ghost_ (
  image.table_limits.Limits.minimum <= image.table_limits.Limits.maximum &&
  image.memory_limits.Limits.minimum <= image.memory_limits.Limits.maximum &&
  image.memory_limits.Limits.maximum <= 65536 &&
  not (Signatures.count image.module_.F.signatures === None) &&
  Functions.encodable image.module_.F.signatures image.module_.F.functions &&
  not (Globals.count image.globals.G.values image.globals.G.permissions === None) &&
  not (Indices.count image.module_.F.table === None) && not (Wasm_framing.size image.data === None))
let (encode_payloads @ total) : (image : image) @ immutable ->
    {out : M.t option | match out with None -> not (payloads_encodable image)
      | Some sections -> payloads_encodable image && decode_payloads sections === Decoded image} @ immutable =
  fun image ->
    ghost_ (payloads_encodable_def image);
    if image.table_limits.Limits.minimum > image.table_limits.Limits.maximum ||
       image.memory_limits.Limits.minimum > image.memory_limits.Limits.maximum ||
       image.memory_limits.Limits.maximum > 65536 then None else
    match Signatures.encode image.module_.F.signatures B.End with None -> None | Some types ->
      match Functions.encode image.module_.F.signatures image.module_.F.functions with None -> None | Some functions ->
        let table = Limits.encode_table image.table_limits B.End () in
        let memory = Limits.encode_memory image.memory_limits B.End () in
        match Globals.encode image.globals B.End with None -> None | Some globals ->
          let exports = Exports.encode image.exports B.End in
          match Indices.encode_element image.module_.F.table B.End with None -> None | Some elements ->
            match Data.encode image.data B.End with None -> None | Some data ->
              let sections = {M.types; functions = functions.Functions.types; code = functions.Functions.code;
                table; memory; globals; exports; elements; data} in
              ghost_ (decode_payloads_def sections); Some sections
let[@def] (encodable @ total) (image : image @ immutable) = ghost_ (
  match encode_payloads image with None -> false | Some sections -> M.encodable sections)
let (encode @ total) : (image : image) @ immutable -> (tail : B.bytes) @ immutable ->
    {out : B.bytes option | match out with None -> not (encodable image)
      | Some bytes -> encodable image && decode bytes === Some (image, tail)} @ immutable =
  fun image tail -> ghost_ (encodable_def image); match encode_payloads image with None -> None | Some sections ->
    match M.encode sections tail with None -> None | Some bytes -> ghost_ (decode_def bytes); Some bytes

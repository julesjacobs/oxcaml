module B = Wasm_u32
module F = Wasm_functions
module G = Wasm_globals
module Binary = Wasm_binary_module
module Limits = Wasm_limits_section
module Exports = Wasm_export_section
module Frame = Wasm_framing
module Indices = Wasm_index_vector
module Calls = Wasm_calls
module C = Wasm_code
let[@def] rec (table_targets @ total) (functions : F.functions @ immutable) (table : F.table @ immutable) =
  match table with
  | F.No_elements -> true
  | F.Element (None, _) -> false
  | F.Element (Some index, rest) ->
    match F.lookup functions index with None -> false | Some _ -> table_targets functions rest
let[@def] (materialized @ total) (image : Binary.image @ immutable) =
  image.Binary.memory_limits.Limits.minimum < 65536 &&
  image.Binary.memory_limits.Limits.minimum <= image.Binary.memory_limits.Limits.maximum &&
  image.Binary.memory_limits.Limits.maximum <= 65536 &&
  image.Binary.table_limits.Limits.minimum <= image.Binary.table_limits.Limits.maximum &&
  (match Frame.size image.Binary.data with None -> false | Some size -> size = image.Binary.memory_limits.Limits.minimum * 65536) &&
  (match Indices.count image.Binary.module_.F.table with None -> false | Some count -> count = image.Binary.table_limits.Limits.minimum) &&
  table_targets image.Binary.module_.F.functions image.Binary.module_.F.table &&
  image.Binary.exports.Exports.memory = 0
type result = Rejected | Result of Calls.result [@@inductive]
(* The host sets the exported global [payload] to [input] and then calls the
   exported function [run], as in [instance.exports.payload.value = input;
   instance.exports.run ()]. Setting the global fails unless it is a mutable
   i64 global. *)
let[@def] (run @ total) (fuel : C.count @ immutable) (bytes : B.bytes @ immutable) (input : Hmc_word64.t @ immutable)
    (capacity : C.count @ immutable) : result @ immutable =
  match Binary.decode bytes with
  | Some (image, B.End) ->
    if not (materialized image) then Rejected else
    (match G.set image.Binary.globals image.Binary.exports.Exports.payload (Wasm_scalar.I64 input) with
    | None -> Rejected
    | Some globals ->
      match Calls.start image.Binary.module_ image.Binary.exports.Exports.run image.Binary.data globals capacity with
      | Calls.Running configuration -> Result (Calls.run fuel image.Binary.module_ configuration)
      | result -> Result result)
  | _ -> Rejected
let (correspondence @ total) : (image : Binary.image) @ immutable -> (bytes : B.bytes) @ immutable ->
    (input : Hmc_word64.t) @ immutable -> (globals : G.t) @ immutable ->
    (fuel : C.count) @ immutable -> (capacity : C.count) @ immutable -> (configuration : Calls.configuration) @ immutable ->
    {u : unit | Binary.decode bytes === Some (image, B.End) && materialized image
      && G.set image.Binary.globals image.Binary.exports.Exports.payload (Wasm_scalar.I64 input) === Some globals
      && Calls.start image.Binary.module_ image.Binary.exports.Exports.run image.Binary.data globals capacity === Calls.Running configuration} ->
    {u : unit | run fuel bytes input capacity === Result (Calls.run fuel image.Binary.module_ configuration)} @ ghost =
  fun image bytes input globals fuel capacity configuration premise -> ghost_ (run_def fuel bytes input capacity)

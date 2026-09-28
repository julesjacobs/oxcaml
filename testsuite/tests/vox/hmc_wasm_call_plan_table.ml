module B = Wasm_u32
module D = Hm_declarative
module K = Hmc_closure_ir
module Index = Hmc_u32_index
module Copy = Hmc_wasm_call_captures
module Frame = Hmc_wasm_dynamic_call_frame
module R = Hmc_wasm_relayout
module T = Wasm_control
module Lift = Wasm_control_lift
module Dispatch = Hmc_wasm_dispatch_code
type table = Empty | Add of B.u32 * Copy.fragment * table [@@inductive]
let[@def] rec (lookup @ total) (table : table @ immutable) (code : B.u32) = match table with
  | Empty -> None | Add (label, fragment, rest) -> if code = label then Some fragment else lookup rest code
let[@def] rec (related @ total) (source : K.table @ immutable) (capacity : R.count) (table : table @ immutable) = ghost_ (
  match source, table with
  | K.Empty, Empty -> true
  | K.Add (entry, rest), Add (code, fragment, tail) ->
    Index.represents (K.size rest) code && Copy.matches entry capacity fragment && related rest capacity tail
  | _ -> false)
let[@def] rec (encodable @ total) (source : K.table @ immutable) (capacity : R.count) (code_capacity : B.u32) = ghost_ (
  match source with
  | K.Empty -> true
  | K.Add (entry, rest) -> Index.fits (K.size rest) code_capacity
    && Copy.encodable entry capacity && encodable rest capacity code_capacity)
let rec (build @ total) : (source : K.table) @ immutable -> (capacity : R.count) -> (code_capacity : B.u32) ->
    {out : table option | match out with None -> not (encodable source capacity code_capacity)
      | Some table -> encodable source capacity code_capacity && related source capacity table} @ immutable =
  fun source capacity code_capacity ->
  ghost_ (encodable_def source capacity code_capacity);
  match source with
  | K.Empty -> ghost_ (related_def source capacity Empty); Some Empty
  | K.Add (entry, rest) ->
    match Index.encode code_capacity (K.size rest), Copy.build entry capacity, build rest capacity code_capacity with
    | Some code, Some fragment, Some tail ->
      let out = Add (code, fragment, tail) in
      ghost_ (related_def source capacity out); Some out
    | _ -> None
let rec (lookup_correct @ total) : (source : K.table) @ immutable -> (capacity : R.count) -> (table : table) @ immutable ->
    (id : D.index) @ immutable -> (code : B.u32) ->
    {u : unit | related source capacity table && Index.represents id code} ->
    {u : unit | match lookup table code, K.lookup source id with
      | Some fragment, Some entry -> Copy.matches entry capacity fragment
      | None, None -> true | _ -> false} @ ghost =
  fun source capacity table id code premise -> ghost_ (
    related_def source capacity table; lookup_def table code; K.lookup_def source id;
    match source, table with
    | K.Add (_, rest), Add (label, _, tail) ->
      let same = Hm_elaboration_check.index_equal id (K.size rest) in
      if code = label then Index.injective id (K.size rest) code ()
      else (if same then Index.unique id code label () else (); lookup_correct rest capacity tail id code ())
    | _ -> ())
let[@def] rec (prepare @ total) (table : table @ immutable) (pc_local : B.u32) (object_local : B.u32)
    (frame_local : B.u32) (argument_tag : B.u32) (argument_payload : B.u32) = match table with
  | Empty -> Dispatch.Empty
  | Add (code, fragment, rest) -> Dispatch.Add (code,
      Lift.embed (Frame.emit fragment pc_local object_local frame_local argument_tag argument_payload) T.Empty,
      prepare rest pc_local object_local frame_local argument_tag argument_payload)
let rec (prepared_lookup @ total) : (table : table) @ immutable -> (code : B.u32) -> (pc_local : B.u32) -> (object_local : B.u32) ->
    (frame_local : B.u32) -> (argument_tag : B.u32) -> (argument_payload : B.u32) ->
    {u : unit | Dispatch.lookup (prepare table pc_local object_local frame_local argument_tag argument_payload) code ===
      (match lookup table code with None -> None | Some fragment ->
        Some (Lift.embed (Frame.emit fragment pc_local object_local frame_local argument_tag argument_payload) T.Empty))} @ ghost =
  fun table code pc_local object_local frame_local argument_tag argument_payload -> ghost_ (
    prepare_def table pc_local object_local frame_local argument_tag argument_payload; lookup_def table code;
    Dispatch.lookup_def (prepare table pc_local object_local frame_local argument_tag argument_payload) code;
    match table with Empty -> () | Add (label, _, rest) ->
      if code = label then () else prepared_lookup rest code pc_local object_local frame_local argument_tag argument_payload)

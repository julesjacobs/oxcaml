module B = Wasm_u32
module D = Hm_declarative
module G = Hmc_cfg_ir
module I = Hmc_tail_ir
module Machine = Hmc_heap_machine
module R = Hmc_wasm_relayout
module Index = Hmc_u32_index
module Codec = Hmc_pointer_frame_codec
module Structured = Hmc_wasm_structured_block
module Save = Hmc_wasm_call_save
module Pad = Hmc_wasm_frame_padding
module Write = Wasm_mixed_write
let[@def] (difference @ total) (n : B.u32) (used : B.u32) : B.u32 = if used <= n then n - used else 0
let rec (padding_index @ total) : (capacity : D.index) @ immutable -> (used : B.u32) -> (n : B.u32) ->
    {u : unit | Index.represents capacity n && used <= n} ->
    {out : D.index | Index.represents out (difference n used)} @ immutable = fun capacity used n premise ->
  ghost_ (Index.represents_def capacity n; difference_def n used);
  match capacity with
  | D.Z -> ghost_ (Index.represents_def D.Z (n - used)); D.Z
  | D.S rest -> if used = 0 then capacity else (ghost_ (difference_def (n - 1) (used - 1)); padding_index rest (used - 1) (n - 1) ())
type call = {save : Save.fragment; padding : Write.writes; padding_length : D.index; saved : R.count; environment : R.count}
type fragment = Structured of Structured.fragment | Call of call | Tail_call of R.count | Return [@@inductive]
let[@def] (corresponds @ total) (globals : Machine.globals @ immutable) (signature : G.signature @ immutable)
    (instruction : I.instruction @ immutable) (capacity : R.count) (max_pc : B.u32) (fragment : fragment @ immutable) = ghost_ (
  match instruction, fragment with
  | I.Keep op, Structured body -> Structured.corresponds globals signature op capacity max_pc body
  | I.Keep (G.Call next), Call call ->
    Save.matches signature next capacity call.save
    && Index.represents (Codec.locals_size signature.G.locals) call.environment
    && (match signature.G.temporaries with
      | G.Value (saved, _, rest) ->
        Index.represents (D.add (Codec.locals_size saved) (Codec.temporaries_size rest)) call.saved
        && 2 + call.saved <= capacity
        && Index.represents call.padding_length (difference capacity (2 + call.saved))
        && Pad.matches call.padding (3 + call.saved) call.padding_length
      | _ -> false)
  | I.Tail_call, Tail_call environment -> Index.represents (Codec.locals_size signature.G.locals) environment
  | I.Keep G.Return, Return -> true
  | _ -> false)
let[@def] (encodable @ total) (globals : Machine.globals @ immutable) (signature : G.signature @ immutable) (instruction : I.instruction @ immutable)
    (capacity : R.count) (max_pc : B.u32) = ghost_ (
  match instruction with
  | I.Tail_call -> Index.fits (Codec.locals_size signature.G.locals) capacity
  | I.Keep G.Return -> true
  | I.Keep (G.Call next) -> (match signature.G.temporaries with
    | G.Value (saved, _, rest) ->
      let saved_size = D.add (Codec.locals_size saved) (Codec.temporaries_size rest) in
      Save.encodable signature next capacity max_pc && Index.fits (Codec.locals_size signature.G.locals) capacity
      && Index.fits saved_size capacity && 2 + Hmc_wasm_relayout_geometry.size saved_size <= capacity
    | _ -> false)
  | I.Keep op -> Structured.encodable globals signature op capacity max_pc)
let (lower @ total) : (globals : Machine.globals) @ immutable -> (signature : G.signature) @ immutable ->
    (instruction : I.instruction) @ immutable -> (capacity_index : D.index) @ immutable -> (capacity : R.count) -> (max_pc : B.u32) ->
    {u : unit | Index.represents capacity_index capacity && capacity < 268435452} ->
    {out : fragment option | match out with None -> not (encodable globals signature instruction capacity max_pc) | Some fragment -> encodable globals signature instruction capacity max_pc && corresponds globals signature instruction capacity max_pc fragment} @ immutable =
  fun globals signature instruction capacity_index capacity max_pc premise ->
    ghost_ (encodable_def globals signature instruction capacity max_pc);
  match instruction with
  | I.Tail_call -> (match Index.encode capacity (Codec.locals_size signature.G.locals) with
    | None -> None
    | Some environment -> let fragment = Tail_call environment in
      ghost_ (corresponds_def globals signature instruction capacity max_pc fragment); Some fragment)
  | I.Keep G.Return -> ghost_ (corresponds_def globals signature instruction capacity max_pc Return); Some Return
  | I.Keep (G.Call next) -> (match signature.G.temporaries with
    | G.Value (saved_context, _, rest) ->
      (match Save.build signature next capacity max_pc,
        Index.encode capacity (Codec.locals_size signature.G.locals),
        Index.encode capacity (D.add (Codec.locals_size saved_context) (Codec.temporaries_size rest)) with
      | Some save, Some environment, Some saved ->
        ghost_ (Hmc_wasm_relayout_geometry.size_represents (D.add (Codec.locals_size saved_context) (Codec.temporaries_size rest)) saved ());
        if 2 + saved > capacity then None else
        let padding_length = padding_index capacity_index (2 + saved) capacity () in
        let _ = ghost_ (difference_def capacity (2 + saved)) in
        let padding = Pad.build (3 + saved) (capacity - 2 - saved) padding_length () in
        let fragment = Call {save; padding; padding_length; saved; environment} in
        ghost_ (corresponds_def globals signature instruction capacity max_pc fragment); Some fragment
      | _ -> None)
    | _ -> None)
  | I.Keep op -> (match Structured.lower globals signature op capacity max_pc with
    | None -> None
    | Some body -> let fragment = Structured body in
      ghost_ (corresponds_def globals signature instruction capacity max_pc fragment); Some fragment)

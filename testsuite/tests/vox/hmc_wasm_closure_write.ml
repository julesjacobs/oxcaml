module B = Wasm_u32
module W = Hmc_word64
module D = Hm_declarative
module G = Hmc_cfg_ir
module Codec = Hmc_pointer_frame_codec
module Index = Hmc_u32_index
module Relayout = Hmc_wasm_relayout
module Geometry = Hmc_wasm_relayout_geometry
module Plan = Wasm_parallel_copy
module Copy = Wasm_cross_copy
module Header = Hmc_wasm_header_update
module Write = Wasm_immediate_write
module PC = Hmc_wasm_pc_update
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module L = Wasm_locals
module M = Wasm_memory
type fragment = {copies : Plan.plan; code : W.limb; bytes : B.u32}
let[@def] (matches @ total) (context : D.context @ immutable) (id : D.index @ immutable)
    (capacity : Relayout.count) (max_code : W.limb) (fragment : fragment @ immutable) = ghost_ (
  Index.fits (Codec.locals_size context) capacity && 2 + Geometry.size (Codec.locals_size context) <= capacity
  && Index.represents id fragment.code && fragment.code <= max_code
  && fragment.bytes = 16 + 16 * Geometry.size (Codec.locals_size context)
  && Relayout.range_is fragment.copies 2 0 (Codec.locals_size context) Plan.End)
let[@def] (encodable @ total) (context : D.context @ immutable) (id : D.index @ immutable)
    (capacity : Relayout.count) (max_code : W.limb) = ghost_ (
  Index.fits (Codec.locals_size context) capacity && Index.fits id max_code
  && 2 + Geometry.size (Codec.locals_size context) <= capacity)
let (build @ total) : (context : D.context) @ immutable -> (id : D.index) @ immutable ->
    (capacity : Relayout.count) -> (max_code : W.limb) ->
    {out : fragment option | match out with None -> not (encodable context id capacity max_code)
      | Some fragment -> encodable context id capacity max_code && matches context id capacity max_code fragment} @ immutable =
  fun context id capacity max_code ->
    ghost_ (encodable_def context id capacity max_code);
    match Index.encode capacity (Codec.locals_size context), Index.encode max_code id with
    | Some captures, Some code ->
      ghost_ (Geometry.size_represents (Codec.locals_size context) captures ());
      if captures + 2 > capacity then None else
      let copies = Relayout.range 2 0 captures Plan.End (Codec.locals_size context) () in
      let fragment = {copies; code; bytes = 16 + 16 * captures} in
      ghost_ (Geometry.size_represents (Codec.locals_size context) captures (); matches_def context id capacity max_code fragment);
      Some fragment
    | _ -> None
let[@def] (zero @ total) (u : unit) : B.u32 = 0
let[@def] (emit @ total) (fragment : fragment @ immutable) (frame_local : B.u32) (heap_local : B.u32) =
  E.append (Copy.emit fragment.copies frame_local heap_local)
    (E.append (Write.emit (zero ()) heap_local (Hmc_wasm_header_words.tag ())) (PC.emit fragment.code heap_local))
let (correct @ total) : (fragment : fragment) @ immutable -> (frame_local : B.u32) -> (heap_local : B.u32) ->
    (state : X.state) @ immutable -> (frame_base : B.u32) -> (heap_base : B.u32) ->
    (copied : B.bytes) @ immutable -> (tagged : B.bytes) @ immutable -> (after : B.bytes) @ immutable ->
    {u : unit | L.get state.X.machine.E.locals frame_local === Some (S.I32 frame_base)
      && L.get state.X.machine.E.locals heap_local === Some (S.I32 heap_base)
      && Copy.apply fragment.copies state.X.memory frame_base heap_base === Some copied
      && M.store copied heap_base (zero ()) (S.I64 (Hmc_wasm_header_words.tag ())) === Some tagged
      && M.store tagged heap_base (PC.offset ()) (S.I64 (Header.number fragment.code)) === Some after} ->
    {u : unit | X.run (emit fragment frame_local heap_local) state === X.Done {X.memory = after; machine = state.X.machine}} @ ghost =
  fun fragment frame_local heap_local state frame_base heap_base copied tagged after premise -> ghost_ (
    Copy.correct fragment.copies frame_local heap_local state frame_base heap_base copied ();
    let middle = {X.memory = copied; machine = state.X.machine} in
    Write.correct (zero ()) heap_local (Hmc_wasm_header_words.tag ()) middle heap_base tagged ();
    PC.correct fragment.code heap_local {X.memory = tagged; machine = state.X.machine} heap_base after ();
    X.append_correct (Write.emit (zero ()) heap_local (Hmc_wasm_header_words.tag ())) (PC.emit fragment.code heap_local) middle;
    emit_def fragment frame_local heap_local;
    X.append_correct (Copy.emit fragment.copies frame_local heap_local)
      (E.append (Write.emit (zero ()) heap_local (Hmc_wasm_header_words.tag ())) (PC.emit fragment.code heap_local)) state)

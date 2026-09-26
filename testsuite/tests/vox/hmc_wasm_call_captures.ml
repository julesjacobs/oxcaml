module B = Wasm_u32
module K = Hmc_closure_ir
module Codec = Hmc_pointer_frame_codec
module Index = Hmc_u32_index
module Relayout = Hmc_wasm_relayout
module Geometry = Hmc_wasm_relayout_geometry
module Plan = Wasm_parallel_copy
module Copy = Wasm_cross_copy
let[@def] (position @ total) (recursive : bool) : Relayout.count = if recursive then 4 else 3
type fragment = {copies : Plan.plan; recursive : bool}
let[@def] (matches @ total) (entry : K.entry @ immutable) (capacity : Relayout.count) (fragment : fragment @ immutable) = ghost_ (
  fragment.recursive === entry.K.recursive && Index.fits (Codec.locals_size entry.K.captured) capacity
  && position entry.K.recursive + Geometry.size (Codec.locals_size entry.K.captured) <= capacity
  && Relayout.range_is fragment.copies 0 (position entry.K.recursive) (Codec.locals_size entry.K.captured) Plan.End)
let[@def] (encodable @ total) (entry : K.entry @ immutable) (capacity : Relayout.count) = ghost_ (
  Index.fits (Codec.locals_size entry.K.captured) capacity
  && position entry.K.recursive + Geometry.size (Codec.locals_size entry.K.captured) <= capacity)
let (build @ total) : (entry : K.entry) @ immutable -> (capacity : Relayout.count) ->
    {out : fragment option | match out with None -> not (encodable entry capacity) | Some fragment -> encodable entry capacity && matches entry capacity fragment} @ immutable =
  fun entry capacity ->
    ghost_ (encodable_def entry capacity);
    match Index.encode capacity (Codec.locals_size entry.K.captured) with
    | None -> None
    | Some count ->
      let target = position entry.K.recursive in
      ghost_ (Geometry.size_represents (Codec.locals_size entry.K.captured) count ());
      if target + count > capacity then None else
      let copies = Relayout.range 0 target count Plan.End (Codec.locals_size entry.K.captured) () in
      let fragment = {copies; recursive = entry.K.recursive} in
      ghost_ (Geometry.size_represents (Codec.locals_size entry.K.captured) count (); matches_def entry capacity fragment);
      Some fragment
let[@def] (emit @ total) (fragment : fragment @ immutable) (object_local : B.u32) (frame_local : B.u32) =
  Copy.emit fragment.copies object_local frame_local

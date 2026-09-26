module B = Wasm_u32
module W = Hmc_word64
module D = Hm_declarative
module G = Hmc_cfg_ir
module Codec = Hmc_pointer_frame_codec
module Index = Hmc_u32_index
module Relayout = Hmc_wasm_relayout
module Geometry = Hmc_wasm_relayout_geometry
module Copy = Wasm_parallel_copy
type fragment = {head_tag : B.u32; head_payload : B.u32; copies : Copy.plan; pc : W.limb; required : Relayout.count}
let[@def] (matches @ total) (signature : G.signature @ immutable) (next : D.index @ immutable)
    (capacity : Relayout.count) (max_pc : W.limb) (fragment : fragment @ immutable) = ghost_ (
  match signature.G.temporaries with
  | G.Value (context, _, schema) ->
    let env_size = Codec.locals_size signature.G.locals in
    let saved_size = Codec.locals_size context in
    let rest_size = Codec.temporaries_size schema in
    let env = Geometry.size env_size in let saved = Geometry.size saved_size in let rest = Geometry.size rest_size in
    fragment.head_tag = 48 + 16 * env && fragment.head_payload = 56 + 16 * env
    && Index.represents next fragment.pc && fragment.pc <= max_pc
    && Index.fits env_size capacity && Index.fits saved_size capacity && Index.fits rest_size capacity
    && 3 + env + saved + rest <= capacity && fragment.required = 2 + saved + rest && fragment.required <= capacity
    && Geometry.two fragment.copies (3 + env) 2 saved_size (3 + env + saved) (2 + saved) rest_size
  | _ -> false)
let[@def] (encodable @ total) (signature : G.signature @ immutable) (next : D.index @ immutable)
    (capacity : Relayout.count) (max_pc : W.limb) = ghost_ (
  match signature.G.temporaries with
  | G.Value (context, _, schema) ->
    let env_size = Codec.locals_size signature.G.locals in
    let saved_size = Codec.locals_size context in
    let rest_size = Codec.temporaries_size schema in
    Index.fits env_size capacity && Index.fits saved_size capacity && Index.fits rest_size capacity
    && Index.fits next max_pc
    && 3 + Geometry.size env_size + Geometry.size saved_size + Geometry.size rest_size <= capacity
    && 2 + Geometry.size saved_size + Geometry.size rest_size <= capacity
  | _ -> false)
let (build @ total) : (signature : G.signature) @ immutable -> (next : D.index) @ immutable ->
    (capacity : Relayout.count) -> (max_pc : W.limb) ->
    {out : fragment option | match out with None -> not (encodable signature next capacity max_pc)
      | Some fragment -> encodable signature next capacity max_pc && matches signature next capacity max_pc fragment} @ immutable =
  fun signature next capacity max_pc ->
  ghost_ (encodable_def signature next capacity max_pc);
  match signature.G.temporaries with
  | G.Value (context, _, schema) ->
    (match Index.encode capacity (Codec.locals_size signature.G.locals), Index.encode capacity (Codec.locals_size context),
        Index.encode capacity (Codec.temporaries_size schema), Index.encode max_pc next with
    | Some env, Some saved, Some rest, Some pc ->
      ghost_ (Geometry.size_represents (Codec.locals_size signature.G.locals) env ();
        Geometry.size_represents (Codec.locals_size context) saved ();
        Geometry.size_represents (Codec.temporaries_size schema) rest ());
      let required = 2 + saved + rest in
      if 3 + env + saved + rest > capacity || required > capacity then None else
      let tail = Relayout.range (3 + env + saved) (2 + saved) rest Copy.End (Codec.temporaries_size schema) () in
      let copies = Relayout.range (3 + env) 2 saved tail (Codec.locals_size context) () in
      let fragment = {head_tag = 48 + 16 * env; head_payload = 56 + 16 * env; copies; pc; required} in
      ghost_ (Geometry.size_represents (Codec.locals_size signature.G.locals) env ();
        Geometry.size_represents (Codec.locals_size context) saved ();
        Geometry.size_represents (Codec.temporaries_size schema) rest ();
        Geometry.two_def copies (3 + env) 2 (Codec.locals_size context) (3 + env + saved) (2 + saved) (Codec.temporaries_size schema);
        matches_def signature next capacity max_pc fragment);
      Some fragment
    | _ -> None)
  | _ -> None
let[@def] (moves @ total) (fragment : fragment @ immutable) : Relayout.fragment @ immutable =
  {Relayout.copies = fragment.copies; pc = fragment.pc; required = fragment.required}

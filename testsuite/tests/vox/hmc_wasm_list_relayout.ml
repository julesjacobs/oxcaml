module B = Wasm_u32
module W = Hmc_word64
module D = Hm_declarative
module G = Hmc_cfg_ir
module Codec = Hmc_pointer_frame_codec
module Index = Hmc_u32_index
module Copy = Wasm_parallel_copy
module Geometry = Hmc_wasm_relayout_geometry
module Relayout = Hmc_wasm_relayout
let[@def] (matches @ total) (signature : G.signature @ immutable) (next : D.index @ immutable)
    (capacity : Relayout.count) (max_pc : W.limb) (fragment : Relayout.fragment @ immutable) = ghost_ (
  let env_size = Codec.locals_size signature.G.locals in
  let temp_size = Codec.temporaries_size signature.G.temporaries in
  let env = Geometry.size env_size in let temps = Geometry.size temp_size in
  Index.fits env_size capacity && Index.fits temp_size capacity
  && Index.represents next fragment.Relayout.pc && fragment.Relayout.pc <= max_pc
  && fragment.Relayout.required = 4 + 2 * env + temps && fragment.Relayout.required <= capacity
  && Geometry.four fragment.Relayout.copies 2 4 env_size 2 (4 + env) env_size
    (2 + env) (4 + 2 * env) temp_size 0 (4 + 2 * env + temps) D.Z)
let[@def] (encodable @ total) (signature : G.signature @ immutable) (next : D.index @ immutable)
    (capacity : Relayout.count) (max_pc : W.limb) = ghost_ (
  Index.fits (Codec.locals_size signature.G.locals) capacity
  && Index.fits (Codec.temporaries_size signature.G.temporaries) capacity
  && Index.fits next max_pc
  && 4 + 2 * Geometry.size (Codec.locals_size signature.G.locals)
    + Geometry.size (Codec.temporaries_size signature.G.temporaries) <= capacity)
let (build @ total) : (signature : G.signature) @ immutable -> (next : D.index) @ immutable ->
    (capacity : Relayout.count) -> (max_pc : W.limb) ->
    {out : Relayout.fragment option | match out with None -> not (encodable signature next capacity max_pc)
      | Some fragment -> encodable signature next capacity max_pc && matches signature next capacity max_pc fragment} @ immutable =
  fun signature next capacity max_pc ->
    ghost_ (encodable_def signature next capacity max_pc);
    match Index.encode capacity (Codec.locals_size signature.G.locals), Index.encode capacity (Codec.temporaries_size signature.G.temporaries), Index.encode max_pc next with
    | Some env, Some temps, Some pc ->
      ghost_ (Geometry.size_represents (Codec.locals_size signature.G.locals) env ();
        Geometry.size_represents (Codec.temporaries_size signature.G.temporaries) temps ());
      let required = 4 + 2 * env + temps in
      if required > capacity then None else
      let old = Relayout.range (2 + env) (4 + 2 * env) temps Copy.End (Codec.temporaries_size signature.G.temporaries) () in
      let saved = Relayout.range 2 (4 + env) env old (Codec.locals_size signature.G.locals) () in
      let copies = Relayout.range 2 4 env saved (Codec.locals_size signature.G.locals) () in
      let fragment = {Relayout.copies; pc; required} in
      ghost_ (Geometry.size_represents (Codec.locals_size signature.G.locals) env ();
        Geometry.size_represents (Codec.temporaries_size signature.G.temporaries) temps ();
        Geometry.split_def Copy.End 0 required D.Z;
        Geometry.two_def old (2 + env) (4 + 2 * env) (Codec.temporaries_size signature.G.temporaries) 0 required D.Z;
        Geometry.four_def copies 2 4 (Codec.locals_size signature.G.locals) 2 (4 + env) (Codec.locals_size signature.G.locals)
          (2 + env) (4 + 2 * env) (Codec.temporaries_size signature.G.temporaries) 0 required D.Z;
        matches_def signature next capacity max_pc fragment);
      Some fragment
    | _ -> None

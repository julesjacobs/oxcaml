module W = Hmc_word64
module D = Hm_declarative
module G = Hmc_cfg_ir
module Index = Hmc_u32_index
module Write = Hmc_wasm_closure_write
module Relayout = Hmc_wasm_relayout
type fragment = {object_ : Write.fragment; pc : W.limb}
let[@def] (matches @ total) (signature : G.signature @ immutable) (id : D.index @ immutable) (next : D.index @ immutable)
    (capacity : Relayout.count) (maximum : W.limb) (fragment : fragment @ immutable) = ghost_ (
  Write.matches signature.G.locals id capacity maximum fragment.object_ && Index.represents next fragment.pc && fragment.pc <= maximum)
let[@def] (encodable @ total) (signature : G.signature @ immutable) (id : D.index @ immutable) (next : D.index @ immutable)
    (capacity : Relayout.count) (maximum : W.limb) = ghost_ (
  Write.encodable signature.G.locals id capacity maximum && Index.fits next maximum)
let (build @ total) : (signature : G.signature) @ immutable -> (id : D.index) @ immutable -> (next : D.index) @ immutable ->
    (capacity : Relayout.count) -> (maximum : W.limb) ->
    {out : fragment option | match out with None -> not (encodable signature id next capacity maximum)
      | Some fragment -> encodable signature id next capacity maximum && matches signature id next capacity maximum fragment} @ immutable =
  fun signature id next capacity maximum ->
    ghost_ (encodable_def signature id next capacity maximum);
    match Write.build signature.G.locals id capacity maximum, Index.encode maximum next with
    | Some object_, Some pc ->
      let fragment = {object_; pc} in
      ghost_ (matches_def signature id next capacity maximum fragment); Some fragment
    | _ -> None

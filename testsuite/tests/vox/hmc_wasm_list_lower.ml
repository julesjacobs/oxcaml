module W = Hmc_word64
module D = Hm_declarative
module G = Hmc_cfg_ir
module Index = Hmc_u32_index
module Relayout = Hmc_wasm_relayout
module Full = Hmc_wasm_list_relayout
type fragment = {empty_pc : W.limb; full : Relayout.fragment}
let[@def] (matches @ total) (signature : G.signature @ immutable) (empty : D.index @ immutable) (next : D.index @ immutable)
    (capacity : Relayout.count) (max_pc : W.limb) (fragment : fragment @ immutable) = ghost_ (
  (match signature.G.accumulator with Some (D.List_type _) -> true | _ -> false)
  && Index.represents empty fragment.empty_pc && fragment.empty_pc <= max_pc
  && Full.matches signature next capacity max_pc fragment.full)
let[@def] (encodable @ total) (signature : G.signature @ immutable) (empty : D.index @ immutable) (next : D.index @ immutable)
    (capacity : Relayout.count) (max_pc : W.limb) = ghost_ (
  (match signature.G.accumulator with Some (D.List_type _) -> true | _ -> false)
  && Index.fits empty max_pc && Full.encodable signature next capacity max_pc)
let (build @ total) : (signature : G.signature) @ immutable -> (empty : D.index) @ immutable -> (next : D.index) @ immutable ->
    (capacity : Relayout.count) -> (max_pc : W.limb) ->
    {out : fragment option | match out with None -> not (encodable signature empty next capacity max_pc)
      | Some fragment -> encodable signature empty next capacity max_pc && matches signature empty next capacity max_pc fragment} @ immutable =
  fun signature empty next capacity max_pc ->
    ghost_ (encodable_def signature empty next capacity max_pc);
    match signature.G.accumulator with
    | Some (D.List_type _) -> (match Index.encode max_pc empty, Full.build signature next capacity max_pc with
      | Some empty_pc, Some full ->
        let fragment = {empty_pc; full} in
        ghost_ (matches_def signature empty next capacity max_pc fragment); Some fragment
      | _ -> None)
    | _ -> None

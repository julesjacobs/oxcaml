module B = Wasm_u32
module W = Hmc_word64
module G = Hmc_cfg_ir
module V = Hmc_tagged_cell
module Machine = Hmc_heap_machine
module Index = Hmc_u32_index
module Literal = Hmc_wasm_literal_load
type fragment = {value : V.value; pc : W.limb}
let[@def] (corresponds @ total) (globals : Machine.globals @ immutable) (instruction : G.instruction @ immutable)
    (fragment : fragment @ immutable) = ghost_ (match instruction with
  | G.Load (G.Global index, _, _, next) -> Machine.global globals index === Some fragment.value && Index.represents next fragment.pc
  | _ -> false)
let[@def] (encodable @ total) (globals : Machine.globals @ immutable) (instruction : G.instruction @ immutable) (max_pc : W.limb) = ghost_ (
  match instruction with
  | G.Load (G.Global index, _, _, next) -> (match Machine.global globals index with None -> false | Some _ -> Index.fits next max_pc)
  | _ -> false)
let (lower @ total) : (globals : Machine.globals) @ immutable -> (instruction : G.instruction) @ immutable -> (max_pc : W.limb) ->
    {out : fragment option | match out with None -> not (encodable globals instruction max_pc) | Some fragment -> encodable globals instruction max_pc && corresponds globals instruction fragment && fragment.pc <= max_pc} @ immutable =
  fun globals instruction max_pc ->
  ghost_ (encodable_def globals instruction max_pc);
  match instruction with
  | G.Load (G.Global index, _, _, next) -> (match Machine.global globals index, Index.encode max_pc next with
    | Some value, Some pc -> let fragment = {value; pc} in ghost_ (corresponds_def globals instruction fragment); Some fragment
    | _ -> None)
  | _ -> None
let[@def] (emit @ total) (fragment : fragment @ immutable) (base_local : B.u32) = Literal.emit fragment.pc fragment.value base_local

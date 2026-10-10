module B = Wasm_u32
module W = Hmc_word64
module G = Hmc_cfg_ir
module Simple = Hmc_wasm_simple_lower
module Relayout = Hmc_wasm_relayout
module Global = Hmc_wasm_global_lower
module Machine = Hmc_heap_machine
module Primitive = Hmc_wasm_primitive_lower
module Geometry = Hmc_wasm_relayout_geometry
type fragment = Simple of Simple.fragment | Relayout of Relayout.fragment | Primitive of Primitive.fragment | Global of Global.fragment [@@inductive]
let[@def] (corresponds @ total) (globals : Machine.globals @ immutable) (signature : G.signature @ immutable) (instruction : G.instruction @ immutable)
    (capacity : Relayout.count) (max_pc : W.limb) (fragment : fragment @ immutable) = ghost_ (match fragment with
  | Simple simple -> Simple.corresponds instruction simple
  | Relayout relayout -> Geometry.matches signature instruction capacity max_pc relayout.Relayout.copies relayout.Relayout.pc relayout.Relayout.required
  | Primitive primitive -> Primitive.matches signature instruction capacity max_pc primitive
  | Global global -> Global.corresponds globals instruction global)
let[@def] (encodable @ total) (globals : Machine.globals @ immutable) (signature : G.signature @ immutable) (instruction : G.instruction @ immutable)
    (capacity : Relayout.count) (max_pc : W.limb) = ghost_ (
  match instruction with
  | G.Save_environment _ | G.Save_value _ | G.Bind _ | G.Restore _ -> Relayout.encodable signature instruction capacity max_pc
  | G.Primitive _ -> Primitive.encodable signature instruction capacity max_pc
  | G.Load (G.Global _, _, _, _) -> Global.encodable globals instruction max_pc
  | _ -> Simple.admitted instruction max_pc)
let (lower @ total) : (globals : Machine.globals) @ immutable -> (signature : G.signature) @ immutable -> (instruction : G.instruction) @ immutable ->
    (capacity : Relayout.count) -> (max_pc : W.limb) ->
    {out : fragment option | match out with None -> not (encodable globals signature instruction capacity max_pc) | Some fragment -> encodable globals signature instruction capacity max_pc && corresponds globals signature instruction capacity max_pc fragment} @ immutable =
  fun globals signature instruction capacity max_pc ->
    ghost_ (encodable_def globals signature instruction capacity max_pc);
    match instruction with
    | G.Save_environment _ | G.Save_value _ | G.Bind _ | G.Restore _ ->
      (match Relayout.build signature instruction capacity max_pc with
      | None -> None
      | Some relayout -> let fragment = Relayout relayout in ghost_ (corresponds_def globals signature instruction capacity max_pc fragment); Some fragment)
    | G.Primitive _ ->
      (match Primitive.build signature instruction capacity max_pc with
      | None -> None
      | Some primitive -> let fragment = Primitive primitive in ghost_ (corresponds_def globals signature instruction capacity max_pc fragment); Some fragment)
    | G.Load (G.Global _, _, _, _) ->
      (match Global.lower globals instruction max_pc with
      | None -> None
      | Some global -> let fragment = Global global in ghost_ (corresponds_def globals signature instruction capacity max_pc fragment); Some fragment)
    | _ -> (match Simple.lower instruction max_pc with
      | None -> None
      | Some simple -> let fragment = Simple simple in ghost_ (corresponds_def globals signature instruction capacity max_pc fragment); Some fragment)
let[@def] (emit @ total) (fragment : fragment @ immutable) (base_local : B.u32) = match fragment with
  | Simple simple -> Simple.emit simple base_local
  | Relayout relayout -> Relayout.emit relayout base_local
  | Primitive primitive -> Primitive.emit primitive base_local
  | Global global -> Global.emit global base_local

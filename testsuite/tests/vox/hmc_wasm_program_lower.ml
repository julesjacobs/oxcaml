module B = Wasm_u32
module I = Hmc_tail_ir
module C = Hmc_cfg_program
module K = Hmc_closure_program
module D = Hm_declarative
module R = Hmc_wasm_relayout
module Cap = Hmc_frame_capacity
module Index = Hmc_u32_index
module Table = Hmc_wasm_program_table
module Plans = Hmc_wasm_call_plan_table
module Restore = Hmc_wasm_frame_restore
module Machine = Hmc_heap_machine
type program = {blocks : Table.table; calls : Plans.table; restore : Wasm_parallel_copy.plan; capacity : R.count; width : B.u32}
let[@def] (corresponds @ total) (source : I.program @ immutable) (globals : Machine.globals @ immutable)
    (max_pc : B.u32) (target : program @ immutable) = ghost_ (
  Index.represents (Cap.capacity source.I.origin.C.blocks) target.capacity
  && target.capacity < 268435452 && target.width = 16 + 16 * target.capacity
  && Table.corresponds globals source.I.origin.C.blocks source.I.code target.blocks target.capacity max_pc
  && Plans.related source.I.origin.C.origin.K.table target.capacity target.calls
  && Restore.matches target.restore (Cap.capacity source.I.origin.C.blocks))
let[@def] (encodable @ total) (source : I.program @ immutable) (globals : Machine.globals @ immutable) (max_pc : B.u32) = ghost_ (
  let capacity_index = Cap.capacity source.I.origin.C.blocks in
  let capacity = Hmc_wasm_relayout_geometry.size capacity_index in
  Index.fits capacity_index 268435451 &&
  if capacity < 0 || capacity > 268435451 then false else
    Table.encodable globals source.I.origin.C.blocks source.I.code capacity max_pc
    && Plans.encodable source.I.origin.C.origin.K.table capacity max_pc)
let (lower @ total) : (source : I.program) @ immutable -> (globals : Machine.globals) @ immutable -> (max_pc : B.u32) ->
    {out : program option | match out with None -> not (encodable source globals max_pc) | Some target -> encodable source globals max_pc && corresponds source globals max_pc target} @ immutable =
  fun source globals max_pc ->
  ghost_ (encodable_def source globals max_pc);
  let capacity_index = Cap.capacity source.I.origin.C.blocks in
  match Index.encode 268435451 capacity_index with
  | None -> None
  | Some capacity ->
    ghost_ (Hmc_wasm_relayout_geometry.size_represents capacity_index capacity ());
    match Table.lower globals source.I.origin.C.blocks source.I.code capacity_index capacity max_pc (),
      Plans.build source.I.origin.C.origin.K.table capacity max_pc with
    | Some blocks, Some calls ->
      let restore = Restore.build capacity_index capacity () in
      let target = {blocks; calls; restore; capacity; width = 16 + 16 * capacity} in
      ghost_ (corresponds_def source globals max_pc target); Some target
    | _ -> None

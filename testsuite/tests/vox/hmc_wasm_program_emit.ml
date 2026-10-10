module B = Wasm_u32
module T = Wasm_control
module WI = Wasm_instruction
module Block = Hmc_wasm_program_block
module Lower = Hmc_wasm_program_lower
module Structured = Hmc_wasm_structured_block
module Slots = Hmc_wasm_descriptor_load
module Lift = Wasm_control_lift
type locals = {structured : Structured.locals; top : B.u32; stack_limit : B.u32;
  code : B.u32; address : B.u32; descriptor : Slots.slots;
  status : B.u32; result_tag : B.u32; result_payload : B.u32}
let[@def] (status @ total) (local : B.u32) (value : B.u32) (tail : T.code @ immutable) =
  T.Instruction (WI.I32_const value, T.Instruction (WI.Local_set local, tail))
let[@def] (protected @ total) (local : B.u32) (failure : B.u32) (body : T.code @ immutable) =
  T.Block (status local failure (T.Block (body, status local 0 T.Empty)), T.Empty)
let[@def] (emit @ total) (program : Lower.program @ immutable) (fragment : Block.fragment @ immutable)
    (locals : locals @ immutable) (table_base : B.u32) (stack_base : B.u32) =
  let base = locals.structured in
  match fragment with
  | Block.Structured body ->
    protected locals.status 2 (Structured.emit body base 2)
  | Block.Call call ->
    protected locals.status 3
      (Hmc_wasm_ordinary_call.emit call.Block.save call.Block.padding base.Structured.frame locals.top
        program.Lower.width locals.stack_limit 2 call.Block.environment base.Structured.scratch program.Lower.calls
        table_base locals.code locals.address locals.descriptor base.Structured.object_)
  | Block.Tail_call environment ->
    protected locals.status 0
      (Hmc_wasm_loaded_call.emit environment base.Structured.scratch program.Lower.calls table_base locals.code
        locals.address locals.descriptor base.Structured.object_ base.Structured.frame)
  | Block.Return ->
    let root = Lift.embed
      (Wasm_frame_snapshot.emit (Hmc_wasm_root_return.reads locals.result_tag locals.result_payload) base.Structured.frame)
      (status locals.status 1 T.Empty) in
    status locals.status 0 (Hmc_wasm_caller_return.emit program.Lower.restore program.Lower.capacity
      base.Structured.frame locals.top stack_base root T.Empty)

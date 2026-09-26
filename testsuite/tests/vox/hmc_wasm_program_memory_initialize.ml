module B = Wasm_u32
module D = Hm_declarative
module G = Hmc_cfg_ir
module H = Hmc_heap_objects
module F = Hmc_heap_frame
module Q = Hmc_heap_state
module Machine = Hmc_heap_machine
module Program = Hmc_tail_ir
module Registers = Hmc_wasm_program_registers
module Resources = Hmc_wasm_program_resources
module Frame = Hmc_wasm_program_frame
module Store = Hmc_wasm_program_frame_store
module Codec = Hmc_pointer_frame_codec
module Index = Hmc_u32_index
module Bounds = Hmc_linear_bounds
module V = Hmc_tagged_cell
let (initialize @ total) : (program : Program.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (width : B.u32) -> (stack_base : B.u32) -> (frame_end : B.u32) -> (heap_base : B.u32) -> (code_capacity : B.u32) ->
    (abstract : Hmc_cfg_semantics.state) @ immutable -> (heap : H.heap) @ immutable -> (activation : F.activation) @ immutable ->
    (registers : Registers.registers) @ immutable -> (memory : B.bytes) @ immutable ->
    (signature : G.signature) @ immutable -> (padding : H.cells) @ immutable -> (pc : B.u32) -> (count : B.u32) ->
    {u : unit | Hmc_heap_invariant.valid program globals registers.Registers.heap_limit {Machine.heap; state = Q.Running (activation, Q.Halt)} abstract
      && Hmc_heap_preservation.extends heap (H.Empty_heap heap_base) && frame_end <= heap_base && frame_end <= stack_base
      && Index.fits (Hmc_closure_ir.size program.Program.origin.Hmc_cfg_program.origin.Hmc_closure_program.table) code_capacity
      && registers.Registers.heap = H.used heap && registers.Registers.top = stack_base
      && stack_base <= registers.Registers.stack_limit && registers.Registers.stack_limit <= heap_base
      && Bounds.covers memory registers.Registers.heap_limit && Bounds.covers memory registers.Registers.stack_limit
      && Bounds.covers memory frame_end && Codec.shape signature activation && Index.represents activation.F.pc pc
      && Index.represents (D.S (D.add (Codec.size signature) (H.length padding))) count
      && registers.Registers.frame <= 4294967248 && frame_end = registers.Registers.frame + 16 * count} ->
    {out : Store.result | Resources.valid program globals width stack_base frame_end abstract heap activation Q.Halt registers out.Store.memory
      && Frame.valid signature activation registers out.Store.memory frame_end pc out.Store.cells padding out.Store.bytes out.Store.suffix count
      && V.length out.Store.memory === V.length memory} @ immutable =
  fun program globals width stack_base frame_end heap_base code_capacity abstract heap activation registers memory signature padding pc count premise ->
    let table = program.Program.origin.Hmc_cfg_program.origin.Hmc_closure_program.table in
    ghost_ (
      Hmc_heap_invariant.valid_def program globals registers.Registers.heap_limit {Machine.heap; state = Q.Running (activation, Q.Halt)} abstract;
      Hmc_heap_code_bounds.encodable table code_capacity heap ();
      Hmc_wasm_heap_suffix.origin table heap heap_base frame_end ();
      Hmc_wasm_heap_suffix.origin table heap heap_base registers.Registers.stack_limit ());
    let materialized = Hmc_heap_image.materialize table code_capacity memory heap registers.Registers.heap_limit () in
    ghost_ (
      Bounds.same_length memory materialized registers.Registers.stack_limit ();
      Bounds.same_length memory materialized frame_end ();
      Hmc_memory_stack.related_def program.Program.origin.Hmc_cfg_program.blocks width materialized stack_base registers.Registers.top Q.Halt;
      Resources.valid_def program globals width stack_base frame_end abstract heap activation Q.Halt registers materialized);
    Store.store program globals width stack_base frame_end abstract heap activation Q.Halt registers materialized signature padding pc count ()

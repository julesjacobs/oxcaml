module B = Wasm_u32
module D = Hm_declarative
module G = Hmc_cfg_ir
module I = Hmc_tail_ir
module U = Hmc_tail_semantics
module H = Hmc_heap_objects
module F = Hmc_heap_frame
module Q = Hmc_heap_state
module Machine = Hmc_heap_machine
module Lower = Hmc_wasm_program_lower
module Registers = Hmc_wasm_program_registers
module Assembly = Hmc_wasm_program_functions
module Runtime = Hmc_wasm_program_runtime
module Resources = Hmc_wasm_program_resources
module Frame = Hmc_wasm_program_frame
module Descriptors = Hmc_wasm_program_descriptors
module Index = Hmc_u32_index
type context = {input : Hmc_word64.t; max_pc : B.u32; block_count : B.u32;
  table_base : B.u32; table_count : Hmc_runtime_descriptor_table.count;
  runtime : Hmc_runtime_closures.table; stack_base : B.u32;
  stack_capacity : D.index; host_capacity : Wasm_code.count}
type running = {abstract : Hmc_cfg_semantics.state; elapsed : D.index;
  heap : H.heap; activation : F.activation; frames : Q.frames; block : G.block;
  registers : Registers.registers; memory : B.bytes; pc : B.u32;
  cells : H.cells; padding : H.cells; bytes : B.bytes; suffix : B.bytes;
  frame_end : B.u32; cell_count : B.u32}
let[@def] (valid @ total) (program : I.program @ immutable) (globals : Machine.globals @ immutable)
    (lowered : Lower.program @ immutable) (context : context @ immutable) (state : running @ immutable) = ghost_ (
  Lower.corresponds program globals context.max_pc lowered
  && Index.represents (G.size program.I.origin.Hmc_cfg_program.blocks) context.block_count
  && state.abstract === U.advance program state.elapsed (U.initial program context.input)
  && G.lookup program.I.origin.Hmc_cfg_program.blocks state.activation.F.pc === Some state.block
  && Resources.valid program globals lowered.Lower.width context.stack_base state.frame_end state.abstract
    state.heap state.activation state.frames state.registers state.memory
  && Descriptors.valid program state.registers state.memory context.runtime context.table_base context.table_count
  && Frame.valid state.block.G.signature state.activation state.registers state.memory state.frame_end state.pc
    state.cells state.padding state.bytes state.suffix state.cell_count
  && Index.represents (H.length state.cells) lowered.Lower.capacity
  && Hmc_memory_stack_capacity.region lowered.Lower.width context.stack_capacity context.stack_base state.registers.Registers.stack_limit
  && state.registers.Registers.frame <= 4294967216)
let[@def] (module_ @ total) (program : I.program @ immutable) (lowered : Lower.program @ immutable) (context : context @ immutable) =
  Assembly.assemble lowered (G.size program.I.origin.Hmc_cfg_program.blocks) context.block_count
    (Runtime.config context.table_base context.stack_base) (Runtime.dispatcher ())
let[@def] (loop @ total) (context : context @ immutable) (state : running @ immutable) =
  Hmc_wasm_program_dispatch.loop (Registers.globals state.registers) state.memory (Wasm_code.Succ context.host_capacity)
let[@def] (configuration @ total) (state : running @ immutable) =
  {Machine.heap = state.heap; state = Q.Running (state.activation, state.frames)}
type transition = {state : running; fuel : {n : Wasm_code.count | not (n === Wasm_code.Zero)}}

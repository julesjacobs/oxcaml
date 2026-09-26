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
module Runtime = Hmc_runtime_closures
module Table = Hmc_runtime_descriptor_table
module Index = Hmc_u32_index
module Bounds = Hmc_linear_bounds
module Bytes = Hmc_linear_bytes
module V = Hmc_tagged_cell
let[@def] (valid @ total) (program : Program.program @ immutable) (registers : Registers.registers @ immutable)
    (memory : B.bytes @ immutable) (runtime : Runtime.table @ immutable) (table_base : B.u32) (table_count : Table.count) = ghost_ (
  Runtime.related program.Program.origin.Hmc_cfg_program.origin.Hmc_closure_program.table program.Program.origin.Hmc_cfg_program.functions runtime
  && Table.related runtime memory table_base table_count && table_base + 32 * table_count <= registers.Registers.frame)
let (preserve @ total) : (program : Program.program) @ immutable -> (before : Registers.registers) @ immutable -> (after : Registers.registers) @ immutable ->
    (memory : B.bytes) @ immutable -> (updated : B.bytes) @ immutable -> (runtime : Runtime.table) @ immutable ->
    (table_base : B.u32) -> (table_count : Table.count) ->
    {u : unit | valid program before memory runtime table_base table_count && after.Registers.frame = before.Registers.frame
      && Hmc_linear_preservation.equal_prefix before.Registers.frame memory updated} ->
    {u : unit | valid program after updated runtime table_base table_count} @ ghost =
  fun program before after memory updated runtime table_base table_count premise -> ghost_ (
    valid_def program before memory runtime table_base table_count;
    Table.preserve runtime memory updated table_base table_count before.Registers.frame ();
    valid_def program after updated runtime table_base table_count)
let (install @ total) : (program : Program.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (width : B.u32) -> (stack_base : B.u32) -> (frame_end : B.u32) -> (abstract : Hmc_cfg_semantics.state) @ immutable ->
    (heap : H.heap) @ immutable -> (activation : F.activation) @ immutable -> (frames : Q.frames) @ immutable ->
    (registers : Registers.registers) @ immutable -> (before : Store.result) @ immutable ->
    (signature : G.signature) @ immutable -> (padding : H.cells) @ immutable -> (pc : B.u32) -> (count : B.u32) ->
    (runtime : Runtime.table) @ immutable -> (table_base : B.u32) -> (table_count : Table.count) ->
    {u : unit | Resources.valid program globals width stack_base frame_end abstract heap activation frames registers before.Store.memory
      && Frame.valid signature activation registers before.Store.memory frame_end pc before.Store.cells padding before.Store.bytes before.Store.suffix count
      && Runtime.related program.Program.origin.Hmc_cfg_program.origin.Hmc_closure_program.table program.Program.origin.Hmc_cfg_program.functions runtime
      && Index.represents (Runtime.size runtime) table_count && table_base + 32 * table_count <= registers.Registers.frame} ->
    {out : Store.result | Resources.valid program globals width stack_base frame_end abstract heap activation frames registers out.Store.memory
      && Frame.valid signature activation registers out.Store.memory frame_end pc out.Store.cells padding out.Store.bytes out.Store.suffix count
      && valid program registers out.Store.memory runtime table_base table_count
      && out.Store.cells === before.Store.cells && out.Store.bytes === before.Store.bytes && out.Store.suffix === before.Store.suffix
      && V.length out.Store.memory === V.length before.Store.memory} @ immutable =
  fun program globals width stack_base frame_end abstract heap activation frames registers before signature padding pc count runtime table_base table_count premise ->
    ghost_ (Frame.valid_def signature activation registers before.Store.memory frame_end pc before.Store.cells padding before.Store.bytes before.Store.suffix count;
      let full = H.Cell (V.Word (Hmc_wasm_header_update.number pc), before.Store.cells) in
      H.length_def full;
      Hmc_wasm_frame_preservation.suffix before.Store.memory registers.Registers.frame frame_end before.Store.bytes full before.Store.suffix count ());
    let memory = Table.store runtime before.Store.memory table_base table_count frame_end () in
    ghost_ (
      Table.address_def table_base table_count; Wasm_scalar.add32_def table_base (32 * table_count);
      let boundary = Table.address table_base table_count in
      let _ = Bounds.suffix before.Store.memory frame_end boundary () in
      Bounds.covers_def before.Store.memory boundary;
      Hmc_heap_image_suffix.seek before.Store.memory memory boundary registers.Registers.frame ();
      Hmc_heap_image_suffix.seek before.Store.memory memory boundary frame_end ();
      Resources.valid_def program globals width stack_base frame_end abstract heap activation frames registers before.Store.memory;
      Bounds.same_length before.Store.memory memory registers.Registers.heap_limit ();
      Bounds.same_length before.Store.memory memory registers.Registers.stack_limit ();
      Hmc_wasm_heap_suffix.preserve before.Store.memory memory heap frame_end ();
      Hmc_memory_stack.preserve_suffix program.Program.origin.Hmc_cfg_program.blocks width before.Store.memory memory stack_base registers.Registers.top frames frame_end ();
      Resources.valid_def program globals width stack_base frame_end abstract heap activation frames registers memory;
      Frame.valid_def signature activation registers memory frame_end pc before.Store.cells padding before.Store.bytes before.Store.suffix count;
      valid_def program registers memory runtime table_base table_count);
    {before with Store.memory}

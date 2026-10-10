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
module Codec = Hmc_pointer_frame_codec
module Index = Hmc_u32_index
module Bytes = Hmc_linear_bytes
module Bounds = Hmc_linear_bounds
module Wire = Hmc_heap_wire
module Header = Hmc_wasm_header_update
module V = Hmc_tagged_cell
type result = {memory : B.bytes; cells : H.cells; bytes : B.bytes; suffix : B.bytes}
let (store @ total) : (program : Program.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (width : B.u32) -> (stack_base : B.u32) -> (frame_end : B.u32) -> (abstract : Hmc_cfg_semantics.state) @ immutable ->
    (heap : H.heap) @ immutable -> (activation : F.activation) @ immutable -> (frames : Q.frames) @ immutable ->
    (registers : Registers.registers) @ immutable -> (memory : B.bytes) @ immutable ->
    (signature : G.signature) @ immutable -> (padding : H.cells) @ immutable -> (pc : B.u32) -> (count : B.u32) ->
    {u : unit | Resources.valid program globals width stack_base frame_end abstract heap activation frames registers memory
      && Codec.shape signature activation && Index.represents activation.F.pc pc
      && Index.represents (D.S (D.add (Codec.size signature) (H.length padding))) count
      && registers.Registers.frame <= 4294967248 && frame_end = registers.Registers.frame + 16 * count
      && Bounds.covers memory frame_end} ->
    {out : result | Resources.valid program globals width stack_base frame_end abstract heap activation frames registers out.memory
      && Frame.valid signature activation registers out.memory frame_end pc out.cells padding out.bytes out.suffix count
      && V.length out.memory === V.length memory
      && Hmc_linear_preservation.equal_prefix registers.Registers.frame memory out.memory
      && Bytes.drop memory frame_end === Some out.suffix && Bytes.drop out.memory frame_end === Some out.suffix} @ immutable =
  fun program globals width stack_base frame_end abstract heap activation frames registers memory signature padding pc count premise ->
    let suffix = Bounds.suffix memory frame_end frame_end () in
    let original = Bounds.suffix memory frame_end registers.Registers.frame () in
    let cells = Codec.encode signature activation padding () in
    let full = H.Cell (V.Word (Header.number pc), cells) in
    let bytes = Wire.encode_cells full suffix in
    let updated = Wasm_memory_splice.replace memory registers.Registers.frame original bytes () in
    ghost_ (
      H.length_def full;
      Hmc_wasm_frame_preservation.suffix updated registers.Registers.frame frame_end bytes full suffix count ();
      Hmc_wasm_frame_preservation.same_length memory updated frame_end suffix ();
      Resources.valid_def program globals width stack_base frame_end abstract heap activation frames registers memory;
      Bounds.same_length memory updated registers.Registers.heap_limit ();
      Bounds.same_length memory updated registers.Registers.stack_limit ();
      Hmc_wasm_heap_suffix.preserve memory updated heap frame_end ();
      Hmc_memory_stack.preserve_suffix program.Program.origin.Hmc_cfg_program.blocks width memory updated stack_base registers.Registers.top frames frame_end ();
      Resources.valid_def program globals width stack_base frame_end abstract heap activation frames registers updated;
      Frame.valid_def signature activation registers updated frame_end pc cells padding bytes suffix count);
    {memory = updated; cells; bytes; suffix}

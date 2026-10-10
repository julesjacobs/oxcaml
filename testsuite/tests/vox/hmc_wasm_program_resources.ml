module B = Wasm_u32
module D = Hm_declarative
module H = Hmc_heap_objects
module F = Hmc_heap_frame
module Q = Hmc_heap_state
module Machine = Hmc_heap_machine
module Program = Hmc_tail_ir
module G = Hmc_cfg_program
module S = Hmc_cfg_semantics
module Source = Hmc_heap_invariant
module Registers = Hmc_wasm_program_registers
module Bounds = Hmc_linear_bounds
module Bytes = Hmc_linear_bytes
module Image = Hmc_heap_image
module Stack = Hmc_memory_stack
module Suffix = Hmc_wasm_heap_suffix
module V = Hmc_tagged_cell
let[@def] (valid @ total) (program : Program.program @ immutable) (globals : Machine.globals @ immutable)
    (width : B.u32) (stack_base : B.u32) (frame_end : B.u32) (abstract : S.state @ immutable)
    (heap : H.heap @ immutable) (activation : F.activation @ immutable) (frames : Q.frames @ immutable)
    (registers : Registers.registers @ immutable) (memory : B.bytes @ immutable) = ghost_ (
  Source.valid program globals registers.Registers.heap_limit {Machine.heap; state = Q.Running (activation, frames)} abstract
  && registers.Registers.top <= registers.Registers.stack_limit && registers.Registers.stack_limit <= registers.Registers.heap
  && registers.Registers.heap = H.used heap && Suffix.above heap frame_end
  && Suffix.above heap registers.Registers.stack_limit && Image.related memory heap
  && Bounds.covers memory registers.Registers.heap_limit && Bounds.covers memory registers.Registers.stack_limit
  && Stack.related program.Program.origin.G.blocks width memory stack_base registers.Registers.top frames
  && frame_end <= stack_base)
let (preserve @ total) : (program : Program.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (width : B.u32) -> (stack_base : B.u32) -> (frame_end : B.u32) -> (abstract : S.state) @ immutable ->
    (heap : H.heap) @ immutable -> (activation : F.activation) @ immutable -> (next : F.activation) @ immutable ->
    (frames : Q.frames) @ immutable -> (capacity : D.index) @ immutable ->
    (before : Registers.registers) @ immutable -> (after : Registers.registers) @ immutable ->
    (memory : B.bytes) @ immutable -> (updated : B.bytes) @ immutable ->
    {u : unit | valid program globals width stack_base frame_end abstract heap activation frames before memory
      && Machine.step program globals before.Registers.heap_limit capacity {Machine.heap; state = Q.Running (activation, frames)} ===
        Machine.Advanced {Machine.heap; state = Q.Running (next, frames)}
      && after.Registers.heap = before.Registers.heap && after.Registers.heap_limit = before.Registers.heap_limit
      && after.Registers.stack_limit = before.Registers.stack_limit && after.Registers.top = before.Registers.top
      && V.length memory === V.length updated && Bounds.covers memory frame_end && Bounds.covers updated frame_end
      && Bytes.drop memory frame_end === Bytes.drop updated frame_end} ->
    {u : unit | valid program globals width stack_base frame_end (Hmc_tail_semantics.step program abstract) heap next frames after updated} @ ghost =
  fun program globals width stack_base frame_end abstract heap activation next frames capacity before after memory updated premise -> ghost_ (
    valid_def program globals width stack_base frame_end abstract heap activation frames before memory;
    Source.step program globals before.Registers.heap_limit capacity {Machine.heap; state = Q.Running (activation, frames)} abstract ();
    Bounds.same_length memory updated before.Registers.heap_limit ();
    Bounds.same_length memory updated before.Registers.stack_limit ();
    Suffix.preserve memory updated heap frame_end ();
    Stack.preserve_suffix program.Program.origin.G.blocks width memory updated stack_base before.Registers.top frames frame_end ();
    valid_def program globals width stack_base frame_end (Hmc_tail_semantics.step program abstract) heap next frames after updated)

let (allocate @ total) : (program : Program.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (width : B.u32) -> (stack_base : B.u32) -> (frame_end : B.u32) -> (abstract : S.state) @ immutable ->
    (heap : H.heap) @ immutable -> (allocation : Hmc_heap_allocate.allocation) @ immutable -> (object_ : H.object_) @ immutable ->
    (activation : F.activation) @ immutable -> (next : F.activation) @ immutable -> (frames : Q.frames) @ immutable ->
    (capacity : D.index) @ immutable -> (before : Registers.registers) @ immutable -> (after : Registers.registers) @ immutable ->
    (memory : B.bytes) @ immutable -> (allocated : B.bytes) @ immutable -> (updated : B.bytes) @ immutable ->
    {u : unit | valid program globals width stack_base frame_end abstract heap activation frames before memory
      && Hmc_heap_allocate.correct program.Program.origin.G.origin.Hmc_closure_program.table heap before.Registers.heap_limit object_
        (Hmc_heap_allocate.Allocated allocation)
      && Machine.step program globals before.Registers.heap_limit capacity {Machine.heap; state = Q.Running (activation, frames)} ===
        Machine.Advanced {Machine.heap = allocation.Hmc_heap_allocate.heap; state = Q.Running (next, frames)}
      && after.Registers.heap = H.used allocation.Hmc_heap_allocate.heap && after.Registers.heap_limit = before.Registers.heap_limit
      && after.Registers.stack_limit = before.Registers.stack_limit && after.Registers.top = before.Registers.top
      && frame_end <= before.Registers.heap
      && Image.related updated allocation.Hmc_heap_allocate.heap
      && Hmc_linear_preservation.equal_prefix before.Registers.heap memory allocated
      && V.length memory === V.length updated && Bounds.covers allocated frame_end && Bounds.covers updated frame_end
      && Bytes.drop allocated frame_end === Bytes.drop updated frame_end} ->
    {u : unit | valid program globals width stack_base frame_end (Hmc_tail_semantics.step program abstract)
      allocation.Hmc_heap_allocate.heap next frames after updated} @ ghost =
  fun program globals width stack_base frame_end abstract heap allocation object_ activation next frames capacity before after memory allocated updated premise -> ghost_ (
    valid_def program globals width stack_base frame_end abstract heap activation frames before memory;
    Hmc_heap_allocate.correct_def program.Program.origin.G.origin.Hmc_closure_program.table heap before.Registers.heap_limit object_
      (Hmc_heap_allocate.Allocated allocation);
    Hmc_heap_extent.ordered (H.slots object_) (H.used heap) (H.used allocation.Hmc_heap_allocate.heap) ();
    Suffix.above_def allocation.Hmc_heap_allocate.heap frame_end;
    Suffix.above_def allocation.Hmc_heap_allocate.heap after.Registers.stack_limit;
    Source.step program globals before.Registers.heap_limit capacity {Machine.heap; state = Q.Running (activation, frames)} abstract ();
    Bounds.same_length memory updated before.Registers.heap_limit ();
    Bounds.same_length memory updated before.Registers.stack_limit ();
    Stack.preserve program.Program.origin.G.blocks width memory allocated stack_base before.Registers.top frames before.Registers.heap ();
    Stack.preserve_suffix program.Program.origin.G.blocks width allocated updated stack_base before.Registers.top frames frame_end ();
    valid_def program globals width stack_base frame_end (Hmc_tail_semantics.step program abstract) allocation.Hmc_heap_allocate.heap next frames after updated)

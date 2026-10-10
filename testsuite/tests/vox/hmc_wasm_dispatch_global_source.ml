module B = Wasm_u32
module W = Hmc_word64
module D = Hm_declarative
module G = Hmc_cfg_ir
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module Frame = Hmc_heap_frame
module State = Hmc_heap_state
module Machine = Hmc_heap_machine
module Program = Hmc_tail_ir
module Codec = Hmc_pointer_frame_codec
module Wire = Hmc_heap_wire
module Header = Hmc_wasm_header_update
module Lower = Hmc_wasm_global_lower
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module Bytes = Hmc_linear_bytes
module Table = Hmc_wasm_table_lower
module Block = Hmc_wasm_block_lower
module Loop = Hmc_wasm_dispatch_loop
module Control = Wasm_control
type result = Hmc_wasm_global_invariant.result
let (correct @ total) : (program : Program.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (heap : Heap.heap) @ immutable -> (heap_limit : W.limb) -> (stack_limit : D.index) @ immutable ->
    (signature : G.signature) @ immutable -> (next_signature : G.signature) @ immutable ->
    (activation : Frame.activation) @ immutable -> (frames : State.frames) @ immutable ->
    (index : D.index) @ immutable -> (ty : D.mono) @ immutable -> (derivation : D.typing) @ immutable -> (next : D.index) @ immutable ->
    (table : Table.table) @ immutable -> (outer : Control.labels) @ immutable -> (fragment : Lower.fragment) @ immutable -> (old_pc : W.limb) -> (cells : Heap.cells) @ immutable -> (padding : Heap.cells) @ immutable ->
    (state : X.state) @ immutable -> (base_local : B.u32) -> (base : B.u32) -> (before_frame : B.bytes) @ immutable -> (tail : B.bytes) @ immutable ->
    {u : unit | Lower.corresponds globals (G.Load (G.Global index, ty, derivation, next)) fragment
      && Table.lookup table old_pc === Some (Block.Global fragment) && state.X.machine.E.stack === S.Empty
      && Program.lookup program.Program.code activation.Frame.pc === Some (Program.Keep (G.Load (G.Global index, ty, derivation, next)))
      && Hmc_u32_index.represents activation.Frame.pc old_pc
      && next_signature.G.locals === signature.G.locals && next_signature.G.temporaries === signature.G.temporaries
      && Codec.decode signature activation.Frame.pc cells === Some (activation, padding)
      && base <= 4294967248 && Wasm_locals.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && Bytes.drop state.X.memory base === Some before_frame
      && Wire.decode_cells (D.S (Heap.length cells)) before_frame === Some (Heap.Cell (V.Word (Header.number old_pc), cells), tail)} ->
    {out : result | Hmc_u32_index.represents out.activation.Frame.pc fragment.Lower.pc
      && Control.run (Loop.cost table old_pc base_local (Block.Global fragment)) (Loop.configuration table base_local outer state)
        === Control.Running (Loop.configuration table base_local outer {X.memory = out.memory; machine = state.X.machine})
      && X.run (Lower.emit fragment base_local) state === X.Done {X.memory = out.memory; machine = state.X.machine}
      && Machine.step program globals heap_limit stack_limit {Machine.heap; state = State.Running (activation, frames)}
        === Machine.Advanced {Machine.heap; state = State.Running (out.activation, frames)}
      && out.activation === {activation with Frame.pc = next; accumulator = fragment.Lower.value}
      && Codec.decode next_signature next out.cells === Some (out.activation, padding)
      && Heap.length out.cells === Heap.length cells && Bytes.drop out.memory base === Some out.bytes
      && Wire.decode_cells (D.S (Heap.length out.cells)) out.bytes === Some (Heap.Cell (V.Word (Header.number fragment.Lower.pc), out.cells), tail)} @ immutable =
  fun program globals heap heap_limit stack_limit signature next_signature activation frames index ty derivation next table outer fragment old_pc cells padding
      state base_local base before_frame tail premise ->
    let out = Hmc_wasm_global_invariant.correct program globals heap heap_limit stack_limit signature next_signature activation frames index ty derivation next
      fragment old_pc cells padding state base_local base before_frame tail () in
    ghost_ (Lower.corresponds_def globals (G.Load (G.Global index, ty, derivation, next)) fragment;
      Hmc_wasm_frame_pc.correct state.X.memory base before_frame old_pc cells tail ();
      Block.emit_def (Block.Global fragment) base_local;
      Loop.correct table old_pc base_local base (Block.Global fragment) outer state {X.memory = out.memory; machine = state.X.machine} ());
    out

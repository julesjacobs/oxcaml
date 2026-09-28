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
module Lower = Hmc_wasm_simple_lower
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module Bytes = Hmc_linear_bytes
type result = {memory : B.bytes; cells : Heap.cells; bytes : B.bytes; activation : Frame.activation}
let (correct @ total) : (program : Program.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (heap : Heap.heap) @ immutable -> (heap_limit : W.limb) -> (stack_limit : D.index) @ immutable ->
    (signature : G.signature) @ immutable -> (next_signature : G.signature) @ immutable ->
    (activation : Frame.activation) @ immutable -> (frames : State.frames) @ immutable ->
    (index : D.index) @ immutable -> (number : Lower.slot) -> (ty : D.mono) @ immutable -> (derivation : D.typing) @ immutable -> (next : D.index) @ immutable ->
    (value : V.value) @ immutable -> (pc : W.limb) -> (old_pc : W.limb) -> (cells : Heap.cells) @ immutable -> (padding : Heap.cells) @ immutable ->
    (state : X.state) @ immutable -> (base_local : B.u32) -> (base : B.u32) -> (before_frame : B.bytes) @ immutable -> (tail : B.bytes) @ immutable ->
    {u : unit | Hmc_heap_simple.lookup activation.Frame.env index === Some value
      && Hmc_u32_index.represents index number
      && base + Lower.slot_tag number <= 4294967280
      && Hmc_u32_index.represents next pc
      && Program.lookup program.Program.code activation.Frame.pc === Some (Program.Keep (G.Load (G.Local index, ty, derivation, next)))
      && Hmc_u32_index.represents activation.Frame.pc old_pc
      && next_signature.G.locals === signature.G.locals && next_signature.G.temporaries === signature.G.temporaries
      && Codec.decode signature activation.Frame.pc cells === Some (activation, padding)
      && base <= 4294967248 && Wasm_locals.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && Bytes.drop state.X.memory base === Some before_frame
      && Wire.decode_cells (D.S (Heap.length cells)) before_frame === Some (Heap.Cell (V.Word (Header.number old_pc), cells), tail)} ->
    {out : result | Hmc_linear_preservation.equal_prefix base state.X.memory out.memory
      && X.run (Lower.emit (Lower.Local (number, pc)) base_local) state === X.Done {X.memory = out.memory; machine = state.X.machine}
      && Machine.step program globals heap_limit stack_limit {Machine.heap; state = State.Running (activation, frames)}
        === Machine.Advanced {Machine.heap; state = State.Running (out.activation, frames)}
      && out.activation === {activation with Frame.pc = next; accumulator = value}
      && Codec.decode next_signature next out.cells === Some (out.activation, padding)
      && Heap.length out.cells === Heap.length cells && Bytes.drop out.memory base === Some out.bytes
      && Wire.decode_cells (D.S (Heap.length out.cells)) out.bytes === Some (Heap.Cell (V.Word (Header.number pc), out.cells), tail)} @ immutable =
  fun program globals heap heap_limit stack_limit signature next_signature activation frames index number ty derivation next value pc old_pc cells padding
      state base_local base before_frame tail premise ->
    ghost_ (Lower.slot_tag_def number; Lower.slot_payload_def number; Machine.step_def program globals heap_limit stack_limit {Machine.heap; state = State.Running (activation, frames)};
      Codec.decode_def signature activation.Frame.pc cells);
    match cells with
    | Heap.Cell (current, Heap.Cell (_, body)) ->
      let after_cells = Heap.Cell (current, Heap.Cell (value, body)) in
      let full = Heap.Cell (V.Word (Header.number pc), after_cells) in
      let after_frame = Wire.encode_cells full tail in
      let after = Wasm_memory_splice.replace state.X.memory base before_frame after_frame () in
      ghost_ (Heap.length_def cells; Heap.length_def (Heap.Cell (activation.Frame.accumulator, body));
        Heap.length_def after_cells; Heap.length_def (Heap.Cell (value, body)); Heap.length_def full;
        Hmc_wasm_relayout_finish.closure old_pc cells before_frame tail ();
        Hmc_wasm_relayout_finish.closure pc after_cells after_frame tail ();
        Lower.emit_def (Lower.Local (number, pc)) base_local;
        Hmc_wasm_local_step.correct signature next_signature activation frames padding body (Heap.length body)
          index number (Lower.slot_tag number) (Lower.slot_payload number) ty derivation next old_pc pc value
          state after tail base_local base before_frame after_frame ());
      {memory = after; cells = after_cells; bytes = after_frame; activation = {activation with Frame.pc = next; accumulator = value}}
    | _ -> unreachable_ ()

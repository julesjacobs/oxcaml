module B = Wasm_u32
module W = Hmc_word64
module D = Hm_declarative
module G = Hmc_cfg_ir
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module Frame = Hmc_heap_frame
module State = Hmc_heap_state
module Simple = Hmc_heap_simple
module Codec = Hmc_pointer_frame_codec
module Wire = Hmc_heap_wire
module Literal = Hmc_wasm_literal_load
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module Bytes = Hmc_linear_bytes
module P = Hmc_linear_preservation
let (correct @ total) : (signature : G.signature) @ immutable -> (next_signature : G.signature) @ immutable ->
    (activation : Frame.activation) @ immutable -> (frames : State.frames) @ immutable ->
    (padding : Heap.cells) @ immutable -> (cells : Heap.cells) @ immutable -> (count : D.index) @ immutable ->
    (index : D.index) @ immutable -> (number : W.limb) -> (source_tag : B.u32) -> (source_payload : B.u32) -> (ty : D.mono) @ immutable -> (derivation : D.typing) @ immutable ->
    (next : D.index) @ immutable -> (old_pc : W.limb) -> (pc : W.limb) -> (value : V.value) @ immutable ->
    (state : X.state) @ immutable -> (after : B.bytes) @ immutable -> (tail : B.bytes) @ immutable ->
    (base_local : B.u32) -> (base : B.u32) -> (before_frame : B.bytes) @ immutable -> (after_frame : B.bytes) @ immutable ->
    {u : unit | source_tag = 48 + 16 * number && source_payload = source_tag + 8 && base + source_tag <= 4294967280
      && Hmc_u32_index.represents index number && P.equal_prefix base state.X.memory after
      && Bytes.drop state.X.memory base === Some before_frame && Bytes.drop after base === Some after_frame
      && Wasm_locals.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && Simple.lookup activation.Frame.env index === Some value
      && Hmc_u32_index.represents activation.Frame.pc old_pc && Hmc_u32_index.represents next pc
      && next_signature.G.locals === signature.G.locals && next_signature.G.temporaries === signature.G.temporaries
      && Codec.decode signature activation.Frame.pc (Heap.Cell (activation.Frame.current, Heap.Cell (activation.Frame.accumulator, cells)))
        === Some (activation, padding)
      && Wire.decode (Wire.Closure_schema (D.S (D.S count))) before_frame
        === Some (Wire.Closure (old_pc, Heap.Cell (activation.Frame.current, Heap.Cell (activation.Frame.accumulator, cells))), tail)
      && Wire.decode (Wire.Closure_schema (D.S (D.S count))) after_frame
        === Some (Wire.Closure (pc, Heap.Cell (activation.Frame.current, Heap.Cell (value, cells))), tail)} ->
    {u : unit | X.run (Hmc_wasm_local_load.emit source_tag source_payload pc base_local) state === X.Done {X.memory = after; machine = state.X.machine}
      && Simple.step (G.Load (G.Local index, ty, derivation, next)) (State.Running (activation, frames))
        === State.Running ({activation with Frame.pc = next; accumulator = value}, frames)
      && Codec.decode next_signature next (Heap.Cell (activation.Frame.current, Heap.Cell (value, cells)))
        === Some ({activation with Frame.pc = next; accumulator = value}, padding)} @ ghost =
  fun signature next_signature activation frames padding cells count index number source_tag source_payload ty derivation next old_pc pc value state after tail base_local base before_frame after_frame premise -> ghost_ (
    Simple.load_def activation.Frame.env (G.Local index);
    Simple.step_def (G.Load (G.Local index, ty, derivation, next)) (State.Running (activation, frames));
    Codec.decode_def signature activation.Frame.pc (Heap.Cell (activation.Frame.current, Heap.Cell (activation.Frame.accumulator, cells)));
    Codec.decode_def next_signature next (Heap.Cell (activation.Frame.current, Heap.Cell (value, cells)));
    Hmc_wasm_frame_update.correct count old_pc pc activation.Frame.current activation.Frame.accumulator value cells
      state after tail base_local base before_frame after_frame ();
    Hmc_wasm_environment_read.correct state.X.memory base before_frame count old_pc cells tail signature activation padding
      index number source_tag source_payload value ();
    Hmc_wasm_local_load.correct source_tag source_payload pc base_local value state base after ())

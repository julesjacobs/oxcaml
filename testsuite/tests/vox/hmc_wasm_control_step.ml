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
module PC = Hmc_wasm_pc_update
module Branch = Hmc_wasm_branch
module Read = Hmc_wasm_frame_header
let (jump @ total) : (signature : G.signature) @ immutable -> (next_signature : G.signature) @ immutable ->
    (activation : Frame.activation) @ immutable -> (frames : State.frames) @ immutable ->
    (padding : Heap.cells) @ immutable -> (cells : Heap.cells) @ immutable -> (count : D.index) @ immutable ->
    (next : D.index) @ immutable -> (old_pc : W.limb) -> (pc : W.limb) ->
    (state : X.state) @ immutable -> (after : B.bytes) @ immutable -> (tail : B.bytes) @ immutable ->
    (base_local : B.u32) -> (base : B.u32) -> (before_frame : B.bytes) @ immutable -> (after_frame : B.bytes) @ immutable ->
    {u : unit | base <= 4294967248 && P.equal_prefix base state.X.memory after
      && Bytes.drop state.X.memory base === Some before_frame && Bytes.drop after base === Some after_frame
      && Wasm_locals.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && Hmc_u32_index.represents activation.Frame.pc old_pc && Hmc_u32_index.represents next pc
      && next_signature.G.locals === signature.G.locals && next_signature.G.temporaries === signature.G.temporaries
      && Codec.decode signature activation.Frame.pc (Heap.Cell (activation.Frame.current, Heap.Cell (activation.Frame.accumulator, cells)))
        === Some (activation, padding)
      && Wire.decode (Wire.Closure_schema (D.S (D.S count))) before_frame
        === Some (Wire.Closure (old_pc, Heap.Cell (activation.Frame.current, Heap.Cell (activation.Frame.accumulator, cells))), tail)
      && Wire.decode (Wire.Closure_schema (D.S (D.S count))) after_frame
        === Some (Wire.Closure (pc, Heap.Cell (activation.Frame.current, Heap.Cell (activation.Frame.accumulator, cells))), tail)} ->
    {u : unit | X.run (PC.emit pc base_local) state === X.Done {X.memory = after; machine = state.X.machine}
      && Simple.step (G.Jump next) (State.Running (activation, frames))
        === State.Running ({activation with Frame.pc = next}, frames)
      && Codec.decode next_signature next (Heap.Cell (activation.Frame.current, Heap.Cell (activation.Frame.accumulator, cells)))
        === Some ({activation with Frame.pc = next}, padding)} @ ghost =
  fun signature next_signature activation frames padding cells count next old_pc pc state after tail base_local base before_frame after_frame premise -> ghost_ (
    Simple.step_def (G.Jump next) (State.Running (activation, frames));
    Codec.decode_def signature activation.Frame.pc (Heap.Cell (activation.Frame.current, Heap.Cell (activation.Frame.accumulator, cells)));
    Codec.decode_def next_signature next (Heap.Cell (activation.Frame.current, Heap.Cell (activation.Frame.accumulator, cells)));
    PC.memory count old_pc pc activation.Frame.current activation.Frame.accumulator cells
      state.X.memory after tail base before_frame after_frame ();
    PC.correct pc base_local state base after ())
let (branch @ total) : (signature : G.signature) @ immutable -> (next_signature : G.signature) @ immutable ->
    (activation : Frame.activation) @ immutable -> (frames : State.frames) @ immutable ->
    (padding : Heap.cells) @ immutable -> (cells : Heap.cells) @ immutable -> (count : D.index) @ immutable ->
    (yes : D.index) @ immutable -> (no : D.index) @ immutable ->
    (yes_pc : W.limb) -> (no_pc : W.limb) -> (condition : bool) -> (next : D.index) @ immutable -> (old_pc : W.limb) -> (pc : W.limb) ->
    (state : X.state) @ immutable -> (after : B.bytes) @ immutable -> (tail : B.bytes) @ immutable ->
    (base_local : B.u32) -> (base : B.u32) -> (before_frame : B.bytes) @ immutable -> (after_frame : B.bytes) @ immutable ->
    {u : unit | base <= 4294967247 && P.equal_prefix base state.X.memory after
      && Bytes.drop state.X.memory base === Some before_frame && Bytes.drop after base === Some after_frame
      && Wasm_locals.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && activation.Frame.accumulator === V.Boolean condition
      && next === (if condition then yes else no) && pc = (if condition then yes_pc else no_pc)
      && Hmc_u32_index.represents yes yes_pc && Hmc_u32_index.represents no no_pc
      && Hmc_u32_index.represents activation.Frame.pc old_pc && Hmc_u32_index.represents next pc
      && next_signature.G.locals === signature.G.locals && next_signature.G.temporaries === signature.G.temporaries
      && Codec.decode signature activation.Frame.pc (Heap.Cell (activation.Frame.current, Heap.Cell (activation.Frame.accumulator, cells)))
        === Some (activation, padding)
      && Wire.decode (Wire.Closure_schema (D.S (D.S count))) before_frame
        === Some (Wire.Closure (old_pc, Heap.Cell (activation.Frame.current, Heap.Cell (activation.Frame.accumulator, cells))), tail)
      && Wire.decode (Wire.Closure_schema (D.S (D.S count))) after_frame
        === Some (Wire.Closure (pc, Heap.Cell (activation.Frame.current, Heap.Cell (activation.Frame.accumulator, cells))), tail)} ->
    {u : unit | X.run (Branch.emit yes_pc no_pc base_local) state === X.Done {X.memory = after; machine = state.X.machine}
      && Simple.step (G.Branch (yes, no)) (State.Running (activation, frames))
        === State.Running ({activation with Frame.pc = next}, frames)
      && Codec.decode next_signature next (Heap.Cell (activation.Frame.current, Heap.Cell (activation.Frame.accumulator, cells)))
        === Some ({activation with Frame.pc = next}, padding)} @ ghost =
  fun signature next_signature activation frames padding cells count yes no yes_pc no_pc condition next old_pc pc state after tail base_local base before_frame after_frame premise -> ghost_ (
    Simple.step_def (G.Branch (yes, no)) (State.Running (activation, frames));
    Codec.decode_def signature activation.Frame.pc (Heap.Cell (activation.Frame.current, Heap.Cell (activation.Frame.accumulator, cells)));
    Codec.decode_def next_signature next (Heap.Cell (activation.Frame.current, Heap.Cell (activation.Frame.accumulator, cells)));
    PC.memory count old_pc pc activation.Frame.current activation.Frame.accumulator cells
      state.X.memory after tail base before_frame after_frame ();
    Read.correct state.X.memory base before_frame (D.S (D.S count)) old_pc
      (Heap.Cell (activation.Frame.current, Heap.Cell (activation.Frame.accumulator, cells))) tail signature activation padding ();
    Read.accumulator_payload_def state.X.memory base; Branch.payload_offset_def ();
    Branch.correct yes_pc no_pc base_local condition state base after ())

module B = Wasm_u32
module W = Hmc_word64
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module Wire = Hmc_heap_wire
module Layout = Hmc_wasm_call_header_layout
module Header = Hmc_wasm_dynamic_call_header
module Memory = Wasm_mixed_memory
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module L = Wasm_locals
module Bytes = Hmc_linear_bytes
module Bounds = Hmc_linear_bounds
module P = Hmc_linear_preservation
let (correct @ total) : (recursive : bool) -> (pc : W.limb) -> (pc_local : B.u32) -> (closure : B.u32) -> (argument : V.value) @ immutable ->
    (state : X.state) @ immutable -> (base : B.u32) -> (limit : B.u32) -> (frame_local : B.u32) ->
    (closure_local : B.u32) -> (argument_tag : B.u32) -> (argument_payload : B.u32) ->
    {u : unit | L.get state.X.machine.E.locals pc_local === Some (S.I32 pc)
      && L.get state.X.machine.E.locals frame_local === Some (S.I32 base)
      && L.get state.X.machine.E.locals closure_local === Some (S.I32 closure)
      && L.get state.X.machine.E.locals argument_tag === Some (S.I64 (V.tag argument))
      && L.get state.X.machine.E.locals argument_payload === Some (S.I64 (V.payload argument))
      && base + Layout.width recursive <= limit && Bounds.covers state.X.memory limit} ->
    {out : Memory.result | X.run (Header.emit recursive pc_local frame_local closure_local argument_tag argument_payload) state === X.Done {X.memory = out.Memory.memory; machine = state.X.machine}
      && Bytes.drop out.Memory.memory base === Some out.Memory.bytes
      && Wire.decode_cells (Heap.length (Layout.cells recursive pc closure argument)) out.Memory.bytes === Some (Layout.cells recursive pc closure argument, out.Memory.suffix)
      && P.equal_prefix base state.X.memory out.Memory.memory
      && V.length out.Memory.memory === V.length state.X.memory && Bounds.covers out.Memory.memory limit
      && Bytes.drop state.X.memory (S.add32 base (Layout.width recursive)) === Some out.Memory.suffix
      && Bytes.drop out.Memory.memory (S.add32 base (Layout.width recursive)) === Some out.Memory.suffix} @ immutable =
  fun recursive pc pc_local closure argument state base limit frame_local closure_local argument_tag argument_payload premise ->
    ghost_ (Header.layout recursive pc pc_local closure argument closure_local argument_tag argument_payload state.X.machine.E.locals (); Layout.width_def recursive);
    let out = Memory.correct (Header.writes recursive pc_local closure_local argument_tag argument_payload)
      (Hmc_wire_word_sequence.words (Layout.cells recursive pc closure argument)) (Layout.width recursive) state base limit frame_local () in
    ghost_ (Header.emit_def recursive pc_local frame_local closure_local argument_tag argument_payload;
      Hmc_wire_word_sequence.recover (Layout.cells recursive pc closure argument) out.Memory.bytes out.Memory.suffix ());
    out

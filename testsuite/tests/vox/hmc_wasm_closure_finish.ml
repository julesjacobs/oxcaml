module B = Wasm_u32
module W = Hmc_word64
module D = Hm_declarative
module G = Hmc_cfg_ir
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module Frame = Hmc_heap_frame
module Codec = Hmc_pointer_frame_codec
module Wire = Hmc_heap_wire
module Header = Hmc_wasm_header_update
module Index = Hmc_u32_index
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module Bytes = Hmc_linear_bytes
module Write = Hmc_wasm_closure_result
module Memory = Hmc_wasm_closure_result_memory
module PC = Hmc_wasm_pc_update
let[@def] (emit @ total) (width : B.u32) (pc : W.limb) (frame_local : B.u32) (heap_local : B.u32) =
  E.append (Write.emit width frame_local heap_local) (PC.emit pc frame_local)
type result = {memory : B.bytes; cells : Heap.cells; bytes : B.bytes; activation : Frame.activation}
let (correct @ total) : (signature : G.signature) @ immutable -> (next_signature : G.signature) @ immutable ->
    (activation : Frame.activation) @ immutable -> (next : D.index) @ immutable -> (old_pc : W.limb) -> (pc : W.limb) ->
    (cells : Heap.cells) @ immutable -> (padding : Heap.cells) @ immutable ->
    (width : B.u32) -> (heap_base : B.u32) -> (heap_end : B.u32) -> (heap_local : B.u32) ->
    (state : X.state) @ immutable -> (frame_local : B.u32) -> (base : B.u32) -> (before_frame : B.bytes) @ immutable -> (tail : B.bytes) @ immutable ->
    {u : unit | Index.represents next pc && heap_end = heap_base + width
      && next_signature.G.locals === signature.G.locals && next_signature.G.temporaries === signature.G.temporaries
      && Codec.decode signature activation.Frame.pc cells === Some (activation, padding)
      && base <= 4294967248
      && Wasm_locals.get state.X.machine.E.locals frame_local === Some (S.I32 base)
      && Wasm_locals.get state.X.machine.E.locals heap_local === Some (S.I32 heap_end)
      && Bytes.drop state.X.memory base === Some before_frame
      && Wire.decode_cells (D.S (Heap.length cells)) before_frame === Some (Heap.Cell (V.Word (Header.number old_pc), cells), tail)} ->
    {out : result | Hmc_linear_preservation.equal_prefix base state.X.memory out.memory
      && X.run (emit width pc frame_local heap_local) state === X.Done {X.memory = out.memory; machine = state.X.machine}
      && out.activation === {activation with Frame.pc = next; accumulator = V.Closure_pointer heap_base}
      && Codec.decode next_signature next out.cells === Some (out.activation, padding)
      && Heap.length out.cells === Heap.length cells && Bytes.drop out.memory base === Some out.bytes
      && Wire.decode_cells (D.S (Heap.length out.cells)) out.bytes === Some (Heap.Cell (V.Word (Header.number pc), out.cells), tail)} @ immutable =
  fun signature next_signature activation next old_pc pc cells padding width heap_base heap_end heap_local state frame_local base before_frame tail premise ->
    ghost_ (Codec.decode_def signature activation.Frame.pc cells);
    match cells with
    | Heap.Cell (current, Heap.Cell (_, body)) ->
      let value = V.Closure_pointer heap_base in
      let after_cells = Heap.Cell (current, Heap.Cell (value, body)) in
      let middle_full = Heap.Cell (V.Word (Header.number old_pc), after_cells) in
      let full = Heap.Cell (V.Word (Header.number pc), after_cells) in
      let middle_frame = Wire.encode_cells middle_full tail in
      let after_frame = Wire.encode_cells full tail in
      let middle = Wasm_memory_splice.replace state.X.memory base before_frame middle_frame () in
      let after = Wasm_memory_splice.replace middle base middle_frame after_frame () in
      let intermediate = {X.memory = middle; machine = state.X.machine} in
      ghost_ (Hmc_wasm_cons_success.prefix_transitive state.X.memory middle after base ();
        Heap.length_def cells; Heap.length_def (Heap.Cell (activation.Frame.accumulator, body));
        Heap.length_def after_cells; Heap.length_def (Heap.Cell (value, body)); Heap.length_def middle_full; Heap.length_def full;
        Hmc_wasm_relayout_finish.closure old_pc cells before_frame tail ();
        Hmc_wasm_relayout_finish.closure old_pc after_cells middle_frame tail ();
        Hmc_wasm_relayout_finish.closure pc after_cells after_frame tail ();
        Memory.correct width activation.Frame.accumulator heap_base heap_end heap_local old_pc current body (Heap.length body)
          state middle frame_local base before_frame middle_frame tail ();
        PC.memory (Heap.length body) old_pc pc current value body middle after tail base middle_frame after_frame ();
        PC.correct pc frame_local intermediate base after ();
        Codec.decode_def next_signature next after_cells;
        emit_def width pc frame_local heap_local;
        X.append_correct (Write.emit width frame_local heap_local) (PC.emit pc frame_local) state);
      {memory = after; cells = after_cells; bytes = after_frame; activation = {activation with Frame.pc = next; accumulator = value}}
    | _ -> unreachable_ ()

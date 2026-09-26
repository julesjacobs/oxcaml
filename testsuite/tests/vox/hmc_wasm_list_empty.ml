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
module Index = Hmc_u32_index
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module L = Wasm_locals
module Bytes = Hmc_linear_bytes
module Result = Hmc_wasm_list_finish_invariant
let (correct @ total) : (program : Program.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (heap_limit : W.limb) -> (stack_limit : D.index) @ immutable -> (frames : State.frames) @ immutable ->
    (signature : G.signature) @ immutable -> (activation : Frame.activation) @ immutable ->
    (empty : D.index) @ immutable -> (full : D.index) @ immutable -> (pc : W.limb) -> (old_pc : W.limb) ->
    (cells : Heap.cells) @ immutable -> (padding : Heap.cells) @ immutable -> (capacity : Hmc_wasm_relayout.count) ->
    (heap : Heap.heap) @ immutable -> (frame_stop : B.u32) ->
    (state : X.state) @ immutable -> (base_local : B.u32) -> (base : B.u32) ->
    (before_frame : B.bytes) @ immutable -> (suffix : B.bytes) @ immutable ->
    {u : unit | Program.lookup program.Program.code activation.Frame.pc === Some (Program.Keep (G.List_branch (empty, full)))
      && activation.Frame.accumulator === V.Nil
      && Index.represents activation.Frame.pc old_pc && Index.represents empty pc && Index.represents (Heap.length cells) capacity
      && Codec.decode signature activation.Frame.pc cells === Some (activation, padding)
      && frame_stop = base + 16 + 16 * capacity && base <= 4294967248
      && Hmc_heap_image.related state.X.memory heap && Hmc_heap_image_suffix.above heap frame_stop
      && L.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && Bytes.drop state.X.memory base === Some before_frame
      && Wire.decode_cells (D.S (Heap.length cells)) before_frame === Some (Heap.Cell (V.Word (Header.number old_pc), cells), suffix)} ->
    {out : Result.result | Hmc_linear_preservation.equal_prefix base state.X.memory out.Result.memory
      && X.run (Hmc_wasm_pc_update.emit pc base_local) state === X.Done {X.memory = out.Result.memory; machine = state.X.machine}
      && Machine.step program globals heap_limit stack_limit {Machine.heap; state = State.Running (activation, frames)}
        === Machine.Advanced {Machine.heap; state = State.Running ({activation with Frame.pc = empty}, frames)}
      && Codec.decode signature empty out.Result.cells === Some ({activation with Frame.pc = empty}, out.Result.padding)
      && out.Result.cells === cells && out.Result.padding === padding
      && Hmc_heap_image.related out.Result.memory heap && Bytes.drop out.Result.memory frame_stop === Some suffix
      && Bytes.drop out.Result.memory base === Some out.Result.bytes
      && Wire.decode_cells (D.S (Heap.length out.Result.cells)) out.Result.bytes === Some (Heap.Cell (V.Word (Header.number pc), out.Result.cells), suffix)} @ immutable =
  fun program globals heap_limit stack_limit frames signature activation empty full pc old_pc cells padding capacity heap frame_stop
      state base_local base before_frame suffix premise ->
    let after_cells = Heap.Cell (V.Word (Header.number pc), cells) in
    let after_frame = Wire.encode_cells after_cells suffix in
    let after = Wasm_memory_splice.replace state.X.memory base before_frame after_frame () in
    ghost_ (Codec.decode_def signature activation.Frame.pc cells; Heap.length_def after_cells);
    (match cells with
    | Heap.Cell (_, Heap.Cell (_, body)) -> ghost_ (
      Heap.length_def cells; Heap.length_def (Heap.Cell (activation.Frame.accumulator, body));
      Hmc_wasm_relayout_finish.closure old_pc cells before_frame suffix ();
      Hmc_wasm_relayout_finish.closure pc cells after_frame suffix ();
      Hmc_wasm_control_step.jump signature signature activation frames padding body (Heap.length body) empty old_pc pc
        state after suffix base_local base before_frame after_frame ())
    | _ -> unreachable_ ());
    ghost_ (Hmc_heap_list_branch.empty program globals heap_limit stack_limit heap activation frames empty full ();
      let before_cells = Heap.Cell (V.Word (Header.number old_pc), cells) in
      Heap.length_def before_cells;
      Index.represents_def (D.S (Heap.length cells)) (capacity + 1);
      Hmc_wasm_frame_suffix.preserve_heap state.X.memory after heap base frame_stop (capacity + 1) before_cells after_cells before_frame after_frame suffix ();
      Hmc_wasm_frame_suffix.suffix after base frame_stop (capacity + 1) after_cells after_frame suffix ());
    {Result.memory = after; cells; padding; bytes = after_frame}

module B = Wasm_u32
module D = Hm_declarative
module V = Hmc_tagged_cell
module H = Hmc_heap_objects
module F = Hmc_heap_frame
module Q = Hmc_heap_state
module G = Hmc_cfg_ir
module I = Hmc_tail_ir
module Machine = Hmc_heap_machine
module Codec = Hmc_pointer_frame_codec
module Wire = Hmc_heap_wire
module Cells = Hmc_wasm_call_save_memory
module Return = Hmc_wasm_return_result
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module L = Wasm_locals
module Bytes = Hmc_linear_bytes
module Bounds = Hmc_linear_bounds
module Image = Hmc_heap_image
module Above = Hmc_heap_image_suffix
let (correct @ total) : (program : I.program) @ immutable -> (globals : Machine.globals) @ immutable -> (heap : H.heap) @ immutable ->
    (heap_limit : B.u32) -> (stack_limit : D.index) @ immutable -> (activation : F.activation) @ immutable ->
    (saved : F.activation) @ immutable -> (frames : Q.frames) @ immutable -> (signature : G.signature) @ immutable ->
    (pc : B.u32) -> (source_pc : B.u32) -> (rest : H.cells) @ immutable -> (source_rest : H.cells) @ immutable ->
    (state : X.state) @ immutable -> (source : B.u32) -> (base : B.u32) -> (limit : B.u32) ->
    (before : B.bytes) @ immutable -> (suffix : B.bytes) @ immutable -> (source_bytes : B.bytes) @ immutable -> (source_suffix : B.bytes) @ immutable ->
    (source_local : B.u32) -> (caller_local : B.u32) ->
    {u : unit | I.lookup program.I.code activation.F.pc === Some (I.Keep G.Return) && activation.F.temporaries === F.Empty
      && Hmc_u32_index.represents saved.F.pc pc && Hmc_u32_index.represents activation.F.pc source_pc
      && base <= 4294967247 && source <= 4294967248 && Bounds.covers state.X.memory limit
      && Image.related state.X.memory heap && Above.above heap (S.add32 base (Return.end_offset ()))
      && Bytes.drop state.X.memory base === Some before && Bytes.drop state.X.memory source === Some source_bytes
      && Wire.decode_cells (H.length (Cells.cells pc saved.F.current saved.F.accumulator rest)) before ===
        Some (Cells.cells pc saved.F.current saved.F.accumulator rest, suffix)
      && Codec.decode signature saved.F.pc (H.Cell (saved.F.current, H.Cell (saved.F.accumulator, rest))) === Some (saved, H.Empty)
      && Wire.decode_cells (H.length (Cells.cells source_pc activation.F.current activation.F.accumulator source_rest)) source_bytes ===
        Some (Cells.cells source_pc activation.F.current activation.F.accumulator source_rest, source_suffix)
      && L.get state.X.machine.E.locals source_local === Some (S.I32 source)
      && L.get state.X.machine.E.locals caller_local === Some (S.I32 base)} ->
    {out : Return.result | X.run (Return.emit source_local caller_local) state === X.Done {X.memory = out.Return.memory; machine = state.X.machine}
      && Machine.step program globals heap_limit stack_limit {Machine.heap; state = Q.Running (activation, Q.Frame (saved, frames))} ===
        Machine.Advanced {Machine.heap; state = Q.Running ({saved with F.accumulator = activation.F.accumulator}, frames)}
      && Image.related out.Return.memory heap && V.length out.Return.memory === V.length state.X.memory && Bounds.covers out.Return.memory limit
      && Bytes.drop out.Return.memory base === Some out.Return.bytes
      && Wire.decode_cells (H.length (Cells.cells pc saved.F.current activation.F.accumulator rest)) out.Return.bytes ===
        Some (Cells.cells pc saved.F.current activation.F.accumulator rest, suffix)
      && Codec.decode signature saved.F.pc (H.Cell (saved.F.current, H.Cell (activation.F.accumulator, rest))) ===
        Some ({saved with F.accumulator = activation.F.accumulator}, H.Empty)} @ immutable =
  fun program globals heap heap_limit stack_limit activation saved frames signature pc source_pc rest source_rest state source base limit before suffix source_bytes source_suffix source_local caller_local premise ->
    let source_cells = Cells.cells source_pc activation.F.current activation.F.accumulator source_rest in
    ghost_ (Cells.cells_def source_pc activation.F.current activation.F.accumulator source_rest;
      Hmc_heap_simple.lookup_def source_cells (D.S (D.S D.Z));
      Hmc_heap_simple.lookup_def (H.Cell (activation.F.current, H.Cell (activation.F.accumulator, source_rest))) (D.S D.Z);
      Hmc_heap_simple.lookup_def (H.Cell (activation.F.accumulator, source_rest)) D.Z;
      Hmc_u32_index.represents_def (D.S (D.S D.Z)) 2; Hmc_u32_index.represents_def (D.S D.Z) 1; Hmc_u32_index.represents_def D.Z 0;
      Hmc_wasm_cells_read.correct state.X.memory source source_bytes (H.length source_cells) source_cells source_suffix
        (D.S (D.S D.Z)) 2 32 40 activation.F.accumulator ();
      Return.tag_offset_def (); Return.payload_offset_def ());
    let result = Return.correct signature saved.F.pc pc saved rest activation.F.accumulator state source base limit before suffix source_local caller_local () in
    ghost_ (Hmc_heap_return_transition.caller program globals heap heap_limit stack_limit activation saved frames ();
      Above.preserve state.X.memory result.Return.memory heap (S.add32 base (Return.end_offset ())) ());
    result

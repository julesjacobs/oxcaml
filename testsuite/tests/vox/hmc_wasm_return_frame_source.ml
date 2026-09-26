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
module Full = Hmc_wasm_return_frame
module Restore = Hmc_wasm_frame_restore
module Stack = Hmc_memory_stack
module Plan = Wasm_parallel_copy
module Cfg = Hmc_cfg_program
module Closure = Hmc_closure_program
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module L = Wasm_locals
module Bytes = Hmc_linear_bytes
module Bounds = Hmc_linear_bounds
module Image = Hmc_heap_image
module Above = Hmc_heap_image_suffix
module P = Hmc_linear_preservation
let rec (prefix_transitive @ total) : (before : B.bytes) @ immutable -> (middle : B.bytes) @ immutable ->
    (after : B.bytes) @ immutable -> (width : B.u32) ->
    {u : unit | P.equal_prefix width before middle && P.equal_prefix width middle after} ->
    {u : unit | P.equal_prefix width before after} @ ghost = fun before middle after width premise -> ghost_ (
  P.equal_prefix_def width before middle; P.equal_prefix_def width middle after; P.equal_prefix_def width before after;
  if width = 0 then () else match before, middle, after with
  | B.Byte (_, a), B.Byte (_, b), B.Byte (_, c) -> prefix_transitive a b c (width - 1) ()
  | _ -> ())
let rec (above_lower @ total) : (heap : H.heap) @ immutable -> (upper : B.u32) -> (lower : B.u32) ->
    {u : unit | lower <= upper && Above.above heap upper} ->
    {u : unit | Above.above heap lower} @ ghost = fun heap upper lower premise -> ghost_ (
    Above.above_def heap upper; Above.above_def heap lower;
    match heap with H.Empty_heap _ -> () | H.Allocate (_, rest) -> above_lower rest upper lower ())
let (correct @ total) : (program : I.program) @ immutable -> (globals : Machine.globals) @ immutable -> (heap : H.heap) @ immutable ->
    (heap_limit : B.u32) -> (stack_limit : D.index) @ immutable -> (activation : F.activation) @ immutable ->
    (saved : F.activation) @ immutable -> (frames : Q.frames) @ immutable -> (signature : G.signature) @ immutable ->
    (pc : B.u32) -> (source_pc : B.u32) -> (rest : H.cells) @ immutable -> (padding : H.cells) @ immutable -> (source_rest : H.cells) @ immutable ->
    (plan : Plan.plan) @ immutable -> (count : Hmc_wasm_relayout.count) ->
    (state : X.state) @ immutable -> (source : B.u32) -> (base : B.u32) -> (top : B.u32) -> (stack_base : B.u32) -> (limit : B.u32) ->
    (before : B.bytes) @ immutable -> (suffix : B.bytes) @ immutable -> (source_bytes : B.bytes) @ immutable -> (source_suffix : B.bytes) @ immutable ->
    (source_local : B.u32) -> (caller_local : B.u32) ->
    {u : unit | I.lookup program.I.code activation.F.pc === Some (I.Keep G.Return) && activation.F.temporaries === F.Empty
      && Hmc_u32_index.represents saved.F.pc pc && Hmc_u32_index.represents activation.F.pc source_pc
      && count >= 2 && base + 16 + 16 * count = top && top <= limit
      && source + 16 + 16 * count <= stack_base && stack_base <= base && source_local <> caller_local
      && Hmc_u32_index.represents (H.length (H.Cell (saved.F.current, H.Cell (saved.F.accumulator, rest)))) count
      && Restore.matches plan (H.length (H.Cell (saved.F.current, H.Cell (saved.F.accumulator, rest))))
      && Bounds.covers state.X.memory limit && Image.related state.X.memory heap
      && H.valid program.I.origin.Cfg.origin.Closure.table heap && (H.used heap <= stack_base || Above.above heap top)
      && Above.above heap (S.add32 source (Restore.width count))
      && Stack.related program.I.origin.Cfg.blocks (Restore.width count) state.X.memory stack_base top (Q.Frame (saved, frames))
      && Bytes.drop state.X.memory base === Some before && Bytes.drop state.X.memory source === Some source_bytes
      && Wire.decode_cells (H.length (Cells.cells pc saved.F.current saved.F.accumulator rest)) before ===
        Some (Cells.cells pc saved.F.current saved.F.accumulator rest, suffix)
      && Codec.decode signature saved.F.pc (H.Cell (saved.F.current, H.Cell (saved.F.accumulator, rest))) === Some (saved, padding)
      && Wire.decode_cells (H.length (Cells.cells source_pc activation.F.current activation.F.accumulator source_rest)) source_bytes ===
        Some (Cells.cells source_pc activation.F.current activation.F.accumulator source_rest, source_suffix)
      && L.get state.X.machine.E.locals source_local === Some (S.I32 source)
      && L.get state.X.machine.E.locals caller_local === Some (S.I32 top)} ->
    {out : Full.result | out.Full.state.X.machine.E.stack === state.X.machine.E.stack
      && P.equal_prefix source state.X.memory out.Full.state.X.memory
      && X.run (Full.emit plan (Restore.width count) source_local caller_local) state === X.Done out.Full.state
      && Machine.step program globals heap_limit stack_limit {Machine.heap; state = Q.Running (activation, Q.Frame (saved, frames))} ===
        Machine.Advanced {Machine.heap; state = Q.Running ({saved with F.accumulator = activation.F.accumulator}, frames)}
      && Image.related out.Full.state.X.memory heap
      && Stack.related program.I.origin.Cfg.blocks (Restore.width count) out.Full.state.X.memory stack_base base frames
      && L.replaced state.X.machine.E.locals caller_local (S.I32 base) out.Full.state.X.machine.E.locals
      && L.get out.Full.state.X.machine.E.locals caller_local === Some (S.I32 base)
      && L.get out.Full.state.X.machine.E.locals source_local === Some (S.I32 source)
      && V.length out.Full.state.X.memory === V.length state.X.memory && Bounds.covers out.Full.state.X.memory limit
      && Bytes.drop out.Full.state.X.memory source === Some out.Full.restored.Wasm_scatter_memory.bytes
      && Wire.decode_cells (H.length (Cells.cells pc saved.F.current activation.F.accumulator rest)) out.Full.restored.Wasm_scatter_memory.bytes ===
        Some (Cells.cells pc saved.F.current activation.F.accumulator rest, out.Full.restored.Wasm_scatter_memory.suffix)
      && Codec.decode signature saved.F.pc (H.Cell (saved.F.current, H.Cell (activation.F.accumulator, rest))) ===
        Some ({saved with F.accumulator = activation.F.accumulator}, padding)} @ immutable =
  fun program globals heap heap_limit stack_limit activation saved frames signature pc source_pc rest padding source_rest plan count state source base top stack_base limit before suffix source_bytes source_suffix source_local caller_local premise ->
    let source_cells = Cells.cells source_pc activation.F.current activation.F.accumulator source_rest in
    ghost_ (Restore.width_def count; Cells.cells_def source_pc activation.F.current activation.F.accumulator source_rest;
      Hmc_heap_simple.lookup_def source_cells (D.S (D.S D.Z));
      Hmc_heap_simple.lookup_def (H.Cell (activation.F.current, H.Cell (activation.F.accumulator, source_rest))) (D.S D.Z);
      Hmc_heap_simple.lookup_def (H.Cell (activation.F.accumulator, source_rest)) D.Z;
      Hmc_u32_index.represents_def (D.S (D.S D.Z)) 2; Hmc_u32_index.represents_def (D.S D.Z) 1; Hmc_u32_index.represents_def D.Z 0;
      Hmc_wasm_cells_read.correct state.X.memory source source_bytes (H.length source_cells) source_cells source_suffix
        (D.S (D.S D.Z)) 2 32 40 activation.F.accumulator ();
      Return.tag_offset_def (); Return.payload_offset_def ());
    let result = Full.correct signature saved.F.pc pc saved rest padding activation.F.accumulator plan count state source base top limit before suffix source_local caller_local () in
    ghost_ (P.shrink base source state.X.memory result.Full.returned.Return.memory ();
      prefix_transitive state.X.memory result.Full.returned.Return.memory result.Full.state.X.memory source ();
      Hmc_heap_return_transition.caller program globals heap heap_limit stack_limit activation saved frames ();
      (if H.used heap <= stack_base then Image.preserve program.I.origin.Cfg.origin.Closure.table state.X.memory result.Full.returned.Return.memory heap base ()
      else (
        Return.end_offset_def (); S.add32_def base (Return.end_offset ());
        above_lower heap top (S.add32 base (Return.end_offset ())) ();
        Above.preserve state.X.memory result.Full.returned.Return.memory heap (S.add32 base (Return.end_offset ())) ()));
      Above.preserve result.Full.returned.Return.memory result.Full.state.X.memory heap (S.add32 source (Restore.width count)) ();
      Stack.related_def program.I.origin.Cfg.blocks (Restore.width count) state.X.memory stack_base top (Q.Frame (saved, frames));
      Stack.previous_def (Restore.width count) top;
      Stack.preserve program.I.origin.Cfg.blocks (Restore.width count) state.X.memory result.Full.returned.Return.memory stack_base base frames base ();
      S.add32_def source (Restore.width count);
      Bounds.covers_def result.Full.returned.Return.memory (S.add32 source (Restore.width count));
      Bounds.covers_def result.Full.state.X.memory (S.add32 source (Restore.width count));
      Stack.preserve_suffix program.I.origin.Cfg.blocks (Restore.width count) result.Full.returned.Return.memory result.Full.state.X.memory
        stack_base base frames (S.add32 source (Restore.width count)) ());
    result

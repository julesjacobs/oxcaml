module B = Wasm_u32
module D = Hm_declarative
module V = Hmc_tagged_cell
module H = Hmc_heap_objects
module W = Hmc_wire_word_sequence
module Q = Wasm_word_sequence
module R = Hmc_wasm_relayout
module Range = Hmc_wasm_range_copy
module Cross = Wasm_cross_words
module Scatter = Wasm_scatter_words
module Memory = Wasm_scatter_memory
module Plan = Wasm_parallel_copy
module Save = Hmc_wasm_call_save
module Index = Hmc_u32_index
module Header = Hmc_wasm_header_update
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module L = Wasm_locals
module Bytes = Hmc_linear_bytes
module Bounds = Hmc_linear_bounds
module P = Hmc_linear_preservation
module Wire = Hmc_heap_wire
module Save_memory = Hmc_wasm_call_save_memory
module Pad = Hmc_wasm_frame_padding
module Pad_memory = Hmc_wasm_padding_memory
module Finish = Hmc_wasm_frame_pad_finish
module Write = Wasm_mixed_write
module Seg = Hmc_frame_segments
let[@def] (emit @ total) (fragment : Save.fragment @ immutable) (padding : Write.writes @ immutable) (source_local : B.u32) (base_local : B.u32) =
  E.append (Save.emit fragment source_local base_local) (Write.emit padding base_local)
let (correct @ total) : (fragment : Save.fragment) @ immutable -> (tail_plan : Plan.plan) @ immutable ->
    (padding : Write.writes) @ immutable -> (padding_count : R.count) -> (padding_length : D.index) @ immutable ->
    (old_pc : B.u32) -> (current : V.value) @ immutable -> (accumulator : V.value) @ immutable -> (rest : H.cells) @ immutable ->
    (position : R.count) -> (count : R.count) -> (state : X.state) @ immutable -> (source : B.u32) -> (base : B.u32) -> (limit : B.u32) ->
    (source_local : B.u32) -> (base_local : B.u32) ->
    {u : unit | Index.represents (H.length rest) count && position + count <= 268435452 && 3 + count <= 268435452
      && Cross.matches fragment.Save.copies (Memory.zero ()) (Memory.zero ()) (W.words (Save_memory.header old_pc current accumulator)) tail_plan
      && Cross.reads state.X.memory source (Memory.zero ()) (W.words (Save_memory.header old_pc current accumulator))
      && R.range_is tail_plan position 2 (H.length rest) Plan.End && Range.reads state.X.memory source position rest
      && Index.represents padding_length padding_count && 3 + count + padding_count <= 268435452
      && Pad.matches padding (3 + count) padding_length
      && base + 48 + 16 * count + 16 * padding_count <= limit && Bounds.covers state.X.memory limit
      && L.get state.X.machine.E.locals source_local === Some (S.I32 source)
      && L.get state.X.machine.E.locals base_local === Some (S.I32 base)} ->
    {out : Finish.result | X.run (emit fragment padding source_local base_local) state === X.Done {X.memory = out.Finish.memory; machine = state.X.machine}
      && Bytes.drop out.Finish.memory base === Some out.Finish.bytes
      && Wire.decode_cells (H.length (Seg.append (Save_memory.cells fragment.Save.pc current accumulator rest) (Pad.cells padding_length))) out.Finish.bytes ===
        Some (Seg.append (Save_memory.cells fragment.Save.pc current accumulator rest) (Pad.cells padding_length), out.Finish.suffix)
      && P.equal_prefix base state.X.memory out.Finish.memory && V.length out.Finish.memory === V.length state.X.memory
      && Bounds.covers out.Finish.memory limit
      && Bytes.drop state.X.memory (S.add32 base (S.add32 (Save_memory.extent count) (Pad_memory.offset padding_count))) === Some out.Finish.suffix
      && Bytes.drop out.Finish.memory (S.add32 base (S.add32 (Save_memory.extent count) (Pad_memory.offset padding_count))) === Some out.Finish.suffix} @ immutable =
  fun fragment tail_plan padding padding_count padding_length old_pc current accumulator rest position count state source base limit source_local base_local premise ->
    let saved = Save_memory.correct fragment tail_plan old_pc current accumulator rest position count state source base limit source_local base_local () in
    let prefix = Save_memory.cells fragment.Save.pc current accumulator rest in
    ghost_ (Save_memory.cells_def fragment.Save.pc current accumulator rest;
      H.length_def prefix; H.length_def (H.Cell (current, H.Cell (accumulator, rest))); H.length_def (H.Cell (accumulator, rest));
      Index.represents_def (H.length prefix) (3 + count);
      Index.represents_def (H.length (H.Cell (current, H.Cell (accumulator, rest)))) (2 + count);
      Index.represents_def (H.length (H.Cell (accumulator, rest))) (1 + count));
    let finished = Finish.correct padding prefix (3 + count) padding_count padding_length
      {X.memory = saved.Save_memory.memory; machine = state.X.machine} base limit saved.Save_memory.bytes saved.Save_memory.suffix base_local () in
    ghost_ (Save_memory.extent_def count; Pad_memory.offset_def padding_count; Pad_memory.offset_def (3 + count);
      S.add32_def base (48 + 16 * count); S.add32_def (48 + 16 * count) (16 * padding_count);
      S.add32_def base (48 + 16 * count + 16 * padding_count);
      Hmc_heap_image_suffix.seek state.X.memory saved.Save_memory.memory (base + 48 + 16 * count) (base + 48 + 16 * count + 16 * padding_count) ();
      Hmc_heap_image_prefix.transitive state.X.memory saved.Save_memory.memory finished.Finish.memory base ();
      emit_def fragment padding source_local base_local;
      X.append_correct (Save.emit fragment source_local base_local) (Write.emit padding base_local) state);
    finished

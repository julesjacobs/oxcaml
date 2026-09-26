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
let[@def] (header @ total) (pc : B.u32) (current : V.value @ immutable) (accumulator : V.value @ immutable) =
  H.Cell (V.Word (Header.number pc), H.Cell (current, H.Cell (accumulator, H.Empty)))
let[@def] (cells @ total) (pc : B.u32) (current : V.value @ immutable) (accumulator : V.value @ immutable) (rest : H.cells @ immutable) =
  H.Cell (V.Word (Header.number pc), H.Cell (current, H.Cell (accumulator, rest)))
let[@def] (extent @ total) (count : R.count) : B.u32 = 48 + 16 * count
type result = {memory : B.bytes; bytes : B.bytes; suffix : B.bytes}
let (correct @ total) : (fragment : Save.fragment) @ immutable -> (tail_plan : Plan.plan) @ immutable ->
    (old_pc : B.u32) -> (current : V.value) @ immutable -> (accumulator : V.value) @ immutable -> (rest : H.cells) @ immutable ->
    (position : R.count) -> (count : R.count) -> (state : X.state) @ immutable -> (source : B.u32) -> (base : B.u32) -> (limit : B.u32) ->
    (source_local : B.u32) -> (base_local : B.u32) ->
    {u : unit | Index.represents (H.length rest) count && position + count <= 268435452 && 3 + count <= 268435452
      && Cross.matches fragment.Save.copies (Memory.zero ()) (Memory.zero ()) (W.words (header old_pc current accumulator)) tail_plan
      && Cross.reads state.X.memory source (Memory.zero ()) (W.words (header old_pc current accumulator))
      && R.range_is tail_plan position 2 (H.length rest) Plan.End && Range.reads state.X.memory source position rest
      && base + 48 + 16 * count <= limit && Bounds.covers state.X.memory limit
      && L.get state.X.machine.E.locals source_local === Some (S.I32 source)
      && L.get state.X.machine.E.locals base_local === Some (S.I32 base)} ->
    {out : result | X.run (Save.emit fragment source_local base_local) state === X.Done {X.memory = out.memory; machine = state.X.machine}
      && Wire.decode_cells (H.length (cells fragment.Save.pc current accumulator rest)) out.bytes === Some (cells fragment.Save.pc current accumulator rest, out.suffix)
      && Bytes.drop out.memory base === Some out.bytes && P.equal_prefix base state.X.memory out.memory
      && Bytes.drop state.X.memory (S.add32 base (extent count)) === Some out.suffix
      && Bytes.drop out.memory (S.add32 base (extent count)) === Some out.suffix
      && V.length out.memory === V.length state.X.memory && Bounds.covers out.memory limit} @ immutable =
  fun fragment tail_plan old_pc current accumulator rest position count state source base limit source_local base_local premise ->
    let leading = header old_pc current accumulator in
    let old_cells = cells old_pc current accumulator rest in
    let new_cells = cells fragment.Save.pc current accumulator rest in
    let width : B.u32 = 48 + 16 * count in
    ghost_ (Memory.zero_def ();
      Hmc_wasm_cross_range_words.correct rest tail_plan Plan.End position 2 count state.X.memory source ();
      Range.tag_def position; Range.tag_def 2;
      Scatter.matches_def Plan.End width Q.End state.X.memory source;
      W.size rest count (16 * count) ();
      Wasm_scatter_range.correct tail_plan Plan.End (Range.tag position) 48 (W.words rest) Q.End state.X.memory source (16 * count) width ());
    ghost_ (header_def old_pc current accumulator; cells_def old_pc current accumulator rest;
      W.words_def leading; W.words_def (H.Cell (current, H.Cell (accumulator, H.Empty)));
      W.words_def (H.Cell (accumulator, H.Empty)); W.words_def H.Empty;
      Q.size_def (W.words leading) 48;
      Q.size_def (Q.Word (V.payload (V.Word (Header.number old_pc)), W.words (H.Cell (current, H.Cell (accumulator, H.Empty))))) 40;
      Q.size_def (W.words (H.Cell (current, H.Cell (accumulator, H.Empty)))) 32;
      Q.size_def (Q.Word (V.payload current, W.words (H.Cell (accumulator, H.Empty)))) 24;
      Q.size_def (W.words (H.Cell (accumulator, H.Empty))) 16;
      Q.size_def (Q.Word (V.payload accumulator, Q.End)) 8; Q.size_def Q.End 0);
    ghost_ (Wasm_scatter_range.right_identity (W.words rest);
      Wasm_scatter_range.correct fragment.Save.copies tail_plan 0 0 (W.words leading) (W.words rest) state.X.memory source 48 48 ();
      W.append leading rest;
      Hmc_frame_segments.append_def leading rest;
      Hmc_frame_segments.append_def (H.Cell (current, H.Cell (accumulator, H.Empty))) rest;
      Hmc_frame_segments.append_def (H.Cell (accumulator, H.Empty)) rest; Hmc_frame_segments.append_def H.Empty rest;
      Wasm_word_sequence_algebra.size (W.words leading) (W.words rest) 48 (16 * count) width ());
    let copied = Memory.correct fragment.Save.copies (W.words old_cells) width state base limit source source_local base_local () in
    let bytes = Q.encode (W.words new_cells) copied.Memory.suffix in
    let memory = Wasm_memory_splice.replace copied.Memory.memory base copied.Memory.bytes bytes () in
    ghost_ (cells_def fragment.Save.pc current accumulator rest;
      W.words_def old_cells; W.words_def new_cells; V.tag_def (V.Word (Header.number old_pc)); V.tag_def (V.Word (Header.number fragment.Save.pc));
      V.payload_def (V.Word (Header.number old_pc)); V.payload_def (V.Word (Header.number fragment.Save.pc));
      let prefix = Q.Word (V.tag (V.Word (Header.number old_pc)), Q.End) in
      let remaining = W.words (H.Cell (current, H.Cell (accumulator, rest))) in
      Q.append_def prefix (Q.Word (Header.number old_pc, remaining)); Q.append_def Q.End (Q.Word (Header.number old_pc, remaining));
      Q.append_def prefix (Q.Word (Header.number fragment.Save.pc, remaining)); Q.append_def Q.End (Q.Word (Header.number fragment.Save.pc, remaining));
      Q.size_def prefix 8; Q.size_def Q.End 0;
      Wasm_sequence_update.at prefix remaining copied.Memory.memory memory copied.Memory.bytes bytes copied.Memory.suffix
        (Header.number old_pc) (Header.number fragment.Save.pc) base 8 (base + 8) ();
      Hmc_wasm_pc_update.offset_def ();
      Hmc_wasm_pc_update.correct fragment.Save.pc base_local {X.memory = copied.Memory.memory; machine = state.X.machine} base memory ();
      Save.emit_def fragment source_local base_local;
      X.append_correct (Wasm_cross_copy.emit fragment.Save.copies source_local base_local) (Hmc_wasm_pc_update.emit fragment.Save.pc base_local) state;
      W.recover new_cells bytes copied.Memory.suffix ();
      Hmc_heap_image_prefix.transitive state.X.memory copied.Memory.memory memory base ();
      Bounds.same_length state.X.memory memory limit ());
    ghost_ (extent_def count;
      H.length_def new_cells; H.length_def (H.Cell (current, H.Cell (accumulator, rest))); H.length_def (H.Cell (accumulator, rest));
      Index.represents_def (H.length new_cells) (3 + count);
      Index.represents_def (H.length (H.Cell (current, H.Cell (accumulator, rest)))) (2 + count);
      Index.represents_def (H.length (H.Cell (accumulator, rest))) (1 + count);
      W.size new_cells (3 + count) width ();
      Q.prefix (W.words new_cells) bytes bytes copied.Memory.suffix copied.Memory.suffix width ();
      Wasm_cell.shift memory base width (base + width) bytes ();
      S.add32_def base width);
    {memory; bytes; suffix = copied.Memory.suffix}

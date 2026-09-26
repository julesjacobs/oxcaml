module B = Wasm_u32
module D = Hm_declarative
module V = Hmc_tagged_cell
module H = Hmc_heap_objects
module F = Hmc_heap_frame
module G = Hmc_cfg_ir
module Codec = Hmc_pointer_frame_codec
module Wire = Hmc_heap_wire
module Header = Hmc_wasm_header_update
module Cells = Hmc_wasm_call_save_memory
module Seg = Hmc_frame_segments
module Plan = Wasm_parallel_copy
module Copy = Wasm_cross_copy
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module L = Wasm_locals
module Bytes = Hmc_linear_bytes
module Bounds = Hmc_linear_bounds
module M = Wasm_memory
module P = Hmc_linear_preservation
let[@def] (plan @ total) (u : unit) = Plan.Copy (32, 32, Plan.Copy (40, 40, Plan.End))
let[@def] (emit @ total) (source_local : B.u32) (caller_local : B.u32) = Copy.emit (plan ()) source_local caller_local
let[@def] (tag_offset @ total) (u : unit) : B.u32 = 32
let[@def] (payload_offset @ total) (u : unit) : B.u32 = 40
let[@def] (end_offset @ total) (u : unit) : B.u32 = 48
type result = {memory : B.bytes; bytes : B.bytes}
let (update @ total) : (signature : G.signature) @ immutable -> (pc : D.index) @ immutable -> (encoded_pc : B.u32) ->
    (saved : F.activation) @ immutable -> (rest : H.cells) @ immutable -> (decoded_tail : H.cells) @ immutable -> (value : V.value) @ immutable ->
    (state : X.state) @ immutable -> (source : B.u32) -> (base : B.u32) -> (limit : B.u32) ->
    (before : B.bytes) @ immutable -> (suffix : B.bytes) @ immutable -> (source_local : B.u32) -> (caller_local : B.u32) ->
    {u : unit | base <= 4294967247 && Bytes.drop state.X.memory base === Some before
      && Wire.decode_cells (H.length (Cells.cells encoded_pc saved.F.current saved.F.accumulator rest)) before ===
        Some (Cells.cells encoded_pc saved.F.current saved.F.accumulator rest, suffix)
      && Codec.decode signature pc (H.Cell (saved.F.current, H.Cell (saved.F.accumulator, rest))) === Some (saved, decoded_tail)
      && M.load state.X.memory source (tag_offset ()) M.W64 === Some (S.I64 (V.tag value))
      && M.load state.X.memory source (payload_offset ()) M.W64 === Some (S.I64 (V.payload value))
      && Bounds.covers state.X.memory limit
      && L.get state.X.machine.E.locals source_local === Some (S.I32 source)
      && L.get state.X.machine.E.locals caller_local === Some (S.I32 base)} ->
    {out : result | X.run (emit source_local caller_local) state === X.Done {X.memory = out.memory; machine = state.X.machine}
      && Bytes.drop out.memory base === Some out.bytes && P.equal_prefix base state.X.memory out.memory
      && Wire.decode_cells (H.length (Cells.cells encoded_pc saved.F.current value rest)) out.bytes ===
        Some (Cells.cells encoded_pc saved.F.current value rest, suffix)
      && Codec.decode signature pc (H.Cell (saved.F.current, H.Cell (value, rest))) === Some ({saved with F.accumulator = value}, decoded_tail)
      && not (Bytes.drop state.X.memory (S.add32 base (end_offset ())) === None)
      && Bytes.drop out.memory (S.add32 base (end_offset ())) === Bytes.drop state.X.memory (S.add32 base (end_offset ()))
      && V.length out.memory === V.length state.X.memory && Bounds.covers out.memory limit} @ immutable =
  fun signature pc encoded_pc saved rest decoded_tail value state source base limit before suffix source_local caller_local premise ->
    let prefix = H.Cell (V.Word (Header.number encoded_pc), H.Cell (saved.F.current, H.Empty)) in
    let cells = Cells.cells encoded_pc saved.F.current value rest in
    let bytes = Wire.encode_cells cells suffix in
    let memory = Wasm_memory_splice.replace state.X.memory base before bytes () in
    ghost_ (Cells.cells_def encoded_pc saved.F.current saved.F.accumulator rest;
      Cells.cells_def encoded_pc saved.F.current value rest;
      Seg.append_def prefix (H.Cell (saved.F.accumulator, rest)); Seg.append_def (H.Cell (saved.F.current, H.Empty)) (H.Cell (saved.F.accumulator, rest)); Seg.append_def H.Empty (H.Cell (saved.F.accumulator, rest));
      Seg.append_def prefix (H.Cell (value, rest)); Seg.append_def (H.Cell (saved.F.current, H.Empty)) (H.Cell (value, rest)); Seg.append_def H.Empty (H.Cell (value, rest));
      H.length_def prefix; H.length_def (H.Cell (saved.F.current, H.Empty)); H.length_def H.Empty;
      Hmc_u32_index.represents_def (D.S (D.S D.Z)) 2; Hmc_u32_index.represents_def (D.S D.Z) 1; Hmc_u32_index.represents_def D.Z 0;
      Copy.apply_def Plan.End state.X.memory source base; tag_offset_def (); payload_offset_def ());
    let _middle = Hmc_wasm_cross_cell_update.correct prefix rest saved.F.accumulator value 2 state.X.memory memory base 32 40 before bytes suffix state.X.memory 32 40 source Plan.End () in
    ghost_ (plan_def (); emit_def source_local caller_local;
      Copy.correct (plan ()) source_local caller_local state source base memory ();
      Codec.decode_def signature pc (H.Cell (saved.F.current, H.Cell (saved.F.accumulator, rest)));
      Codec.decode_def signature pc (H.Cell (saved.F.current, H.Cell (value, rest)));
      Bounds.same_length state.X.memory memory limit ());
    ghost_ (end_offset_def ());
    let old_header = Cells.header encoded_pc saved.F.current saved.F.accumulator in
    let new_header = Cells.header encoded_pc saved.F.current value in
    ghost_ (Cells.header_def encoded_pc saved.F.current saved.F.accumulator; Cells.header_def encoded_pc saved.F.current value;
      H.length_def old_header; H.length_def new_header;
      H.length_def (H.Cell (saved.F.current, H.Cell (saved.F.accumulator, H.Empty)));
      H.length_def (H.Cell (saved.F.current, H.Cell (value, H.Empty)));
      H.length_def (H.Cell (saved.F.accumulator, H.Empty)); H.length_def (H.Cell (value, H.Empty)); H.length_def H.Empty;
      Hmc_u32_index.represents_def (D.S (D.S (D.S D.Z))) 3;
      Hmc_u32_index.represents_def (D.S (D.S D.Z)) 2; Hmc_u32_index.represents_def (D.S D.Z) 1; Hmc_u32_index.represents_def D.Z 0;
      Hmc_wire_word_sequence.size old_header 3 48 (); Hmc_wire_word_sequence.size new_header 3 48 ();
      Hmc_wire_word_sequence.decode (H.length (Cells.cells encoded_pc saved.F.current saved.F.accumulator rest)) before (Cells.cells encoded_pc saved.F.current saved.F.accumulator rest) suffix ();
      Hmc_wire_word_sequence.decode (H.length cells) bytes cells suffix ();
      Hmc_wire_word_sequence.append old_header rest; Hmc_wire_word_sequence.append new_header rest;
      Seg.append_def old_header rest; Seg.append_def new_header rest;
      Seg.append_def (H.Cell (saved.F.current, H.Cell (saved.F.accumulator, H.Empty))) rest;
      Seg.append_def (H.Cell (saved.F.current, H.Cell (value, H.Empty))) rest;
      Seg.append_def (H.Cell (saved.F.accumulator, H.Empty)) rest; Seg.append_def (H.Cell (value, H.Empty)) rest; Seg.append_def H.Empty rest);
    let old_rest = Wasm_word_sequence.split (Hmc_wire_word_sequence.words old_header) (Hmc_wire_word_sequence.words rest) before suffix () in
    let new_rest = Wasm_word_sequence.split (Hmc_wire_word_sequence.words new_header) (Hmc_wire_word_sequence.words rest) bytes suffix () in
    ghost_ (Wasm_word_sequence.unique (Hmc_wire_word_sequence.words rest) old_rest new_rest suffix ();
      Wasm_word_sequence.prefix (Hmc_wire_word_sequence.words old_header) before before old_rest old_rest 48 ();
      Wasm_word_sequence.prefix (Hmc_wire_word_sequence.words new_header) bytes bytes new_rest new_rest 48 ();
      Wasm_cell.shift state.X.memory base 48 (base + 48) before ();
      Wasm_cell.shift memory base 48 (base + 48) bytes ();
      S.add32_def base 48);
    {memory; bytes}

let (correct @ total) : (signature : G.signature) @ immutable -> (pc : D.index) @ immutable -> (encoded_pc : B.u32) ->
    (saved : F.activation) @ immutable -> (rest : H.cells) @ immutable -> (value : V.value) @ immutable ->
    (state : X.state) @ immutable -> (source : B.u32) -> (base : B.u32) -> (limit : B.u32) ->
    (before : B.bytes) @ immutable -> (suffix : B.bytes) @ immutable -> (source_local : B.u32) -> (caller_local : B.u32) ->
    {u : unit | base <= 4294967247 && Bytes.drop state.X.memory base === Some before
      && Wire.decode_cells (H.length (Cells.cells encoded_pc saved.F.current saved.F.accumulator rest)) before ===
        Some (Cells.cells encoded_pc saved.F.current saved.F.accumulator rest, suffix)
      && Codec.decode signature pc (H.Cell (saved.F.current, H.Cell (saved.F.accumulator, rest))) === Some (saved, H.Empty)
      && M.load state.X.memory source (tag_offset ()) M.W64 === Some (S.I64 (V.tag value))
      && M.load state.X.memory source (payload_offset ()) M.W64 === Some (S.I64 (V.payload value))
      && Bounds.covers state.X.memory limit
      && L.get state.X.machine.E.locals source_local === Some (S.I32 source)
      && L.get state.X.machine.E.locals caller_local === Some (S.I32 base)} ->
    {out : result | X.run (emit source_local caller_local) state === X.Done {X.memory = out.memory; machine = state.X.machine}
      && Bytes.drop out.memory base === Some out.bytes && P.equal_prefix base state.X.memory out.memory
      && Wire.decode_cells (H.length (Cells.cells encoded_pc saved.F.current value rest)) out.bytes ===
        Some (Cells.cells encoded_pc saved.F.current value rest, suffix)
      && Codec.decode signature pc (H.Cell (saved.F.current, H.Cell (value, rest))) === Some ({saved with F.accumulator = value}, H.Empty)
      && not (Bytes.drop state.X.memory (S.add32 base (end_offset ())) === None)
      && Bytes.drop out.memory (S.add32 base (end_offset ())) === Bytes.drop state.X.memory (S.add32 base (end_offset ()))
      && V.length out.memory === V.length state.X.memory && Bounds.covers out.memory limit} @ immutable =
  fun signature pc encoded_pc saved rest value state source base limit before suffix source_local caller_local premise ->
    update signature pc encoded_pc saved rest H.Empty value state source base limit before suffix source_local caller_local ()

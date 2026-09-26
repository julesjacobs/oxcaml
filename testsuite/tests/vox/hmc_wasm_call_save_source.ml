module B = Wasm_u32
module D = Hm_declarative
module G = Hmc_cfg_ir
module H = Hmc_heap_objects
module F = Hmc_heap_frame
module V = Hmc_tagged_cell
module R = Hmc_wasm_relayout
module Index = Hmc_u32_index
module Codec = Hmc_pointer_frame_codec
module Save = Hmc_wasm_call_save
module Cells = Hmc_wasm_call_save_memory
module Padded = Hmc_wasm_call_save_padded
module Finish = Hmc_wasm_frame_pad_finish
module Pad = Hmc_wasm_frame_padding
module Slice = Hmc_frame_call_slices
module Reads = Hmc_wasm_call_save_reads
module Write = Wasm_mixed_write
module Seg = Hmc_frame_segments
module Wire = Hmc_heap_wire
module Header = Hmc_wasm_header_update
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module L = Wasm_locals
module Bytes = Hmc_linear_bytes
module Bounds = Hmc_linear_bounds
module P = Hmc_linear_preservation
type result = {written : Finish.result; view : Slice.result}
let (correct @ total) : (signature : G.signature) @ immutable -> (activation : F.activation) @ immutable ->
    (cells : H.cells) @ immutable -> (old_padding : H.cells) @ immutable -> (context : D.context) @ immutable ->
    (ty : D.mono) @ immutable -> (schema : G.temporaries) @ immutable -> (next : D.index) @ immutable ->
    (env_count : R.count) -> (count : R.count) -> (old_pc : B.u32) -> (fragment : Save.fragment) @ immutable -> (capacity : R.count) ->
    (padding : Write.writes) @ immutable -> (padding_count : R.count) -> (padding_length : D.index) @ immutable ->
    (state : X.state) @ immutable -> (source : B.u32) -> (base : B.u32) -> (limit : B.u32) ->
    (bytes : B.bytes) @ immutable -> (suffix : B.bytes) @ immutable -> (source_local : B.u32) -> (base_local : B.u32) ->
    {u : unit | signature.G.temporaries === G.Value (context, ty, schema)
      && Codec.decode signature activation.F.pc cells === Some (activation, old_padding)
      && Index.represents activation.F.pc old_pc && Save.matches signature next capacity fragment
      && Index.represents (Codec.locals_size signature.G.locals) env_count
      && Index.represents (D.add (Codec.locals_size context) (Codec.temporaries_size schema)) count
      && 3 + env_count + count <= 268435452 && source + 64 + 16 * env_count + 16 * count <= 4294967296
      && Index.represents padding_length padding_count && 3 + count + padding_count <= 268435452
      && Pad.matches padding (3 + count) padding_length && base + 48 + 16 * count + 16 * padding_count <= limit
      && Bounds.covers state.X.memory limit && Bytes.drop state.X.memory source === Some bytes
      && Wire.decode_cells (D.S (H.length cells)) bytes === Some (H.Cell (V.Word (Header.number old_pc), cells), suffix)
      && L.get state.X.machine.E.locals source_local === Some (S.I32 source)
      && L.get state.X.machine.E.locals base_local === Some (S.I32 base)} ->
    {out : result | H.length out.view.Slice.remaining === D.add (Codec.locals_size context) (Codec.temporaries_size schema)
      && Codec.decode (Hmc_frame_call_save.signature context schema) next
        (H.Cell (activation.F.current, H.Cell (activation.F.accumulator, out.view.Slice.remaining))) === Some (out.view.Slice.saved, H.Empty)
      && X.run (Padded.emit fragment padding source_local base_local) state === X.Done {X.memory = out.written.Finish.memory; machine = state.X.machine}
      && activation.F.temporaries === F.Value (out.view.Slice.closure, out.view.Slice.saved.F.env, out.view.Slice.saved.F.temporaries)
      && out.view.Slice.saved.F.pc === next && out.view.Slice.saved.F.current === activation.F.current && out.view.Slice.saved.F.accumulator === activation.F.accumulator
      && Bytes.drop out.written.Finish.memory base === Some out.written.Finish.bytes
      && Wire.decode_cells (H.length (Seg.append (Cells.cells fragment.Save.pc activation.F.current activation.F.accumulator out.view.Slice.remaining) (Pad.cells padding_length))) out.written.Finish.bytes ===
        Some (Seg.append (Cells.cells fragment.Save.pc activation.F.current activation.F.accumulator out.view.Slice.remaining) (Pad.cells padding_length), out.written.Finish.suffix)
      && Codec.decode (Hmc_frame_call_save.signature context schema) next
        (Seg.append (H.Cell (activation.F.current, H.Cell (activation.F.accumulator, out.view.Slice.remaining))) (Pad.cells padding_length)) === Some (out.view.Slice.saved, Pad.cells padding_length)
      && Bytes.drop state.X.memory (S.add32 base (S.add32 (Hmc_wasm_call_save_memory.extent count) (Hmc_wasm_padding_memory.offset padding_count))) === Some out.written.Finish.suffix
      && Bytes.drop out.written.Finish.memory (S.add32 base (S.add32 (Hmc_wasm_call_save_memory.extent count) (Hmc_wasm_padding_memory.offset padding_count))) === Some out.written.Finish.suffix
      && P.equal_prefix base state.X.memory out.written.Finish.memory
      && V.length out.written.Finish.memory === V.length state.X.memory && Bounds.covers out.written.Finish.memory limit} @ immutable =
  fun signature activation cells old_padding context ty schema next env_count count old_pc fragment capacity padding padding_count padding_length state source base limit bytes suffix source_local base_local premise ->
    let view = Reads.correct signature activation cells old_padding context ty schema next env_count count (3 + env_count) old_pc state.X.memory source bytes suffix () in
    let tail = Hmc_wasm_call_save_layout.correct fragment signature next capacity context ty schema env_count view.Slice.remaining old_pc activation.F.current activation.F.accumulator () in
    let written = Padded.correct fragment tail padding padding_count padding_length old_pc activation.F.current activation.F.accumulator view.Slice.remaining
      (3 + env_count) count state source base limit source_local base_local () in
    ghost_ (Hmc_frame_decode_suffix.correct (Hmc_frame_call_save.signature context schema) next
      (H.Cell (activation.F.current, H.Cell (activation.F.accumulator, view.Slice.remaining))) view.Slice.saved H.Empty (Pad.cells padding_length) ();
      Seg.append_def H.Empty (Pad.cells padding_length));
    {written; view}

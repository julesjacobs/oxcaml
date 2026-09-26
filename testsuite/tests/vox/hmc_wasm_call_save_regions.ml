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
module Source = Hmc_wasm_call_save_source
module K = Hmc_closure_ir
module Image = Hmc_heap_image
module Runtime = Hmc_runtime_closures
module Table = Hmc_runtime_descriptor_table
module Transport = Hmc_wasm_frame_transport
let rec (above_lower @ total) : (heap : H.heap) @ immutable -> (upper : B.u32) -> (lower : B.u32) ->
    {u : unit | lower <= upper && Hmc_heap_image_suffix.above heap upper} ->
    {u : unit | Hmc_heap_image_suffix.above heap lower} @ ghost = fun heap upper lower premise -> ghost_ (
    Hmc_heap_image_suffix.above_def heap upper; Hmc_heap_image_suffix.above_def heap lower;
    match heap with H.Empty_heap _ -> () | H.Allocate (_, rest) -> above_lower rest upper lower ())
type result = {saved : Source.result; active : Transport.result}
let (correct @ total) : (table : K.table) @ immutable -> (heap : H.heap) @ immutable ->
    (runtime : Runtime.table) @ immutable -> (table_base : B.u32) -> (table_count : Table.count) ->
    (frame_count : R.count) -> (frame_stop : B.u32) -> (signature : G.signature) @ immutable -> (activation : F.activation) @ immutable ->
    (cells : H.cells) @ immutable -> (old_padding : H.cells) @ immutable -> (context : D.context) @ immutable ->
    (ty : D.mono) @ immutable -> (schema : G.temporaries) @ immutable -> (next : D.index) @ immutable ->
    (env_count : R.count) -> (count : R.count) -> (old_pc : B.u32) -> (fragment : Save.fragment) @ immutable -> (capacity : R.count) ->
    (padding : Write.writes) @ immutable -> (padding_count : R.count) -> (padding_length : D.index) @ immutable ->
    (state : X.state) @ immutable -> (source : B.u32) -> (base : B.u32) -> (limit : B.u32) ->
    (bytes : B.bytes) @ immutable -> (suffix : B.bytes) @ immutable -> (source_local : B.u32) -> (base_local : B.u32) ->
    {u : unit | H.valid table heap && Image.related state.X.memory heap && (H.used heap <= base || Hmc_heap_image_suffix.above heap (S.add32 base (S.add32 (Hmc_wasm_call_save_memory.extent count) (Hmc_wasm_padding_memory.offset padding_count))))
      && Table.related runtime state.X.memory table_base table_count && table_base + 32 * table_count <= base
      && Index.represents (H.length cells) frame_count && frame_stop = source + 16 + 16 * frame_count && frame_stop <= base
      && signature.G.temporaries === G.Value (context, ty, schema)
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
    {out : result | H.length out.saved.Source.view.Slice.remaining === D.add (Codec.locals_size context) (Codec.temporaries_size schema)
      && Codec.decode (Hmc_frame_call_save.signature context schema) next
        (H.Cell (activation.F.current, H.Cell (activation.F.accumulator, out.saved.Source.view.Slice.remaining))) === Some (out.saved.Source.view.Slice.saved, H.Empty)
      && X.run (Padded.emit fragment padding source_local base_local) state === X.Done {X.memory = out.saved.Source.written.Finish.memory; machine = state.X.machine}
      && activation.F.temporaries === F.Value (out.saved.Source.view.Slice.closure, out.saved.Source.view.Slice.saved.F.env, out.saved.Source.view.Slice.saved.F.temporaries)
      && out.saved.Source.view.Slice.saved.F.pc === next && out.saved.Source.view.Slice.saved.F.current === activation.F.current && out.saved.Source.view.Slice.saved.F.accumulator === activation.F.accumulator
      && Bytes.drop out.saved.Source.written.Finish.memory base === Some out.saved.Source.written.Finish.bytes
      && Wire.decode_cells (H.length (Seg.append (Cells.cells fragment.Save.pc activation.F.current activation.F.accumulator out.saved.Source.view.Slice.remaining) (Pad.cells padding_length))) out.saved.Source.written.Finish.bytes ===
        Some (Seg.append (Cells.cells fragment.Save.pc activation.F.current activation.F.accumulator out.saved.Source.view.Slice.remaining) (Pad.cells padding_length), out.saved.Source.written.Finish.suffix)
      && Codec.decode (Hmc_frame_call_save.signature context schema) next
        (Seg.append (H.Cell (activation.F.current, H.Cell (activation.F.accumulator, out.saved.Source.view.Slice.remaining))) (Pad.cells padding_length)) === Some (out.saved.Source.view.Slice.saved, Pad.cells padding_length)
      && P.equal_prefix base state.X.memory out.saved.Source.written.Finish.memory
      && Image.related out.saved.Source.written.Finish.memory heap
      && Table.related runtime out.saved.Source.written.Finish.memory table_base table_count
      && Bytes.drop out.saved.Source.written.Finish.memory source === Some out.active.Transport.bytes
      && Wire.decode_cells (D.S (H.length cells)) out.active.Transport.bytes === Some (H.Cell (V.Word (Header.number old_pc), cells), out.active.Transport.tail)
      && V.length out.saved.Source.written.Finish.memory === V.length state.X.memory && Bounds.covers out.saved.Source.written.Finish.memory limit} @ immutable =
  fun table heap runtime table_base table_count frame_count frame_stop signature activation cells old_padding context ty schema next env_count count old_pc fragment capacity padding padding_count padding_length state source base limit bytes suffix source_local base_local premise ->
    let saved = Source.correct signature activation cells old_padding context ty schema next env_count count old_pc fragment capacity
      padding padding_count padding_length state source base limit bytes suffix source_local base_local () in
    ghost_ (if H.used heap <= base then Image.preserve table state.X.memory saved.Source.written.Finish.memory heap base ()
      else Hmc_heap_image_suffix.preserve state.X.memory saved.Source.written.Finish.memory heap
        (S.add32 base (S.add32 (Cells.extent count) (Hmc_wasm_padding_memory.offset padding_count))) ();
      Table.preserve runtime state.X.memory saved.Source.written.Finish.memory table_base table_count base ();
      P.shrink base frame_stop state.X.memory saved.Source.written.Finish.memory ();
      H.length_def (H.Cell (V.Word (Header.number old_pc), cells));
      Index.represents_def (D.S (H.length cells)) (frame_count + 1));
    let active = Transport.correct state.X.memory saved.Source.written.Finish.memory source frame_stop (frame_count + 1)
      (H.Cell (V.Word (Header.number old_pc), cells)) bytes suffix () in
    {saved; active}

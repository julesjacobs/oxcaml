module B = Wasm_u32
module D = Hm_declarative
module G = Hmc_cfg_ir
module H = Hmc_heap_objects
module F = Hmc_heap_frame
module V = Hmc_tagged_cell
module Codec = Hmc_pointer_frame_codec
module Seg = Hmc_frame_segments
module Slice = Hmc_frame_call_slices
module View = Hmc_frame_slices
module R = Hmc_wasm_relayout
module Index = Hmc_u32_index
module Wire = Hmc_heap_wire
module Header = Hmc_wasm_header_update
module Memory = Hmc_wasm_call_save_memory
module Words = Hmc_wire_word_sequence
module Cross = Wasm_cross_words
module Range = Hmc_wasm_range_copy
module Q = Wasm_word_sequence
let (correct @ total) : (signature : G.signature) @ immutable -> (activation : F.activation) @ immutable ->
    (cells : H.cells) @ immutable -> (padding : H.cells) @ immutable -> (context : D.context) @ immutable ->
    (ty : D.mono) @ immutable -> (schema : G.temporaries) @ immutable -> (next : D.index) @ immutable ->
    (env_count : R.count) -> (count : R.count) -> (position : R.count) -> (pc : B.u32) -> (memory : B.bytes) @ immutable -> (base : B.u32) ->
    (bytes : B.bytes) @ immutable -> (suffix : B.bytes) @ immutable ->
    {u : unit | signature.G.temporaries === G.Value (context, ty, schema)
      && Codec.decode signature activation.F.pc cells === Some (activation, padding)
      && Index.represents (Codec.locals_size signature.G.locals) env_count
      && Index.represents (D.add (Codec.locals_size context) (Codec.temporaries_size schema)) count
      && position = 3 + env_count && 3 + env_count + count <= 268435452 && base + 64 + 16 * env_count + 16 * count <= 4294967296
      && Hmc_linear_bytes.drop memory base === Some bytes
      && Wire.decode_cells (D.S (H.length cells)) bytes === Some (H.Cell (V.Word (Header.number pc), cells), suffix)} ->
    {out : Slice.result | H.length out.Slice.remaining === D.add (Codec.locals_size context) (Codec.temporaries_size schema)
      && activation.F.temporaries === F.Value (out.Slice.closure, out.Slice.saved.F.env, out.Slice.saved.F.temporaries)
      && out.Slice.saved.F.pc === next && out.Slice.saved.F.current === activation.F.current && out.Slice.saved.F.accumulator === activation.F.accumulator
      && Codec.decode (Hmc_frame_call_save.signature context schema) next (H.Cell (activation.F.current, H.Cell (activation.F.accumulator, out.Slice.remaining))) === Some (out.Slice.saved, H.Empty)
      && Cross.reads memory base (Wasm_scatter_memory.zero ()) (Words.words (Memory.header pc activation.F.current activation.F.accumulator))
      && Range.reads memory base position out.Slice.remaining} @ immutable =
  fun signature activation cells padding context ty schema next env_count count position pc memory base bytes suffix premise ->
    let out = Slice.correct signature activation cells padding context ty schema next () in
    let view = View.decode signature activation cells padding () in
    let full = H.Cell (V.Word (Header.number pc), cells) in
    ghost_ (H.length_def full; Wasm_scatter_memory.zero_def ();
      Index.represents_def D.Z 0; Index.represents_def (D.S D.Z) 1; Index.represents_def (D.S (D.S D.Z)) 2;
      Hmc_heap_simple.lookup_def full D.Z;
      Hmc_heap_simple.lookup_def full (D.S D.Z); Hmc_heap_simple.lookup_def cells D.Z;
      Hmc_heap_simple.lookup_def full (D.S (D.S D.Z)); Hmc_heap_simple.lookup_def cells (D.S D.Z);
      Hmc_heap_simple.lookup_def (H.Cell (activation.F.accumulator, view.View.body)) D.Z;
      Hmc_wasm_cells_read.correct memory base bytes (H.length full) full suffix D.Z 0 0 8 (V.Word (Header.number pc)) ();
      Hmc_wasm_cells_read.correct memory base bytes (H.length full) full suffix (D.S D.Z) 1 16 24 activation.F.current ();
      Hmc_wasm_cells_read.correct memory base bytes (H.length full) full suffix (D.S (D.S D.Z)) 2 32 40 activation.F.accumulator ();
      Memory.header_def pc activation.F.current activation.F.accumulator;
      Words.words_def (Memory.header pc activation.F.current activation.F.accumulator);
      Words.words_def (H.Cell (activation.F.current, H.Cell (activation.F.accumulator, H.Empty)));
      Words.words_def (H.Cell (activation.F.accumulator, H.Empty)); Words.words_def H.Empty;
      let words = Words.words (Memory.header pc activation.F.current activation.F.accumulator) in
      Cross.reads_def memory base 0 words;
      match words with Q.Word (_, w1) -> Cross.reads_def memory base 8 w1;
        (match w1 with Q.Word (_, w2) -> Cross.reads_def memory base 16 w2;
          (match w2 with Q.Word (_, w3) -> Cross.reads_def memory base 24 w3;
            (match w3 with Q.Word (_, w4) -> Cross.reads_def memory base 32 w4;
              (match w4 with Q.Word (_, w5) -> Cross.reads_def memory base 40 w5; Cross.reads_def memory base 48 Q.End
              | _ -> ()) | _ -> ()) | _ -> ()) | _ -> ()) | _ -> ());
    ghost_ (Index.represents_def (D.S (Codec.locals_size signature.G.locals)) (1 + env_count);
      Index.represents_def (D.S (D.S (Codec.locals_size signature.G.locals))) (2 + env_count);
      Index.represents_def (D.S (D.S (D.S (Codec.locals_size signature.G.locals)))) (3 + env_count);
      Seg.drop_def (D.S (D.S (D.S (D.S (Codec.locals_size signature.G.locals))))) full;
      Hmc_wasm_range_read.correct out.Slice.remaining (3 + env_count) count (D.S (D.S (D.S (Codec.locals_size signature.G.locals))))
        full out.Slice.start memory base bytes suffix ());
    out

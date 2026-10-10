module B = Wasm_u32
module W = Hmc_word64
module D = Hm_declarative
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module Wire = Hmc_heap_wire
module Words = Hmc_wire_word_sequence
module Q = Wasm_word_sequence
module Four = Wasm_four_words
module Update = Wasm_sequence_update
module Splice = Wasm_memory_splice
module Write = Wasm_frame_write
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module L = Wasm_locals
module Bytes = Hmc_linear_bytes
module P = Hmc_linear_preservation
let[@def] (writes @ total) (head_tag : B.u32) (head_payload : B.u32) (tail_tag : B.u32) (tail_payload : B.u32) =
  Write.Write (0, head_tag, Write.Write (8, head_payload, Write.Write (16, tail_tag, Write.Write (24, tail_payload, Write.End))))
let[@def] (emit @ total) (base_local : B.u32) (head_tag : B.u32) (head_payload : B.u32) (tail_tag : B.u32) (tail_payload : B.u32) =
  Write.emit (writes head_tag head_payload tail_tag tail_payload) base_local
let (words @ total) : (head : V.value) @ immutable -> (tail : V.value) @ immutable -> (bytes : B.bytes) @ immutable -> (suffix : B.bytes) @ immutable ->
    {u : unit | Wire.decode Wire.Cons_schema bytes === Some (Wire.Cons (head, tail), suffix)} ->
    {u : unit | Q.decode (Four.layout (V.tag head) (V.payload head) (V.tag tail) (V.payload tail)) bytes === Some suffix} @ ghost =
  fun head tail bytes suffix premise -> ghost_ (
    Wire.decode_def Wire.Cons_schema bytes;
    Wire.decode_cells_def (D.S (D.S D.Z)) bytes;
    (match V.decode bytes with None -> () | Some (_, rest) ->
      Wire.decode_cells_def (D.S D.Z) rest;
      match V.decode rest with None -> () | Some (_, remaining) -> Wire.decode_cells_def D.Z remaining);
    let cells = Heap.Cell (head, Heap.Cell (tail, Heap.Empty)) in
    Words.decode (D.S (D.S D.Z)) bytes cells suffix ();
    Words.words_def cells; Words.words_def (Heap.Cell (tail, Heap.Empty)); Words.words_def Heap.Empty;
    Four.layout_def (V.tag head) (V.payload head) (V.tag tail) (V.payload tail))
let (correct @ total) : (head : V.value) @ immutable -> (tail : V.value) @ immutable ->
    (old0 : W.t) @ immutable -> (old1 : W.t) @ immutable -> (old2 : W.t) @ immutable -> (old3 : W.t) @ immutable ->
    (state : X.state) @ immutable -> (after : B.bytes) @ immutable -> (base : B.u32) -> (base_local : B.u32) ->
    (head_tag : B.u32) -> (head_payload : B.u32) -> (tail_tag : B.u32) -> (tail_payload : B.u32) ->
    (before_frame : B.bytes) @ immutable -> (after_frame : B.bytes) @ immutable -> (suffix : B.bytes) @ immutable ->
    {u : unit | base <= 4294967264 && P.equal_prefix base state.X.memory after
      && Bytes.drop state.X.memory base === Some before_frame && Bytes.drop after base === Some after_frame
      && Q.decode (Four.layout old0 old1 old2 old3) before_frame === Some suffix
      && Wire.decode Wire.Cons_schema after_frame === Some (Wire.Cons (head, tail), suffix)
      && L.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && L.get state.X.machine.E.locals head_tag === Some (S.I64 (V.tag head))
      && L.get state.X.machine.E.locals head_payload === Some (S.I64 (V.payload head))
      && L.get state.X.machine.E.locals tail_tag === Some (S.I64 (V.tag tail))
      && L.get state.X.machine.E.locals tail_payload === Some (S.I64 (V.payload tail))} ->
    {u : unit | X.run (emit base_local head_tag head_payload tail_tag tail_payload) state === X.Done {X.memory = after; machine = state.X.machine}
      && V.length after === V.length state.X.memory} @ ghost =
  fun head tail old0 old1 old2 old3 state after base base_local head_tag head_payload tail_tag tail_payload before_frame after_frame suffix premise -> ghost_ (
    let a = V.tag head in let b = V.payload head in let c = V.tag tail in let d = V.payload tail in
    words head tail after_frame suffix ();
    let frame1 = Q.encode (Four.layout a old1 old2 old3) suffix in
    let memory1 = Splice.replace state.X.memory base before_frame frame1 () in
    let frame2 = Q.encode (Four.layout a b old2 old3) suffix in
    let memory2 = Splice.replace state.X.memory base before_frame frame2 () in
    let frame3 = Q.encode (Four.layout a b c old3) suffix in
    let memory3 = Splice.replace state.X.memory base before_frame frame3 () in
    Four.expose old0 old1 old2 old3; Four.expose a old1 old2 old3; Four.expose a b old2 old3; Four.expose a b c old3; Four.expose a b c d;
    Four.offset0_def (); Four.offset1_def (); Four.offset2_def (); Four.offset3_def ();
    Update.at Q.End (Q.Word (old1, Q.Word (old2, Q.Word (old3, Q.End)))) state.X.memory memory1 before_frame frame1 suffix old0 a base 0 base ();
    Splice.shared state.X.memory memory1 memory2 base ();
    Update.at (Q.Word (a, Q.End)) (Q.Word (old2, Q.Word (old3, Q.End))) memory1 memory2 frame1 frame2 suffix old1 b base 8 (base + 8) ();
    Splice.shared state.X.memory memory2 memory3 base ();
    Update.at (Q.Word (a, Q.Word (b, Q.End))) (Q.Word (old3, Q.End)) memory2 memory3 frame2 frame3 suffix old2 c base 16 (base + 16) ();
    Splice.shared state.X.memory memory3 after base ();
    Update.at (Q.Word (a, Q.Word (b, Q.Word (c, Q.End)))) Q.End memory3 after frame3 after_frame suffix old3 d base 24 (base + 24) ();
    writes_def head_tag head_payload tail_tag tail_payload;
    Write.apply_def (writes head_tag head_payload tail_tag tail_payload) state.X.memory base state.X.machine.E.locals;
    Write.apply_def (Write.Write (8, head_payload, Write.Write (16, tail_tag, Write.Write (24, tail_payload, Write.End)))) memory1 base state.X.machine.E.locals;
    Write.apply_def (Write.Write (16, tail_tag, Write.Write (24, tail_payload, Write.End))) memory2 base state.X.machine.E.locals;
    Write.apply_def (Write.Write (24, tail_payload, Write.End)) memory3 base state.X.machine.E.locals;
    Write.apply_def Write.End after base state.X.machine.E.locals;
    emit_def base_local head_tag head_payload tail_tag tail_payload;
    Write.correct (writes head_tag head_payload tail_tag tail_payload) base_local state base after ())

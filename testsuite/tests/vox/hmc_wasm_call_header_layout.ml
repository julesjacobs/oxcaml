module B = Wasm_u32
module W = Hmc_word64
module D = Hm_declarative
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module Header = Hmc_wasm_header_update
module Plan = Hmc_wasm_call_header
module Write = Wasm_mixed_write
module Words = Hmc_wire_word_sequence
module Q = Wasm_word_sequence
module Match = Wasm_mixed_words
module S = Wasm_scalar
module L = Wasm_locals
let[@def] (width @ total) (recursive : bool) : B.u32 = if recursive then 80 else 64
let[@def] (cells @ total) (recursive : bool) (pc : W.limb) (closure : B.u32) (argument : V.value @ immutable) =
  let self = if recursive then Heap.Cell (V.Closure_pointer closure, Heap.Empty) else Heap.Empty in
  Heap.Cell (V.Word (Header.number pc), Heap.Cell (V.Closure_pointer closure, Heap.Cell (V.Nil, Heap.Cell (argument, self))))
let (correct @ total) : (recursive : bool) -> (pc : W.limb) -> (closure : B.u32) -> (argument : V.value) @ immutable ->
    (closure_local : B.u32) -> (argument_tag : B.u32) -> (argument_payload : B.u32) -> (locals : S.stack) @ immutable ->
    {u : unit | L.get locals closure_local === Some (S.I32 closure)
      && L.get locals argument_tag === Some (S.I64 (V.tag argument))
      && L.get locals argument_payload === Some (S.I64 (V.payload argument))} ->
    {u : unit | Q.size (Words.words (cells recursive pc closure argument)) (width recursive)
      && Match.matches (Plan.writes recursive pc closure_local argument_tag argument_payload) (Wasm_mixed_memory.zero ()) (Words.words (cells recursive pc closure argument)) locals} @ ghost =
  fun recursive pc closure argument closure_local argument_tag argument_payload locals premise -> ghost_ (
    cells_def recursive pc closure argument; width_def recursive; Wasm_mixed_memory.zero_def ();
    Plan.writes_def recursive pc closure_local argument_tag argument_payload;
    V.tag_def (V.Word (Header.number pc)); V.payload_def (V.Word (Header.number pc));
    V.tag_def (V.Closure_pointer closure); V.payload_def (V.Closure_pointer closure);
    V.tag_def V.Nil; V.payload_def V.Nil;
    Header.number_def 1; Header.number_def 2; Header.number_def 4; Header.number_def 0; Header.number_def closure;
    if not recursive then (
      let c4 = Heap.Empty in
      let c3 = Heap.Cell (argument, c4) in
      let c2 = Heap.Cell (V.Nil, c3) in
      let c1 = Heap.Cell (V.Closure_pointer closure, c2) in
      let c0 = Heap.Cell (V.Word (Header.number pc), c1) in
      Words.words_def c0;
      Words.words_def c1;
      Words.words_def c2;
      Words.words_def c3;
      Words.words_def c4;
      let q8 = Q.End in let p8 = Write.End in
      let q7 = Q.Word (V.payload argument, q8) in let p7 = Write.Write (56, Write.Word_local argument_payload, p8) in
      let q6 = Q.Word (V.tag argument, q7) in let p6 = Write.Write (48, Write.Word_local argument_tag, p7) in
      let q5 = Q.Word (Header.number 0, q6) in let p5 = Write.Write (40, Write.Constant (Header.number 0), p6) in
      let q4 = Q.Word (Header.number 2, q5) in let p4 = Write.Write (32, Write.Constant (Header.number 2), p5) in
      let q3 = Q.Word (Header.number closure, q4) in let p3 = Write.Write (24, Write.Pointer_local closure_local, p4) in
      let q2 = Q.Word (Header.number 4, q3) in let p2 = Write.Write (16, Write.Constant (Header.number 4), p3) in
      let q1 = Q.Word (Header.number pc, q2) in let p1 = Write.Write (8, Write.Constant (Header.number pc), p2) in
      let q0 = Q.Word (Header.number 1, q1) in let p0 = Write.Write (0, Write.Constant (Header.number 1), p1) in
      Q.size_def q0 64; Match.matches_def p0 0 q0 locals; Write.read_def (Write.Constant (Header.number 1)) locals;
      Q.size_def q1 56; Match.matches_def p1 8 q1 locals; Write.read_def (Write.Constant (Header.number pc)) locals;
      Q.size_def q2 48; Match.matches_def p2 16 q2 locals; Write.read_def (Write.Constant (Header.number 4)) locals;
      Q.size_def q3 40; Match.matches_def p3 24 q3 locals; Write.read_def (Write.Pointer_local closure_local) locals;
      Q.size_def q4 32; Match.matches_def p4 32 q4 locals; Write.read_def (Write.Constant (Header.number 2)) locals;
      Q.size_def q5 24; Match.matches_def p5 40 q5 locals; Write.read_def (Write.Constant (Header.number 0)) locals;
      Q.size_def q6 16; Match.matches_def p6 48 q6 locals; Write.read_def (Write.Word_local argument_tag) locals;
      Q.size_def q7 8; Match.matches_def p7 56 q7 locals; Write.read_def (Write.Word_local argument_payload) locals;
      Q.size_def q8 0; Match.matches_def p8 64 q8 locals) else ();
    if recursive then (
      let c5 = Heap.Empty in
      let c4 = Heap.Cell (V.Closure_pointer closure, c5) in
      let c3 = Heap.Cell (argument, c4) in
      let c2 = Heap.Cell (V.Nil, c3) in
      let c1 = Heap.Cell (V.Closure_pointer closure, c2) in
      let c0 = Heap.Cell (V.Word (Header.number pc), c1) in
      Words.words_def c0;
      Words.words_def c1;
      Words.words_def c2;
      Words.words_def c3;
      Words.words_def c4;
      Words.words_def c5;
      let q10 = Q.End in let p10 = Write.End in
      let q9 = Q.Word (Header.number closure, q10) in let p9 = Write.Write (72, Write.Pointer_local closure_local, p10) in
      let q8 = Q.Word (Header.number 4, q9) in let p8 = Write.Write (64, Write.Constant (Header.number 4), p9) in
      let q7 = Q.Word (V.payload argument, q8) in let p7 = Write.Write (56, Write.Word_local argument_payload, p8) in
      let q6 = Q.Word (V.tag argument, q7) in let p6 = Write.Write (48, Write.Word_local argument_tag, p7) in
      let q5 = Q.Word (Header.number 0, q6) in let p5 = Write.Write (40, Write.Constant (Header.number 0), p6) in
      let q4 = Q.Word (Header.number 2, q5) in let p4 = Write.Write (32, Write.Constant (Header.number 2), p5) in
      let q3 = Q.Word (Header.number closure, q4) in let p3 = Write.Write (24, Write.Pointer_local closure_local, p4) in
      let q2 = Q.Word (Header.number 4, q3) in let p2 = Write.Write (16, Write.Constant (Header.number 4), p3) in
      let q1 = Q.Word (Header.number pc, q2) in let p1 = Write.Write (8, Write.Constant (Header.number pc), p2) in
      let q0 = Q.Word (Header.number 1, q1) in let p0 = Write.Write (0, Write.Constant (Header.number 1), p1) in
      Q.size_def q0 80; Match.matches_def p0 0 q0 locals; Write.read_def (Write.Constant (Header.number 1)) locals;
      Q.size_def q1 72; Match.matches_def p1 8 q1 locals; Write.read_def (Write.Constant (Header.number pc)) locals;
      Q.size_def q2 64; Match.matches_def p2 16 q2 locals; Write.read_def (Write.Constant (Header.number 4)) locals;
      Q.size_def q3 56; Match.matches_def p3 24 q3 locals; Write.read_def (Write.Pointer_local closure_local) locals;
      Q.size_def q4 48; Match.matches_def p4 32 q4 locals; Write.read_def (Write.Constant (Header.number 2)) locals;
      Q.size_def q5 40; Match.matches_def p5 40 q5 locals; Write.read_def (Write.Constant (Header.number 0)) locals;
      Q.size_def q6 32; Match.matches_def p6 48 q6 locals; Write.read_def (Write.Word_local argument_tag) locals;
      Q.size_def q7 24; Match.matches_def p7 56 q7 locals; Write.read_def (Write.Word_local argument_payload) locals;
      Q.size_def q8 16; Match.matches_def p8 64 q8 locals; Write.read_def (Write.Constant (Header.number 4)) locals;
      Q.size_def q9 8; Match.matches_def p9 72 q9 locals; Write.read_def (Write.Pointer_local closure_local) locals;
      Q.size_def q10 0; Match.matches_def p10 80 q10 locals) else ();
    ())

module B = Wasm_u32
module V = Hmc_tagged_cell
module Wire = Hmc_heap_wire
module Four = Wasm_four_words
module Q = Wasm_word_sequence
module Read = Wasm_word_read
module Bounds = Hmc_linear_bounds
module Bytes = Hmc_linear_bytes
module P = Hmc_linear_preservation
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module L = Wasm_locals
module Write = Hmc_wasm_cons_write
type result = {memory : B.bytes; bytes : B.bytes; suffix : B.bytes}
let (correct @ total) : (head : V.value) @ immutable -> (tail : V.value) @ immutable ->
    (state : X.state) @ immutable -> (base : B.u32) -> (limit : B.u32) -> (base_local : B.u32) ->
    (head_tag : B.u32) -> (head_payload : B.u32) -> (tail_tag : B.u32) -> (tail_payload : B.u32) ->
    {u : unit | base + 32 <= limit && Bounds.covers state.X.memory limit
      && L.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && L.get state.X.machine.E.locals head_tag === Some (S.I64 (V.tag head))
      && L.get state.X.machine.E.locals head_payload === Some (S.I64 (V.payload head))
      && L.get state.X.machine.E.locals tail_tag === Some (S.I64 (V.tag tail))
      && L.get state.X.machine.E.locals tail_payload === Some (S.I64 (V.payload tail))} ->
    {out : result | X.run (Write.emit base_local head_tag head_payload tail_tag tail_payload) state === X.Done {X.memory = out.memory; machine = state.X.machine}
      && V.length out.memory === V.length state.X.memory && P.equal_prefix base state.X.memory out.memory
      && Bytes.drop out.memory base === Some out.bytes
      && Wire.decode Wire.Cons_schema out.bytes === Some (Wire.Cons (head, tail), out.suffix)
      && Bytes.drop state.X.memory (S.add32 base (Four.width ())) === Some out.suffix
      && Bytes.drop out.memory (S.add32 base (Four.width ())) === Some out.suffix
      && Bounds.covers out.memory limit} @ immutable =
  fun head tail state base limit base_local head_tag head_payload tail_tag tail_payload premise ->
    let before = Bounds.suffix state.X.memory limit base () in
    ghost_ (Bounds.distance_def base limit);
    let first = Read.read before (limit - base) () in
    ghost_ (Read.remaining_def (limit - base));
    let second = Read.read first.Read.tail (limit - base - 8) () in
    ghost_ (Read.remaining_def (limit - base - 8));
    let third = Read.read second.Read.tail (limit - base - 16) () in
    ghost_ (Read.remaining_def (limit - base - 16));
    let fourth = Read.read third.Read.tail (limit - base - 24) () in
    let suffix = fourth.Read.tail in
    let bytes = Wire.encode (Wire.Cons (head, tail)) suffix in
    let memory = Wasm_memory_splice.replace state.X.memory base before bytes () in
    ghost_ (Wire.schema_def (Wire.Cons (head, tail));
      Four.layout_def first.Read.word second.Read.word third.Read.word fourth.Read.word;
      Q.decode_def (Four.layout first.Read.word second.Read.word third.Read.word fourth.Read.word) before;
      Q.decode_def (Q.Word (second.Read.word, Q.Word (third.Read.word, Q.Word (fourth.Read.word, Q.End)))) first.Read.tail;
      Q.decode_def (Q.Word (third.Read.word, Q.Word (fourth.Read.word, Q.End))) second.Read.tail;
      Q.decode_def (Q.Word (fourth.Read.word, Q.End)) third.Read.tail; Q.decode_def Q.End suffix;
      Hmc_word64.equal_def first.Read.word first.Read.word; Hmc_word64.equal_def second.Read.word second.Read.word;
      Hmc_word64.equal_def third.Read.word third.Read.word; Hmc_word64.equal_def fourth.Read.word fourth.Read.word;
      Write.correct head tail first.Read.word second.Read.word third.Read.word fourth.Read.word state memory base base_local
        head_tag head_payload tail_tag tail_payload before bytes suffix ();
      Four.width_def ();
      Four.size first.Read.word second.Read.word third.Read.word fourth.Read.word;
      Q.prefix (Four.layout first.Read.word second.Read.word third.Read.word fourth.Read.word) before before suffix suffix 32 ();
      Write.words head tail bytes suffix ();
      Four.size (V.tag head) (V.payload head) (V.tag tail) (V.payload tail);
      Four.width_def ();
      Q.prefix (Four.layout (V.tag head) (V.payload head) (V.tag tail) (V.payload tail)) bytes bytes suffix suffix 32 ();
      Wasm_cell.shift state.X.memory base 32 (base + 32) before ();
      Wasm_cell.shift memory base 32 (base + 32) bytes ();
      S.add32_def base 32;
      Bounds.same_length state.X.memory memory limit ());
    {memory; bytes; suffix}

module B = Wasm_u32
module V = Hmc_tagged_cell
module Q = Wasm_word_sequence
module Write = Wasm_mixed_write
module Words = Wasm_mixed_words
module Raw = Wasm_words_read
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module L = Wasm_locals
module Bytes = Hmc_linear_bytes
module Bounds = Hmc_linear_bounds
module P = Hmc_linear_preservation
let[@def] (zero @ total) (u : unit) : B.u32 = 0
type result = {memory : B.bytes; bytes : B.bytes; suffix : B.bytes}
let (correct @ total) : (writes : Write.writes) @ immutable -> (values : Q.words) @ immutable -> (width : B.u32) ->
    (state : X.state) @ immutable -> (base : B.u32) -> (limit : B.u32) -> (base_local : B.u32) ->
    {u : unit | Words.matches writes (zero ()) values state.X.machine.E.locals && Q.size values width && width <= 4294967288
      && base + width <= limit && Bounds.covers state.X.memory limit
      && L.get state.X.machine.E.locals base_local === Some (S.I32 base)} ->
    {out : result | X.run (Write.emit writes base_local) state === X.Done {X.memory = out.memory; machine = state.X.machine}
      && Q.decode values out.bytes === Some out.suffix && Bytes.drop out.memory base === Some out.bytes
      && P.equal_prefix base state.X.memory out.memory && V.length out.memory === V.length state.X.memory
      && Bytes.drop state.X.memory (S.add32 base width) === Some out.suffix
      && Bytes.drop out.memory (S.add32 base width) === Some out.suffix && Bounds.covers out.memory limit} @ immutable =
  fun writes values width state base limit base_local premise ->
    ghost_ (zero_def ());
    let stop : B.u32 = base + width in
    let _end = Bounds.suffix state.X.memory limit stop () in
    ghost_ (Bounds.covers_def state.X.memory stop);
    let before = Bounds.suffix state.X.memory stop base () in
    ghost_ (Bounds.distance_def base stop);
    let raw = Raw.read values before width () in
    let bytes = Q.encode values raw.Raw.suffix in
    let memory = Wasm_memory_splice.replace state.X.memory base before bytes () in
    ghost_ (Q.size_def Q.End 0; Q.append_def Q.End raw.Raw.words; Q.append_def Q.End values;
      Words.correct raw.Raw.words values Q.End writes 0 width state.X.machine.E.locals state.X.memory memory base before bytes raw.Raw.suffix ();
      Write.correct writes base_local state base memory ();
      Q.prefix raw.Raw.words before before raw.Raw.suffix raw.Raw.suffix width ();
      Q.prefix values bytes bytes raw.Raw.suffix raw.Raw.suffix width ();
      Wasm_cell.shift state.X.memory base width stop before ();
      Wasm_cell.shift memory base width stop bytes ();
      S.add32_def base width; Bounds.same_length state.X.memory memory limit ());
    {memory; bytes; suffix = raw.Raw.suffix}

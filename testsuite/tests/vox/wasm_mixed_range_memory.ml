module B = Wasm_u32
module V = Hmc_tagged_cell
module Q = Wasm_word_sequence
module Raw = Wasm_words_read
module Split = Wasm_word_shape_split
module Bounds = Hmc_linear_bounds
module Bytes = Hmc_linear_bytes
module P = Hmc_linear_preservation
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module L = Wasm_locals
type result = {memory : B.bytes; bytes : B.bytes; suffix : B.bytes}
let (correct @ total) : (writes : Wasm_mixed_write.writes) @ immutable -> (values : Q.words) @ immutable ->
    (width : B.u32) -> (prefix : Q.words) @ immutable -> (offset : B.u32) -> (state : X.state) @ immutable ->
    (base : B.u32) -> (limit : B.u32) -> (base_local : B.u32) ->
    {u : unit | Wasm_mixed_words.matches writes offset values state.X.machine.E.locals && Q.size values width && Q.size prefix offset
      && offset + width <= 4294967288 && base + offset + width <= limit && Bounds.covers state.X.memory limit
      && L.get state.X.machine.E.locals base_local === Some (S.I32 base)} ->
    {out : result | X.run (Wasm_mixed_write.emit writes base_local) state === X.Done {X.memory = out.memory; machine = state.X.machine}
      && V.length out.memory === V.length state.X.memory && Bounds.covers out.memory limit
      && P.equal_prefix (S.add32 base offset) state.X.memory out.memory
      && Bytes.drop out.memory (S.add32 base offset) === Some out.bytes && Q.decode values out.bytes === Some out.suffix
      && Bytes.drop state.X.memory (S.add32 base (S.add32 offset width)) === Some out.suffix
      && Bytes.drop out.memory (S.add32 base (S.add32 offset width)) === Some out.suffix} @ immutable =
  fun writes values width prefix offset state base limit base_local premise ->
    let extent : B.u32 = offset + width in
    let stop : B.u32 = base + extent in
    let _end = Bounds.suffix state.X.memory limit stop () in
    ghost_ (Bounds.covers_def state.X.memory stop);
    let before = Bounds.suffix state.X.memory stop base () in
    let layout = Q.append prefix values in
    ghost_ (Bounds.distance_def base stop; Wasm_word_sequence_algebra.size prefix values offset width extent ());
    let raw = Raw.read layout before extent () in
    let split = Split.split prefix values raw.Raw.words offset () in
    let after_words = Q.append split.Split.prefix values in
    let frame = Q.encode after_words raw.Raw.suffix in
    let memory = Wasm_memory_splice.replace state.X.memory base before frame () in
    ghost_ (Wasm_mixed_words.correct split.Split.rest values split.Split.prefix writes offset width state.X.machine.E.locals
        state.X.memory memory base before frame raw.Raw.suffix ();
      Wasm_mixed_write.correct writes base_local state base memory ();
      Wasm_word_sequence_algebra.size split.Split.prefix values offset width extent ();
      Q.prefix raw.Raw.words before before raw.Raw.suffix raw.Raw.suffix extent ();
      Q.prefix after_words frame frame raw.Raw.suffix raw.Raw.suffix extent ();
      Wasm_cell.shift state.X.memory base extent stop before (); Wasm_cell.shift memory base extent stop frame ());
    let old_values = Q.split split.Split.prefix split.Split.rest before raw.Raw.suffix () in
    let bytes = Q.split split.Split.prefix values frame raw.Raw.suffix () in
    ghost_ (Q.prefix split.Split.prefix before frame old_values bytes offset ();
      Hmc_linear_prefix_join.correct state.X.memory memory base offset (base + offset) before frame ();
      Wasm_cell.shift memory base offset (base + offset) frame ();
      S.add32_def base offset; S.add32_def offset width; S.add32_def base extent;
      Bounds.same_length state.X.memory memory limit ());
    {memory; bytes; suffix = raw.Raw.suffix}

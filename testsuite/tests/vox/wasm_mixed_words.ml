module B = Wasm_u32
module Q = Wasm_word_sequence
module A = Wasm_word_sequence_algebra
module Shape = Wasm_cross_words
module Write = Wasm_mixed_write
module S = Wasm_scalar
module Bytes = Hmc_linear_bytes
module P = Hmc_linear_preservation
let[@def] rec (matches @ total) (writes : Write.writes @ immutable) (offset : B.u32) (values : Q.words @ immutable) (locals : S.stack @ immutable) = ghost_ (
  match values, writes with
  | Q.End, Write.End -> true
  | Q.Word (word, rest), Write.Write (at, value, more) -> at = offset && offset <= 4294967287
    && Write.read value locals === Some word && matches more (offset + 8) rest locals
  | _ -> false)
let rec (correct @ total) : (old : Q.words) @ immutable -> (values : Q.words) @ immutable -> (prefix : Q.words) @ immutable ->
    (writes : Write.writes) @ immutable -> (offset : B.u32) -> (width : B.u32) -> (locals : S.stack) @ immutable ->
    (before : B.bytes) @ immutable -> (after : B.bytes) @ immutable -> (base : B.u32) ->
    (before_frame : B.bytes) @ immutable -> (after_frame : B.bytes) @ immutable -> (suffix : B.bytes) @ immutable ->
    {u : unit | Shape.shape old values && Q.size values width && Q.size prefix offset
      && offset + width <= 4294967288 && base + offset + width <= 4294967296
      && matches writes offset values locals && P.equal_prefix base before after
      && Bytes.drop before base === Some before_frame && Bytes.drop after base === Some after_frame
      && Q.decode (Q.append prefix old) before_frame === Some suffix
      && Q.decode (Q.append prefix values) after_frame === Some suffix} ->
    {u : unit | Write.apply writes before base locals === Some after} @ ghost =
  fun old values prefix writes offset width locals before after base before_frame after_frame suffix premise -> ghost_ (
    Shape.shape_def old values; Q.size_def values width; matches_def writes offset values locals;
    Write.apply_def writes before base locals;
    match old, values, writes with
    | Q.End, Q.End, Write.End ->
      Q.unique (Q.append prefix Q.End) before_frame after_frame suffix ();
      Hmc_wasm_range_copy.unique before after base before_frame ()
    | Q.Word (old_word, old_rest), Q.Word (word, rest), Write.Write (_, value, more) ->
      let middle_words = Q.append prefix (Q.Word (word, old_rest)) in
      let middle_frame = Q.encode middle_words suffix in
      let middle = Wasm_memory_splice.replace before base before_frame middle_frame () in
      Wasm_sequence_update.at prefix old_rest before middle before_frame middle_frame suffix old_word word base offset (base + offset) ();
      Wasm_memory_splice.shared before middle after base ();
      let one = Q.Word (word, Q.End) in
      let next_prefix = Q.append prefix one in
      Q.size_def one 8; Q.size_def Q.End 0; A.size prefix one offset 8 (offset + 8) ();
      A.associative prefix one old_rest; A.associative prefix one rest;
      Q.append_def one old_rest; Q.append_def Q.End old_rest;
      Q.append_def one rest; Q.append_def Q.End rest;
      correct old_rest rest next_prefix more (offset + 8) (width - 8) locals middle after base middle_frame after_frame suffix ()
    | _ -> ())

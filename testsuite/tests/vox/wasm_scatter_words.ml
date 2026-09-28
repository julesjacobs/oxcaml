module B = Wasm_u32
module Q = Wasm_word_sequence
module A = Wasm_word_sequence_algebra
module P = Hmc_linear_preservation
module Bytes = Hmc_linear_bytes
module M = Wasm_memory
module S = Wasm_scalar
module Plan = Wasm_parallel_copy
module Copy = Wasm_cross_copy
module Splice = Wasm_memory_splice
module Shape = Wasm_cross_words
let[@def] rec (matches @ total) (plan : Plan.plan @ immutable) (target : B.u32)
    (values : Q.words @ immutable) (source : B.bytes @ immutable) (source_base : B.u32) = ghost_ (match values with
  | Q.End -> plan === Plan.End
  | Q.Word (value, rest) -> target <= 4294967287 &&
    (match plan with Plan.Copy (offset, destination, more) -> destination = target &&
      M.load source source_base offset M.W64 === Some (S.I64 value) && matches more (target + 8) rest source source_base
    | _ -> false))
let rec (correct @ total) : (old : Q.words) @ immutable -> (values : Q.words) @ immutable -> (prefix : Q.words) @ immutable ->
    (plan : Plan.plan) @ immutable -> (target_offset : B.u32) ->
    (source : B.bytes) @ immutable -> (before : B.bytes) @ immutable -> (after : B.bytes) @ immutable -> (source_base : B.u32) -> (base : B.u32) ->
    (before_frame : B.bytes) @ immutable -> (after_frame : B.bytes) @ immutable -> (suffix : B.bytes) @ immutable ->
    (width : B.u32) ->
    {u : unit | Shape.shape old values && Q.size values width && Q.size prefix target_offset
      && target_offset + width <= 4294967288 && base + target_offset + width <= 4294967296
      && matches plan target_offset values source source_base
      && source === before
      && P.equal_prefix base before after && Bytes.drop before base === Some before_frame && Bytes.drop after base === Some after_frame
      && Q.decode (Q.append prefix old) before_frame === Some suffix
      && Q.decode (Q.append prefix values) after_frame === Some suffix} ->
    {u : unit | Copy.apply plan source source_base base === Some after} @ ghost =
  fun old values prefix plan target_offset source before after source_base base before_frame after_frame suffix width premise -> ghost_ (
    Shape.shape_def old values; Q.size_def values width; matches_def plan target_offset values source source_base;
    match old, values with
    | Q.End, Q.End ->
      Copy.apply_def Plan.End source source_base base;
      Q.unique (Q.append prefix Q.End) before_frame after_frame suffix ();
      Hmc_wasm_range_copy.unique before after base before_frame ()
    | Q.Word (old_word, old_rest), Q.Word (value, rest) ->
      (match plan with
      | Plan.Copy (_, _, more) ->
        let one = Q.Word (old_word, Q.End) in
        let next_prefix = Q.append prefix one in
        let middle_words = Q.append prefix (Q.Word (old_word, rest)) in
        let middle_frame = Q.encode middle_words suffix in
        let middle = Splice.replace before base before_frame middle_frame () in
        Q.size_def one 8; Q.size_def Q.End 0; A.size prefix one target_offset 8 (target_offset + 8) ();
        A.associative prefix one old_rest; A.associative prefix one rest;
        Q.append_def one old_rest; Q.append_def Q.End old_rest;
        Q.append_def one rest; Q.append_def Q.End rest;
        correct old_rest rest next_prefix more (target_offset + 8)
          source before middle source_base base before_frame middle_frame suffix (width - 8) ();
        Splice.shared before middle after base ();
        Wasm_sequence_update.at prefix rest middle after middle_frame after_frame suffix old_word value base target_offset (base + target_offset) ();
        Copy.apply_def plan source source_base base
      | _ -> ())
    | _ -> ())

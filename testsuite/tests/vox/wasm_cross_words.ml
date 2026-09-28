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
let[@def] rec (shape @ total) (old : Q.words @ immutable) (values : Q.words @ immutable) = ghost_ (match old, values with
  | Q.End, Q.End -> true | Q.Word (_, a), Q.Word (_, b) -> shape a b | _ -> false)
let[@def] rec (matches @ total) (plan : Plan.plan @ immutable) (source : B.u32) (target : B.u32)
    (values : Q.words @ immutable) (tail : Plan.plan @ immutable) = ghost_ (match values with
  | Q.End -> plan === tail
  | Q.Word (_, rest) -> source <= 4294967287 && target <= 4294967287 &&
    (match plan with Plan.Copy (a, b, more) -> a = source && b = target && matches more (source + 8) (target + 8) rest tail | _ -> false))
let[@def] rec (reads @ total) (memory : B.bytes @ immutable) (base : B.u32) (offset : B.u32) (values : Q.words @ immutable) = ghost_ (match values with
  | Q.End -> true
  | Q.Word (value, rest) -> offset <= 4294967287 && M.load memory base offset M.W64 === Some (S.I64 value) && reads memory base (offset + 8) rest)
let rec (correct @ total) : (old : Q.words) @ immutable -> (values : Q.words) @ immutable -> (prefix : Q.words) @ immutable ->
    (plan : Plan.plan) @ immutable -> (tail_plan : Plan.plan) @ immutable -> (source_offset : B.u32) -> (target_offset : B.u32) ->
    (source : B.bytes) @ immutable -> (before : B.bytes) @ immutable -> (after : B.bytes) @ immutable -> (source_base : B.u32) -> (base : B.u32) ->
    (before_frame : B.bytes) @ immutable -> (after_frame : B.bytes) @ immutable -> (suffix : B.bytes) @ immutable ->
    (width : B.u32) ->
    {u : unit | shape old values && Q.size values width && Q.size prefix target_offset
      && target_offset + width <= 4294967288 && base + target_offset + width <= 4294967296
      && matches plan source_offset target_offset values tail_plan && reads source source_base source_offset values
      && Copy.apply tail_plan source source_base base === Some before
      && P.equal_prefix base before after && Bytes.drop before base === Some before_frame && Bytes.drop after base === Some after_frame
      && Q.decode (Q.append prefix old) before_frame === Some suffix
      && Q.decode (Q.append prefix values) after_frame === Some suffix} ->
    {u : unit | Copy.apply plan source source_base base === Some after} @ ghost =
  fun old values prefix plan tail_plan source_offset target_offset source before after source_base base before_frame after_frame suffix width premise -> ghost_ (
    shape_def old values; Q.size_def values width; matches_def plan source_offset target_offset values tail_plan; reads_def source source_base source_offset values;
    match old, values with
    | Q.End, Q.End ->
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
        correct old_rest rest next_prefix more tail_plan (source_offset + 8) (target_offset + 8)
          source before middle source_base base before_frame middle_frame suffix (width - 8) ();
        Splice.shared before middle after base ();
        Wasm_sequence_update.at prefix rest middle after middle_frame after_frame suffix old_word value base target_offset (base + target_offset) ();
        Copy.apply_def plan source source_base base
      | _ -> ())
    | _ -> ())

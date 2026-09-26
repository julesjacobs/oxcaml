module B = Wasm_u32
module Q = Wasm_word_sequence
module Cross = Wasm_cross_words
module Scatter = Wasm_scatter_words
module Plan = Wasm_parallel_copy
let rec (correct @ total) : (plan : Plan.plan) @ immutable -> (tail : Plan.plan) @ immutable ->
    (source_offset : B.u32) -> (target_offset : B.u32) -> (values : Q.words) @ immutable -> (rest : Q.words) @ immutable ->
    (memory : B.bytes) @ immutable -> (base : B.u32) -> (width : B.u32) -> (stop : B.u32) ->
    {u : unit | Cross.matches plan source_offset target_offset values tail
      && Cross.reads memory base source_offset values && Q.size values width && target_offset + width = stop
      && Scatter.matches tail stop rest memory base} ->
    {u : unit | Scatter.matches plan target_offset (Q.append values rest) memory base} @ ghost =
  fun plan tail source_offset target_offset values rest memory base width stop premise -> ghost_ (
    Cross.matches_def plan source_offset target_offset values tail; Cross.reads_def memory base source_offset values;
    Q.size_def values width; Q.append_def values rest; Scatter.matches_def plan target_offset (Q.append values rest) memory base;
    match values, plan with
    | Q.Word (_, remaining), Plan.Copy (_, _, more) ->
      correct more tail (source_offset + 8) (target_offset + 8) remaining rest memory base (width - 8) stop ()
    | _ -> ())
let rec (right_identity @ total) : (words : Q.words) @ immutable ->
    {u : unit | Q.append words Q.End === words} @ ghost = fun words -> ghost_ (
  Q.append_def words Q.End; match words with Q.End -> () | Q.Word (_, rest) -> right_identity rest)

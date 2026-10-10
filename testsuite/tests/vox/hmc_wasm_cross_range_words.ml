module B = Wasm_u32
module D = Hm_declarative
module Heap = Hmc_heap_objects
module V = Hmc_tagged_cell
module Q = Wasm_word_sequence
module Words = Hmc_wire_word_sequence
module Index = Hmc_u32_index
module Lower = Hmc_wasm_relayout
module Range = Hmc_wasm_range_copy
module Plan = Wasm_parallel_copy
module Cross = Wasm_cross_words
let rec (correct @ total) : (values : Heap.cells) @ immutable -> (plan : Plan.plan) @ immutable -> (tail : Plan.plan) @ immutable ->
    (source : Lower.count) -> (target : Lower.count) -> (count : Lower.count) -> (memory : B.bytes) @ immutable -> (base : B.u32) ->
    {u : unit | Index.represents (Heap.length values) count && source + count <= 268435452 && target + count <= 268435452
      && Lower.range_is plan source target (Heap.length values) tail && Range.reads memory base source values} ->
    {u : unit | Cross.matches plan (Range.tag source) (Range.tag target) (Words.words values) tail
      && Cross.reads memory base (Range.tag source) (Words.words values)} @ ghost =
  fun values plan tail source target count memory base premise -> ghost_ (
    Heap.length_def values; Index.represents_def (Heap.length values) count;
    Lower.range_is_def plan source target (Heap.length values) tail; Range.reads_def memory base source values;
    Words.words_def values; Range.tag_def source; Range.tag_def target;
    Cross.matches_def plan (Range.tag source) (Range.tag target) (Words.words values) tail;
    Cross.reads_def memory base (Range.tag source) (Words.words values);
    match values with
    | Heap.Empty -> ()
    | Heap.Cell (value, rest) ->
      (match plan with
      | Plan.Copy (_, _, (Plan.Copy (_, _, more) as payload)) ->
        Range.payload_def source; Range.payload_def target; Range.tag_def (source + 1); Range.tag_def (target + 1);
        Cross.matches_def payload (Range.tag source + 8) (Range.tag target + 8) (Q.Word (V.payload value, Words.words rest)) tail;
        Cross.reads_def memory base (Range.tag source + 8) (Q.Word (V.payload value, Words.words rest));
        correct rest more tail (source + 1) (target + 1) (count - 1) memory base ()
      | _ -> ()))

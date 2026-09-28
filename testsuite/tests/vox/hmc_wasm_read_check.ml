module B = Wasm_u32
module W = Hmc_word64
module V = Hmc_tagged_cell
module H = Hmc_heap_objects
module M = Wasm_memory
module S = Wasm_scalar
module Q = Wasm_word_sequence
module Cross = Wasm_cross_words
module Range = Hmc_wasm_range_copy
module R = Hmc_wasm_relayout
let (word @ total) : (memory : B.bytes) @ immutable -> (base : B.u32) -> (offset : B.u32) -> (value : W.t) @ immutable ->
    {out : bool | out === (M.load memory base offset M.W64 === Some (S.I64 value))} =
  fun memory base offset value -> match M.load memory base offset M.W64 with
  | Some (S.I64 actual) -> ghost_ (W.equal_def actual value); W.equal actual value
  | _ -> false
let rec (words @ total) : (memory : B.bytes) @ immutable -> (base : B.u32) -> (offset : B.u32) -> (values : Q.words) @ immutable ->
    {out : bool | out === Cross.reads memory base offset values} =
  fun memory base offset values -> ghost_ (Cross.reads_def memory base offset values);
  match values with
  | Q.End -> true
  | Q.Word (value, rest) -> offset <= 4294967287 && word memory base offset value && words memory base (offset + 8) rest
let rec (cells @ total) : (memory : B.bytes) @ immutable -> (base : B.u32) -> (position : R.count) -> (values : H.cells) @ immutable ->
    {out : bool | out === Range.reads memory base position values} =
  fun memory base position values -> ghost_ (Range.reads_def memory base position values);
  match values with
  | H.Empty -> true
  | H.Cell (value, rest) -> position < 268435452 && word memory base (Range.tag position) (V.tag value)
      && word memory base (Range.payload position) (V.payload value) && cells memory base (position + 1) rest

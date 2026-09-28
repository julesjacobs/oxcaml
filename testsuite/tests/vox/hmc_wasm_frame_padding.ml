module B = Wasm_u32
module D = Hm_declarative
module H = Hmc_heap_objects
module V = Hmc_tagged_cell
module R = Hmc_wasm_relayout
module Index = Hmc_u32_index
module Write = Wasm_mixed_write
module Words = Wasm_mixed_words
module Wire = Hmc_wire_word_sequence
module Q = Wasm_word_sequence
module S = Wasm_scalar
let[@def] rec (cells @ total) (count : D.index @ immutable) = match count with
  | D.Z -> H.Empty | D.S rest -> H.Cell (V.Nil, cells rest)
let rec (length @ total) : (count : D.index) @ immutable -> {u : unit | H.length (cells count) === count} @ ghost =
  fun count -> ghost_ (cells_def count; H.length_def (cells count); match count with D.Z -> () | D.S rest -> length rest)
let[@def] rec (matches @ total) (writes : Write.writes @ immutable) (position : int) (count : D.index @ immutable) = ghost_ (
  match count, writes with
  | D.Z, Write.End -> true
  | D.S rest, Write.Write (tag, Write.Constant a, Write.Write (payload, Write.Constant b, tail)) ->
    tag = 16 * position && payload = 16 * position + 8 && a === V.tag V.Nil && b === V.payload V.Nil && matches tail (position + 1) rest
  | _ -> false)
let rec (build @ total) : (position : R.count) -> (count : R.count) -> (length : D.index) @ immutable ->
    {u : unit | Index.represents length count && position + count <= 268435452} ->
    {writes : Write.writes | matches writes position length} @ immutable =
  fun position count length premise -> ghost_ (Index.represents_def length count);
    match length with
    | D.Z -> ghost_ (matches_def Write.End position length); Write.End
    | D.S rest ->
      let tail = build (position + 1) (count - 1) rest () in
      let writes = Write.Write (16 * position, Write.Constant (V.tag V.Nil), Write.Write (16 * position + 8, Write.Constant (V.payload V.Nil), tail)) in
      ghost_ (matches_def writes position length); writes
let rec (layout @ total) : (writes : Write.writes) @ immutable -> (position : R.count) -> (count : R.count) ->
    (n : D.index) @ immutable -> (locals : S.stack) @ immutable -> (offset : B.u32) ->
    {u : unit | Index.represents n count && position + count <= 268435452 && matches writes position n && offset = 16 * position} ->
    {u : unit | Words.matches writes offset (Wire.words (cells n)) locals} @ ghost =
  fun writes position count n locals offset premise -> ghost_ (
    Index.represents_def n count; matches_def writes position n; cells_def n;
    Wire.words_def (cells n); Words.matches_def writes offset (Wire.words (cells n)) locals;
    match n, writes with
    | D.S rest, Write.Write (_, Write.Constant a, (Write.Write (_, Write.Constant b, tail) as payload)) ->
      Write.read_def (Write.Constant a) locals; Write.read_def (Write.Constant b) locals;
      Words.matches_def payload (offset + 8) (Q.Word (V.payload V.Nil, Wire.words (cells rest))) locals;
      layout tail (position + 1) (count - 1) rest locals (offset + 16) ()
    | _ -> ())

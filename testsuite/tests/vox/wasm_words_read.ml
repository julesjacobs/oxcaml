module B = Wasm_u32
module Q = Wasm_word_sequence
module Read = Wasm_word_read
module Bounds = Hmc_linear_bounds
module Shape = Wasm_cross_words
type result = {words : Q.words; suffix : B.bytes}
let rec (read @ total) : (layout : Q.words) @ immutable -> (input : B.bytes) @ immutable -> (width : B.u32) ->
    {u : unit | Q.size layout width && Bounds.covers input width} ->
    {out : result | Q.decode out.words input === Some out.suffix && Q.size out.words width && Shape.shape out.words layout} @ immutable =
  fun layout input width premise ->
    ghost_ (Q.size_def layout width);
    match layout with
    | Q.End ->
      ghost_ (Q.decode_def Q.End input; Q.size_def Q.End width; Shape.shape_def Q.End Q.End);
      {words = Q.End; suffix = input}
    | Q.Word (_, rest) ->
      let head = Read.read input width () in
      ghost_ (Read.remaining_def width);
      let tail = read rest head.Read.tail (width - 8) () in
      let words = Q.Word (head.Read.word, tail.words) in
      ghost_ (Q.decode_def words input; Hmc_word64.equal_def head.Read.word head.Read.word;
        Q.size_def words width; Shape.shape_def words layout);
      {words; suffix = tail.suffix}

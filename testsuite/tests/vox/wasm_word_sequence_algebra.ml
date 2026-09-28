module B = Wasm_u32
module Q = Wasm_word_sequence
let rec (associative @ total) : (first : Q.words) @ immutable -> (second : Q.words) @ immutable -> (third : Q.words) @ immutable ->
    {u : unit | Q.append (Q.append first second) third === Q.append first (Q.append second third)} @ ghost =
  fun first second third -> ghost_ (
    Q.append_def first second; Q.append_def (Q.append first second) third; Q.append_def first (Q.append second third);
    match first with Q.End -> () | Q.Word (_, rest) -> associative rest second third)
let rec (size @ total) : (first : Q.words) @ immutable -> (second : Q.words) @ immutable ->
    (a : B.u32) -> (b : B.u32) -> (total : B.u32) ->
    {u : unit | Q.size first a && Q.size second b && total = a + b} ->
    {u : unit | Q.size (Q.append first second) total} @ ghost = fun first second a b total premise -> ghost_ (
    Q.size_def first a; Q.append_def first second; Q.size_def (Q.append first second) total;
    match first with Q.End -> () | Q.Word (_, rest) -> size rest second (a - 8) b (total - 8) ())

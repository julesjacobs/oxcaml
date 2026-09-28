module B = Wasm_u32
module Q = Wasm_word_sequence
module Shape = Wasm_cross_words
type result = {prefix : Q.words; rest : Q.words}
let rec (split @ total) : (prefix : Q.words) @ immutable -> (values : Q.words) @ immutable -> (old : Q.words) @ immutable -> (width : B.u32) ->
    {u : unit | Q.size prefix width && Shape.shape old (Q.append prefix values)} ->
    {out : result | old === Q.append out.prefix out.rest && Q.size out.prefix width && Shape.shape out.rest values} @ immutable =
  fun prefix values old width premise ->
    ghost_ (Q.size_def prefix width; Q.append_def prefix values; Shape.shape_def old (Q.append prefix values));
    match prefix, old with
    | Q.End, _ -> ghost_ (Q.append_def Q.End old; Q.size_def Q.End width); {prefix = Q.End; rest = old}
    | Q.Word (_, more), Q.Word (value, tail) ->
      let out = split more values tail (width - 8) () in
      let head = Q.Word (value, out.prefix) in
      ghost_ (Q.append_def head out.rest; Q.size_def head width);
      {prefix = head; rest = out.rest}
    | _ -> unreachable_ ()

module B = Wasm_u32
module L = Hmc_linear_bytes
module P = Hmc_linear_preservation
let rec (replace @ total) : (memory : B.bytes) @ immutable -> (base : B.u32) -> (old : B.bytes) @ immutable ->
    (suffix : B.bytes) @ immutable -> {u : unit | L.drop memory base === Some old} ->
    {after : B.bytes | P.equal_prefix base memory after && L.drop after base === Some suffix} @ immutable =
  fun memory base old suffix premise ->
    ghost_ (L.drop_def memory base);
    if base = 0 then (ghost_ (P.equal_prefix_def base memory suffix; L.drop_def suffix base); suffix)
    else match memory with
    | B.End -> unreachable_ ()
    | B.Byte (head, rest) ->
      let tail = replace rest (base - 1) old suffix () in
      let after = B.Byte (head, tail) in
      ghost_ (P.equal_prefix_def base memory after; L.drop_def after base); after
let rec (concat @ total) : (before : B.bytes) @ immutable -> (after : B.bytes) @ immutable ->
    (base : B.u32) -> (count : B.u32) -> (total : B.u32) -> (left : B.bytes) @ immutable -> (right : B.bytes) @ immutable ->
    {u : unit | total = base + count && P.equal_prefix base before after
      && L.drop before base === Some left && L.drop after base === Some right && P.equal_prefix count left right} ->
    {u : unit | P.equal_prefix total before after} @ ghost = fun before after base count total left right premise -> ghost_ (
    P.equal_prefix_def base before after; L.drop_def before base; L.drop_def after base;
    if base = 0 then () else (
      P.equal_prefix_def total before after;
      match before, after with
      | B.Byte (_, b), B.Byte (_, a) -> concat b a (base - 1) count (total - 1) left right ()
      | _ -> ()))
let rec (shared @ total) : (source : B.bytes) @ immutable -> (left : B.bytes) @ immutable -> (right : B.bytes) @ immutable ->
    (base : B.u32) -> {u : unit | P.equal_prefix base source left && P.equal_prefix base source right} ->
    {u : unit | P.equal_prefix base left right} @ ghost = fun source left right base premise -> ghost_ (
    P.equal_prefix_def base source left; P.equal_prefix_def base source right; P.equal_prefix_def base left right;
    if base = 0 then () else match source, left, right with
    | B.Byte (_, s), B.Byte (_, a), B.Byte (_, b) -> shared s a b (base - 1) ()
    | _ -> ())

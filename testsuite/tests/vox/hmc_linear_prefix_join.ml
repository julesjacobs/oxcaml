module B = Wasm_u32
module L = Hmc_linear_bytes
module P = Hmc_linear_preservation
let rec (correct @ total) : (before : B.bytes) @ immutable -> (after : B.bytes) @ immutable -> (base : B.u32) -> (count : B.u32) -> (stop : B.u32) ->
    (left : B.bytes) @ immutable -> (right : B.bytes) @ immutable ->
    {u : unit | stop = base + count && P.equal_prefix base before after
      && L.drop before base === Some left && L.drop after base === Some right && P.equal_prefix count left right} ->
    {u : unit | P.equal_prefix stop before after} @ ghost =
  fun before after base count stop left right premise -> ghost_ (
    L.drop_def before base; L.drop_def after base;
    if base = 0 then () else (
      P.equal_prefix_def base before after; P.equal_prefix_def stop before after;
      match before, after with
      | B.Byte (_, a), B.Byte (_, b) -> correct a b (base - 1) count (stop - 1) left right ()
      | _ -> ()))

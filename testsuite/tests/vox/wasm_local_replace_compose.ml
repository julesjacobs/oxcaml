module B = Wasm_u32
module S = Wasm_scalar
module L = Wasm_locals
let rec (compose @ total) : (before : S.stack) @ immutable -> (middle : S.stack) @ immutable -> (after : S.stack) @ immutable ->
    (index : B.u32) -> (first : S.value) @ immutable -> (last : S.value) @ immutable ->
    {u : unit | L.replaced before index first middle && L.replaced middle index last after} ->
    {u : unit | L.replaced before index last after} @ ghost =
  fun before middle after index first last premise -> ghost_ (
    L.replaced_def before index first middle; L.replaced_def middle index last after; L.replaced_def before index last after;
    match before, middle, after with
    | S.Push (_, a), S.Push (_, b), S.Push (_, c) -> if index = 0 then () else compose a b c (index - 1) first last ()
    | _ -> ())

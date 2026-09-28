module B = Wasm_u32
module S = Wasm_scalar
module L = Wasm_locals
let rec (replaced @ total) : (before : S.stack) @ immutable -> (index : B.u32) -> (value : S.value) @ immutable ->
    (left : S.stack) @ immutable -> (right : S.stack) @ immutable ->
    {u : unit | L.replaced before index value left && L.replaced before index value right} ->
    {u : unit | left === right} @ ghost = fun before index value left right premise -> ghost_ (
  L.replaced_def before index value left; L.replaced_def before index value right;
  match before, left, right with
  | S.Push (_, rest), S.Push (_, a), S.Push (_, b) ->
    if index = 0 then () else replaced rest (index - 1) value a b ()
  | _ -> ())

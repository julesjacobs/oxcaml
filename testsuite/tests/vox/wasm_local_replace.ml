module B = Wasm_u32
module S = Wasm_scalar
module L = Wasm_locals
let[@def] rec (replace @ total) (locals : S.stack @ immutable) (index : B.u32) (value : S.value @ immutable) =
  match locals with
  | S.Empty -> S.Empty
  | S.Push (old, rest) -> if index = 0 then S.Push (value, rest) else S.Push (old, replace rest (index - 1) value)
let rec (unique @ total) : (before : S.stack) @ immutable -> (index : B.u32) -> (value : S.value) @ immutable -> (after : S.stack) @ immutable ->
    {u : unit | L.replaced before index value after} ->
    {u : unit | after === replace before index value} @ ghost = fun before index value after premise -> ghost_ (
  L.replaced_def before index value after; replace_def before index value;
  match before, after with
  | S.Push (_, rest), S.Push (_, tail) -> if index = 0 then () else unique rest (index - 1) value tail ()
  | _ -> ())
let (correct @ total) : (before : S.stack) @ immutable -> (index : B.u32) -> (value : S.value) @ immutable ->
    {u : unit | L.can_set before index value} ->
    {u : unit | L.set before index value === Some (replace before index value)} @ ghost = fun before index value premise -> ghost_ (
  match L.set before index value with None -> () | Some after -> unique before index value after ())

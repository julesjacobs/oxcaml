module B = Wasm_u32
module S = Wasm_scalar
module L = Wasm_locals

type permissions = Empty | Global of bool * permissions [@@inductive]
type t = {values : S.stack; permissions : permissions}
let[@def] rec (writable @ total) (permissions : permissions @ immutable) (index : B.u32) =
  match permissions with
  | Empty -> false
  | Global (mutable_, rest) -> if index = 0 then mutable_ else writable rest (index - 1)
let[@def] (get @ total) (globals : t @ immutable) (index : B.u32) = L.get globals.values index
let[@def] (can_set @ total) (globals : t @ immutable) (index : B.u32) (value : S.value @ immutable) =
  writable globals.permissions index && L.can_set globals.values index value
let (set @ total) : (globals : t) @ immutable -> (index : B.u32) -> (value : S.value) @ immutable ->
    {out : t option | match out with
      | None -> not (can_set globals index value)
      | Some after -> can_set globals index value && get after index === Some value
        && after.permissions === globals.permissions && L.same_types globals.values after.values
        && L.replaced globals.values index value after.values} @ immutable = fun globals index value ->
  ghost_ (can_set_def globals index value);
  if writable globals.permissions index then
    match L.set globals.values index value with
    | None -> None
    | Some values ->
      let after = {values; permissions = globals.permissions} in
      ghost_ (get_def after index); Some after
  else None
let (other_global @ total) : (before : t) @ immutable -> (index : B.u32) -> (value : S.value) @ immutable ->
    (after : t) @ immutable -> (other : B.u32) ->
    {u : unit | L.replaced before.values index value after.values && index <> other} ->
    {u : unit | get before other === get after other} @ ghost = fun before index value after other premise -> ghost_ (
  L.other_local before.values index value after.values other ();
  get_def before other; get_def after other)

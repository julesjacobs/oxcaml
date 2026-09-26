module B = Wasm_u32
module I = Wasm_instruction
module S = Wasm_scalar

let[@def] rec (get @ total) (locals : S.stack @ immutable) (index : B.u32) =
  match locals with
  | S.Empty -> None
  | S.Push (value, rest) -> if index = 0 then Some value else get rest (index - 1)
let[@def] rec (same_types @ total) (before : S.stack @ immutable) (after : S.stack @ immutable) =
  match before, after with
  | S.Empty, S.Empty -> true
  | S.Push (a, rest), S.Push (b, tail) -> S.same_type a b && same_types rest tail
  | _ -> false
let rec (types_reflexive @ total) : (locals : S.stack) @ immutable ->
    {u : unit | same_types locals locals} @ ghost = fun locals -> ghost_ (
  same_types_def locals locals;
  match locals with
  | S.Empty -> ()
  | S.Push (value, rest) -> S.same_type_def value value; types_reflexive rest)
let[@def] rec (replaced @ total) (before : S.stack @ immutable) (index : B.u32)
    (value : S.value @ immutable) (after : S.stack @ immutable) = ghost_ (
  match before, after with
  | S.Push (old, rest), S.Push (new_, tail) ->
    if index = 0 then new_ === value && rest === tail
    else old === new_ && replaced rest (index - 1) value tail
  | _ -> false)
let[@def] (can_set @ total) (locals : S.stack @ immutable) (index : B.u32)
    (value : S.value @ immutable) =
  match get locals index with None -> false | Some old -> S.same_type old value
let rec (set @ total) : (locals : S.stack) @ immutable -> (index : B.u32) ->
    (value : S.value) @ immutable ->
    {out : S.stack option | match out with
      | None -> not (can_set locals index value)
      | Some updated -> can_set locals index value && get updated index === Some value
        && same_types locals updated && replaced locals index value updated} @ immutable =
  fun locals index value ->
    ghost_ (can_set_def locals index value; get_def locals index);
    match locals with
    | S.Empty -> None
    | S.Push (old, rest) ->
      if index = 0 then
        if S.same_type old value then (
          let updated = S.Push (value, rest) in
          ghost_ (get_def updated index; same_types_def locals updated;
            types_reflexive rest; replaced_def locals index value updated);
          Some updated)
        else None
      else (match set rest (index - 1) value with
        | None -> ghost_ (can_set_def rest (index - 1) value); None
        | Some tail ->
          let updated = S.Push (old, tail) in
          ghost_ (can_set_def rest (index - 1) value; get_def updated index;
            same_types_def locals updated; S.same_type_def old old;
            replaced_def locals index value updated);
          Some updated)

let rec (other_local @ total) : (before : S.stack) @ immutable -> (index : B.u32) ->
    (value : S.value) @ immutable -> (after : S.stack) @ immutable -> (other : B.u32) ->
    {u : unit | replaced before index value after && index <> other} ->
    {u : unit | get before other === get after other} @ ghost =
  fun before index value after other premise -> ghost_ (
    replaced_def before index value after; get_def before other; get_def after other;
    match before, after with
    | S.Push (_, rest), S.Push (_, tail) ->
      if index <> 0 && other <> 0 then other_local rest (index - 1) value tail (other - 1) () else ()
    | _ -> ())

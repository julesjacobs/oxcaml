open Copy_spec
open Level_unifier_spec
open Level_finite_spec

let[@def] rec (substitute @ total)
    (delta : (node Pref.t @ immutable total -> ty @ immutable total) @ total)
    (t : ty @ immutable) = match t with
  | Variable p -> delta p
  | Boolean -> Boolean
  | Function (a, b) -> Function (substitute delta a, substitute delta b)

let[@def] (normalizes @ total) (h : node Pref.heap @ immutable)
    (sigma : (node Pref.t @ immutable total -> ty @ immutable total) @ total)
    (x : node Pref.t @ immutable) (t : tree @ immutable) = ghost_ (
  if H.mem h x then sigma x === readback t else sigma x === Variable x)

open Unifier_spec
open Unifier_finite_spec

let[@def] rec (substitute @ total)
    (delta : (node Pref.t @ immutable total -> ty @ immutable total) @ total)
    (t : ty @ immutable) = match t with
  | TVar p -> delta p
  | TBool -> TBool
  | TArrow (a, b) -> TArrow (substitute delta a, substitute delta b)

let[@def] (normalizes @ total) (h : Pref.heap @ immutable)
    (sigma : (node Pref.t @ immutable total -> ty @ immutable total) @ total)
    (x : node Pref.t @ immutable) (t : tree @ immutable) = ghost_ (
  if H.mem h x then sigma x === readback t else sigma x === TVar x)

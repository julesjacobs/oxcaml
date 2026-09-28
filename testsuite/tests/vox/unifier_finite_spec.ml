open Unifier_spec

type tree =
  | Free of node Pref.t
  | Boolean of node Pref.t
  | Branch of node Pref.t * tree * tree
  | Alias of node Pref.t * tree
  [@@inductive]

let[@def] (root @ total) (tree : tree @ immutable) = match tree with
  | Free p | Boolean p | Branch (p, _, _) | Alias (p, _) -> p

let[@def] rec (finite @ total) (h : node Pref.heap @ immutable)
    (tree : tree @ immutable) = ghost_ (
  H.mem h (root tree) && match tree with
  | Free p -> H.at h p === Some Var
  | Boolean p -> H.at h p === Some Bool
  | Branch (p, a, b) -> H.at h p === Some (Arrow (root a, root b))
    && finite h a && finite h b
  | Alias (p, q) -> H.at h p === Some (Link (root q)) && finite h q)

let[@def] rec (size @ total) (tree : tree @ immutable) = match tree with
  | Free _ | Boolean _ -> Bigint.one
  | Alias (_, child) -> Bigint.add Bigint.one (size child)
  | Branch (_, a, b) -> Bigint.add Bigint.one (Bigint.add (size a) (size b))

let[@def] (edge @ total) (h : node Pref.heap @ immutable)
    (p : node Pref.t @ immutable) (q : node Pref.t @ immutable) = ghost_ (
  match H.at h p with
  | Some (Link x) -> q === x
  | Some (Arrow (a, b)) -> q === a || q === b
  | _ -> false)

type walk = Stop | Step of node Pref.t * walk [@@inductive]

let[@def] rec (walks @ total) (h : node Pref.heap @ immutable)
    (p : node Pref.t @ immutable) (q : node Pref.t @ immutable)
    (w : walk @ immutable) = ghost_ (match w with
  | Stop -> p === q
  | Step (next, rest) -> edge h p next && walks h next q rest)

let[@def] rec (readback @ total) (tree : tree @ immutable) = match tree with
  | Free p -> TVar p
  | Boolean _ -> TBool
  | Alias (_, child) -> readback child
  | Branch (_, a, b) -> TArrow (readback a, readback b)

let[@def] (allocatable @ total) (h : node Pref.heap @ immutable)
    (v : node @ immutable) = ghost_ (match v with
  | Var | Bool -> true
  | Link q -> H.mem h q
  | Arrow (a, b) -> H.mem h a && H.mem h b)

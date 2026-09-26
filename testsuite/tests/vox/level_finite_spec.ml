open Copy_spec
open Level_unifier_spec

type tree =
  | Free of node Pref.t
  | Constant_tree of node Pref.t
  | Word_tree of node Pref.t
  | List_tree of node Pref.t * tree
  | Branch of node Pref.t * tree * tree
  | Alias_tree of node Pref.t * tree
  [@@inductive]

let[@def] (tree_root @ total) (tree : tree @ immutable) = match tree with
  | Free p | Constant_tree p | Word_tree p | List_tree (p, _) | Branch (p, _, _) | Alias_tree (p, _) -> p

let[@def] rec (finite @ total) (h : node Pref.heap @ immutable)
    (tree : tree @ immutable) = ghost_ (
  H.mem h (tree_root tree) && match tree with
  | Free p -> observe h p === Some Var
  | Constant_tree p -> observe h p === Some Bool
  | Word_tree p -> observe h p === Some Word
  | List_tree (p, a) -> observe h p === Some (List (tree_root a)) && finite h a
  | Branch (p, a, b) -> observe h p === Some (Arrow (tree_root a, tree_root b))
    && finite h a && finite h b
  | Alias_tree (p, q) -> observe h p === Some (Link (tree_root q)) && finite h q)

let[@def] rec (size @ total) (tree : tree @ immutable) = match tree with
  | Free _ | Constant_tree _ | Word_tree _ -> Bigint.one
  | Alias_tree (_, child) | List_tree (_, child) -> Bigint.add Bigint.one (size child)
  | Branch (_, a, b) -> Bigint.add Bigint.one (Bigint.add (size a) (size b))

let[@def] (edge @ total) (h : node Pref.heap @ immutable)
    (p : node Pref.t @ immutable) (q : node Pref.t @ immutable) = ghost_ (
  match observe h p with
  | Some (Link x | List x) -> q === x
  | Some (Arrow (a, b)) -> q === a || q === b
  | _ -> false)

type walk = Stop | Step of node Pref.t * walk [@@inductive]

let[@def] rec (walks @ total) (h : node Pref.heap @ immutable)
    (p : node Pref.t @ immutable) (q : node Pref.t @ immutable)
    (w : walk @ immutable) = ghost_ (match w with
  | Stop -> p === q
  | Step (next, rest) -> edge h p next && walks h next q rest)

let[@def] rec (readback @ total) (tree : tree @ immutable) = match tree with
  | Free p -> Variable p
  | Constant_tree _ -> Boolean
  | Word_tree _ -> Word64
  | List_tree (_, child) -> List_type (readback child)
  | Alias_tree (_, child) -> readback child
  | Branch (_, a, b) -> Function (readback a, readback b)

let[@def] (allocatable @ total) (h : node Pref.heap @ immutable)
    (v : node @ immutable) = ghost_ (match v.desc with
  | Var | Bool | Word -> true
  | Link q | List q -> H.mem h q
  | Arrow (a, b) -> H.mem h a && H.mem h b)

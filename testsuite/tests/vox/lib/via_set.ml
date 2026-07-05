type iset [@@vox.sort lean "ISet"]
type tree = Leaf | Node of tree * int * tree
type t = tree{ bst _ } [@vox.via (elems : iset)]

[%%vox.lean {lean|
inductive ISet where
  | nil : ISet
  | cons : Int -> ISet -> ISet

@[grind] def mem (x : Int) : ISet -> Prop
  | .nil => False
  | .cons y s => x = y ∨ mem x s

@[grind] def ins (x : Int) (s : ISet) : ISet := ISet.cons x s

@[grind] def bst : Vox_Via_set_tree -> Prop
  | .Leaf => True
  | .Node l _ _ => bst l

@[grind] def elems : Vox_Via_set_tree -> ISet
  | .Leaf => .nil
  | .Node l v _ => .cons v (elems l)

@[grind] def tmem (x : Int) : Vox_Via_set_tree -> Bool
  | .Leaf => false
  | .Node l v _ => if x = v then true else tmem x l

@[grind] theorem tmem_elems (x : Int) (u : Vox_Via_set_tree) :
    (tmem x u = true) = mem x (elems u) := by
  induction u <;> grind
grind_pattern tmem_elems => mem x (elems u)
|lean}]

let add : (x : int) -> (s : t) -> t{ _ = ins x s } =
  fun x s ->
    let refine_ t0 = s in
    (Node (t0, x, Leaf) : t{ _ = ins x s })

let member : (x : int) -> (s : t) -> bool{ _ = mem x s } =
  fun x s ->
    let refine_ t0 = s in
    let rec go : (u : tree) -> bool{ _ = mem x (elems u) } =
      fun u ->
        match u with
        | Leaf -> false
        | Node (l, v, _) -> if x = v then true else go l
    in
    go t0

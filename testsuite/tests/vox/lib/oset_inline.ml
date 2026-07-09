type myt [@@vox.sort lean "MyT"]
type tree = Leaf | Node of tree * int * tree
type t = tree{ bst _ } [@vox.via (elems : myt)]

[%%vox.lean {lean|
inductive MyT where
  | nil : MyT
  | cons : Int -> MyT -> MyT
@[grind] def mem (x : Int) : MyT -> Prop
  | .nil => False
  | .cons y s => x = y ∨ mem x s
@[grind] def ins (x : Int) (s : MyT) : MyT := MyT.cons x s
@[grind] def empty_s : MyT := MyT.nil
@[grind] def bst : Vox_Oset_inline_tree -> Prop
  | .Leaf => True
  | .Node l _ _ => bst l
@[grind] def elems : Vox_Oset_inline_tree -> MyT
  | .Leaf => .nil
  | .Node l v _ => .cons v (elems l)
theorem mem_empty (x : Int) : ¬ mem x empty_s := by grind
theorem mem_ins (x y : Int) (s : MyT) :
    mem y (ins x s) ↔ (y = x ∨ mem y s) := by grind
|lean}]

let empty : unit -> t{ _ = empty_s } = fun () -> (Leaf : t{ _ = empty_s })
let add : (x : int) -> (s : t) -> t{ _ = ins x s } =
  fun x s -> let refine_ t0 = s in (Node (t0, x, Leaf) : t{ _ = ins x s })
let member : (x : int) -> (s : t) -> bool{ _ = mem x s } =
  fun x s ->
    let refine_ t0 = s in
    let rec go : (u : tree) -> bool{ _ = mem x (elems u) } =
      fun u -> match u with
        | Leaf -> false
        | Node (l, v, _) -> if x = v then true else go l
    in go t0

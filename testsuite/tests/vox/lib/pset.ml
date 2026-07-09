type 'a iset [@@vox.sort lean "ISet"]
type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree
type 'a t = 'a tree{ bst _ } [@vox.via (elems : 'a iset)]

[%%vox.lean {lean|
inductive ISet (a : Type) where
  | nil : ISet a
  | cons : a -> ISet a -> ISet a

@[grind] def mem {a : Type} (x : a) : ISet a -> Prop
  | .nil => False
  | .cons y s => x = y ∨ mem x s

@[grind] def ins {a : Type} (x : a) (s : ISet a) : ISet a := ISet.cons x s

@[grind] def bst {a : Type} : Vox_Pset_tree a -> Prop
  | .Leaf => True
  | .Node l _ _ => bst l

@[grind] def elems {a : Type} : Vox_Pset_tree a -> ISet a
  | .Leaf => .nil
  | .Node l v _ => .cons v (elems l)
|lean}]

(* Honest proof at the abstract element sort: unpack the image binder to
   its tree ([bst t0], link [elems t0 = s]), rebuild, and the image-vocab
   contract [elems (Node ..) = ins x s] discharges via the link + defs --
   no [DecidableEq], so it proves generically. *)
let add : (x : 'a) -> (s : 'a t) -> 'a t{ _ = ins x s } =
  fun x s ->
    let refine_ t0 = s in
    (Node (t0, x, Leaf) : 'a t{ _ = ins x s })

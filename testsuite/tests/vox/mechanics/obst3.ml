type iset [@@vox.sort lean "ISet"]

[%%vox.lean {lean|
inductive ISet where
  | nil : ISet
  | cons : Int -> ISet -> ISet
@[grind] def mem_s (x : Int) : ISet -> Prop
  | .nil => False
  | .cons y s => x = y ∨ mem_s x s
@[grind] def ins (x : Int) (s : ISet) : ISet := ISet.cons x s

@[grind] def bst : Vox_Obst3_tree -> Prop
  | .Leaf => True
  | .Node l _ _ => bst l
@[grind] def elems : Vox_Obst3_tree -> ISet
  | .Leaf => .nil
  | .Node l v _ => .cons v (elems l)
|lean}]

module type ORD = sig
  type t [@@vox.sort int]
  val compare : (x : t) -> (y : t) -> int{ _ >= (-1) && _ <= 1 }
end

module type S = sig
  type elt
  type t : value refines (iset)
  val add : (x : elt) -> (s : t) -> t{ _ = ins x s }
end

module Make (Ord : ORD) : S with type elt = Ord.t = struct
  type elt = Ord.t
  type tree = Leaf | Node of tree * Ord.t * tree
  type t = tree{ bst _ } [@vox.via (elems : iset)]
  let add : (x : Ord.t) -> (s : t) -> t{ _ = ins x s } =
    fun x s ->
      let refine_ t0 = s in
      (Node (t0, x, Leaf) : t{ _ = ins x s })
end

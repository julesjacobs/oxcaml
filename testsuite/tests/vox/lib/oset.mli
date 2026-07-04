(* A set behind a FULLY ABSTRACT sealed interface: the representation
   type is hidden ([@@vox.sort opaque] gives it its own uninterpreted
   sort, Vox_Oset_t, rather than the shared VoxU), the model constants
   are axioms, and the laws are obligations the implementation pays at
   its seal.  Clients reason from this text alone; nothing about the
   representation -- not even that it is a tree -- ever leaves the
   unit. *)

type t [@@vox.sort opaque]

[%%vox.lean {lean|
public axiom mem : Int -> Vox_Oset_t -> Prop
public axiom bst : Vox_Oset_t -> Prop
public axiom no_mem : Vox_Oset_t -> Prop
public axiom insert : Int -> Vox_Oset_t -> Vox_Oset_t

public axiom no_mem_spec (x : Int) (t : Vox_Oset_t)
    (h : no_mem t) : ¬ mem x t
grind_pattern no_mem_spec => mem x t, no_mem t

public axiom bst_insert (x : Int) (t : Vox_Oset_t)
    (h : bst t) : bst (insert x t)
grind_pattern bst_insert => bst (insert x t)

public axiom mem_insert (x y : Int) (t : Vox_Oset_t) :
    mem y (insert x t) ↔ (y = x ∨ mem y t)
grind_pattern mem_insert => mem y (insert x t)
|lean}]

type set = t{ bst _ }

val empty : set{ no_mem _ }
val member : (x : int) -> (t : set) -> bool{ _ = mem x t }
val insert : (x : int) -> (t : set) -> set{ _ = insert x t }

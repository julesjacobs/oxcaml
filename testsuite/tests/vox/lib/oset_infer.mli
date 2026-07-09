(* Fully abstract set, but WITHOUT [@@vox.sort opaque]: the interface
   block mentions this type's minted solver name [Vox_Oset_infer_t], so
   the sort is INFERRED opaque -- the attribute line is unnecessary. *)

type t

[%%vox.lean {lean|
public axiom mem : Int -> Vox_Oset_infer_t -> Prop
public axiom bst : Vox_Oset_infer_t -> Prop
public axiom no_mem : Vox_Oset_infer_t -> Prop
public axiom insert : Int -> Vox_Oset_infer_t -> Vox_Oset_infer_t

public axiom no_mem_spec (x : Int) (t : Vox_Oset_infer_t)
    (h : no_mem t) : ¬ mem x t
grind_pattern no_mem_spec => mem x t, no_mem t

public axiom bst_insert (x : Int) (t : Vox_Oset_infer_t)
    (h : bst t) : bst (insert x t)
grind_pattern bst_insert => bst (insert x t)

public axiom mem_insert (x y : Int) (t : Vox_Oset_infer_t) :
    mem y (insert x t) ↔ (y = x ∨ mem y t)
grind_pattern mem_insert => mem y (insert x t)
|lean}]

type set = t{ bst _ }

val empty : set{ no_mem _ }
val member : (x : int) -> (t : set) -> bool{ _ = mem x t }
val insert : (x : int) -> (t : set) -> set{ _ = insert x t }

(* A binary search tree behind a SPECCED interface.  The block below
   defines the logical MODEL -- membership over the whole structure,
   the ordering invariant, and a model-level [insert] -- and proves
   its theorems once, by induction; both the definitions and the
   theorems travel to the implementation and to every client through
   this .cmi.  The operations' refinements tie the code to the model:
   nothing is assumed anywhere. *)

type tree =
  | Leaf
  | Node of tree * int * tree

[%%vox.lean {lean|
@[grind, expose] public def mem : Int -> Vox_Bst_tree -> Prop
  | _, .Leaf => False
  | x, .Node l v r => x = v ∨ mem x l ∨ mem x r

@[grind, expose] public def all_lt : Vox_Bst_tree -> Int -> Prop
  | .Leaf, _ => True
  | .Node l v r, b => v < b ∧ all_lt l b ∧ all_lt r b

@[grind, expose] public def all_gt : Vox_Bst_tree -> Int -> Prop
  | .Leaf, _ => True
  | .Node l v r, b => v > b ∧ all_gt l b ∧ all_gt r b

@[grind, expose] public def bst : Vox_Bst_tree -> Prop
  | .Leaf => True
  | .Node l v r => all_lt l v ∧ all_gt r v ∧ bst l ∧ bst r

@[grind, expose] public def insert : Int -> Vox_Bst_tree -> Vox_Bst_tree
  | x, .Leaf => .Node .Leaf x .Leaf
  | x, .Node l v r =>
      if x = v then .Node l v r
      else if x < v then .Node (insert x l) v r
      else .Node l v (insert x r)

-- Client-facing laws (obligations discharged in the .ml by induction).
-- The ordering / insertion scaffolding that proves them lives privately
-- in the .ml block.
public axiom bst_insert (x : Int) (t : Vox_Bst_tree)
    (h : bst t) : bst (insert x t)
grind_pattern bst_insert => bst (insert x t)

public axiom mem_insert (x y : Int) (t : Vox_Bst_tree) :
    mem y (insert x t) ↔ (y = x ∨ mem y t)
grind_pattern mem_insert => mem y (insert x t)
|lean}]

(* The API type is the refined abbreviation itself: a set IS a tree
   satisfying the ordering invariant.  Parameters of type [set] are
   contracts through the abbreviation like any other. *)
type set = tree{ bst _ }

val empty : set{ _ = Leaf }

(* Efficient one-path search, proved equal to the model membership
   (which quantifies the WHOLE tree): the ordering lemmas bridge the
   path the code takes to the subtrees it skips. *)
val member : (x : int) -> (t : set) -> bool{ _ = mem x t }

(* Insertion returns exactly the model's insert; the [set] layer and
   the equation conjoin (a refinement over a refined abbreviation
   FLATTENS onto the underlying skeleton), and [mem x _] plus the full
   characterization [mem y (insert x t) <-> y = x || mem y t] follow
   from the exported theorems. *)
val insert : (x : int) -> (t : set) -> set{ _ = insert x t && mem x _ }

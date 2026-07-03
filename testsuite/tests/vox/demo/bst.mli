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
@[grind] def mem : Int -> Vox_Bst_tree -> Prop
  | _, .Leaf => False
  | x, .Node l v r => x = v ∨ mem x l ∨ mem x r

@[grind] def all_lt : Vox_Bst_tree -> Int -> Prop
  | .Leaf, _ => True
  | .Node l v r, b => v < b ∧ all_lt l b ∧ all_lt r b

@[grind] def all_gt : Vox_Bst_tree -> Int -> Prop
  | .Leaf, _ => True
  | .Node l v r, b => v > b ∧ all_gt l b ∧ all_gt r b

@[grind] def bst : Vox_Bst_tree -> Prop
  | .Leaf => True
  | .Node l v r => all_lt l v ∧ all_gt r v ∧ bst l ∧ bst r

@[grind] def insert : Int -> Vox_Bst_tree -> Vox_Bst_tree
  | x, .Leaf => .Node .Leaf x .Leaf
  | x, .Node l v r =>
      if x = v then .Node l v r
      else if x < v then .Node (insert x l) v r
      else .Node l v (insert x r)

-- The ordering invariant makes one-path search complete: an element
-- bounded away from a subtree is not in it.
theorem not_mem_lt (x b : Int) (t : Vox_Bst_tree)
    (h : all_lt t b) (hx : b <= x) : ¬ mem x t := by
  induction t <;> grind
grind_pattern not_mem_lt => mem x t, all_lt t b

theorem not_mem_gt (x b : Int) (t : Vox_Bst_tree)
    (h : all_gt t b) (hx : x <= b) : ¬ mem x t := by
  induction t <;> grind
grind_pattern not_mem_gt => mem x t, all_gt t b

theorem all_lt_insert (x b : Int) (t : Vox_Bst_tree)
    (h : all_lt t b) (hx : x < b) : all_lt (insert x t) b := by
  induction t <;> grind
grind_pattern all_lt_insert => all_lt (insert x t) b

theorem all_gt_insert (x b : Int) (t : Vox_Bst_tree)
    (h : all_gt t b) (hx : b < x) : all_gt (insert x t) b := by
  induction t <;> grind
grind_pattern all_gt_insert => all_gt (insert x t) b

theorem bst_insert (x : Int) (t : Vox_Bst_tree)
    (h : bst t) : bst (insert x t) := by
  induction t <;> grind
grind_pattern bst_insert => bst (insert x t)

theorem mem_insert (x y : Int) (t : Vox_Bst_tree) :
    mem y (insert x t) ↔ (y = x ∨ mem y t) := by
  induction t <;> grind
grind_pattern mem_insert => mem y (insert x t)
|lean}]

val empty : tree{ _ = Leaf }

(* Efficient one-path search, proved equal to the model membership
   (which quantifies the WHOLE tree): the ordering lemmas bridge the
   path the code takes to the subtrees it skips. *)
val member : (x : int) -> (t : tree{ bst _ }) -> bool{ _ = mem x t }

(* Insertion returns exactly the model's insert; the interface-level
   facts [bst _] and [mem x _] follow from the exported theorems, as
   does -- at any client use -- the full characterization
   [mem y (insert x t) <-> y = x || mem y t]. *)
val insert :
  (x : int) -> (t : tree{ bst _ }) -> tree{ _ = insert x t && bst _ && mem x _ }

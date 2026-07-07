(* Implementation of bst.mli, checked against its own interface's
   model (the .cmi carries the definitions and theorems; there is no
   local prelude and no assumption).  Every match arm carries its own
   obligation: [member]'s arms prove that inspecting ONE path decides
   membership in the WHOLE tree, and [insert]'s arms prove the code
   builds exactly the model's tree, from which its interface facts
   follow. *)

type tree =
  | Leaf
  | Node of tree * int * tree

[%%vox.lean {lean|
@[grind, expose] def mem : Int -> Vox_Bst_tree -> Prop
  | _, .Leaf => False
  | x, .Node l v r => x = v ∨ mem x l ∨ mem x r

@[grind, expose] def all_lt : Vox_Bst_tree -> Int -> Prop
  | .Leaf, _ => True
  | .Node l v r, b => v < b ∧ all_lt l b ∧ all_lt r b

@[grind, expose] def all_gt : Vox_Bst_tree -> Int -> Prop
  | .Leaf, _ => True
  | .Node l v r, b => v > b ∧ all_gt l b ∧ all_gt r b

@[grind, expose] def bst : Vox_Bst_tree -> Prop
  | .Leaf => True
  | .Node l v r => all_lt l v ∧ all_gt r v ∧ bst l ∧ bst r

@[grind, expose] def insert : Int -> Vox_Bst_tree -> Vox_Bst_tree
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

type set = tree{ bst _ }

let empty : set{ _ = Leaf } = Leaf

let rec member (x : int) (t : set) : bool{ _ = mem x t } =
  match t with
  | Leaf -> false
  | Node (l, v, r) ->
    if x = v then true
    else if x < v then member x l
    else member x r

let rec insert (x : int) (t : set) : set{ _ = insert x t && mem x _ }
  =
  match t with
  | Leaf -> Node (Leaf, x, Leaf)
  | Node (l, v, r) ->
    if x = v then t
    else if x < v then begin
      let l' = insert x l in
      Node (l', v, r)
    end
    else begin
      let r' = insert x r in
      Node (l, v, r')
    end

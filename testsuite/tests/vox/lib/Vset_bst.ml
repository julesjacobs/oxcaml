(* Implementation of Vset_bst.mli, checked against its own interface's model
   (the .cmi carries the public defs + axioms; there is no local prelude and
   no assumption).  The .ml block RESTATES the model defs without [public]
   (the model-duplication tax, blueprint §4), adds the PRIVATE scaffolding,
   and discharges each .mli axiom with a same-named [theorem].  Zero trust.

   Scaffolding (all private, no .mli twin):
   - bnot_mem_lt/gt: an element bounded away from a subtree is not in it --
     makes one-path [member] complete.
   - ball_lt/gt_insert: [bins] preserves the ordering bounds.
   - ball_lt/gt_mono: a proven bound weakens to a looser one (needed to move
     [r]'s lower bound down to the pivot in the join case).
   - bmem_join / ball_lt/gt_join / bok_join: [bjoin] (used by [bdel] at the
     two-child pivot) preserves membership-union and, given both subtrees are
     ok and separated by a pivot [b], preserves [bok].  bok_join carries a
     3-part grind trigger so its pivot [b] is bound by the ball_lt/ball_gt
     facts in context (a bare [bok (bjoin l r)] trigger leaves [b] free).
   - ball_lt/gt_delete: [bdel] preserves the ordering bounds (it only removes
     elements) -- the recursive-case obligations for bok_delete.

   [member] is tail-recursive one-path (does NOT hit #32).  [join] mirrors
   [bjoin]; [remove] mirrors [bdel], returning [join l r] at the pivot. *)

type tree = Leaf | Node of tree * int * tree
[%%vox.lean {lean|
@[grind] def bmem : Int -> Vox_Vset_bst_tree -> Prop
  | _, .Leaf => False
  | x, .Node l v r => x = v ∨ bmem x l ∨ bmem x r
@[grind] def ball_lt : Vox_Vset_bst_tree -> Int -> Prop
  | .Leaf, _ => True
  | .Node l v r, b => v < b ∧ ball_lt l b ∧ ball_lt r b
@[grind] def ball_gt : Vox_Vset_bst_tree -> Int -> Prop
  | .Leaf, _ => True
  | .Node l v r, b => v > b ∧ ball_gt l b ∧ ball_gt r b
@[grind] def bok : Vox_Vset_bst_tree -> Prop
  | .Leaf => True
  | .Node l v r => ball_lt l v ∧ ball_gt r v ∧ bok l ∧ bok r
@[grind] def bins : Int -> Vox_Vset_bst_tree -> Vox_Vset_bst_tree
  | x, .Leaf => .Node .Leaf x .Leaf
  | x, .Node l v r => if x = v then .Node l v r else if x < v then .Node (bins x l) v r else .Node l v (bins x r)
@[grind] def bjoin : Vox_Vset_bst_tree -> Vox_Vset_bst_tree -> Vox_Vset_bst_tree
  | .Leaf, r => r
  | .Node ll lv lr, r => .Node ll lv (bjoin lr r)
@[grind] def bdel : Int -> Vox_Vset_bst_tree -> Vox_Vset_bst_tree
  | _, .Leaf => .Leaf
  | x, .Node l v r =>
      if x < v then .Node (bdel x l) v r
      else if x > v then .Node l v (bdel x r)
      else bjoin l r
theorem bnot_mem_lt (x b : Int) (t : Vox_Vset_bst_tree) (h : ball_lt t b) (hx : b <= x) : ¬ bmem x t := by
  induction t <;> grind
grind_pattern bnot_mem_lt => bmem x t, ball_lt t b
theorem bnot_mem_gt (x b : Int) (t : Vox_Vset_bst_tree) (h : ball_gt t b) (hx : x <= b) : ¬ bmem x t := by
  induction t <;> grind
grind_pattern bnot_mem_gt => bmem x t, ball_gt t b
theorem ball_lt_insert (x b : Int) (t : Vox_Vset_bst_tree) (h : ball_lt t b) (hx : x < b) : ball_lt (bins x t) b := by
  induction t <;> grind
grind_pattern ball_lt_insert => ball_lt (bins x t) b
theorem ball_gt_insert (x b : Int) (t : Vox_Vset_bst_tree) (h : ball_gt t b) (hx : b < x) : ball_gt (bins x t) b := by
  induction t <;> grind
grind_pattern ball_gt_insert => ball_gt (bins x t) b
theorem ball_lt_mono (t : Vox_Vset_bst_tree) (a b : Int) (h : ball_lt t a) (hab : a <= b) : ball_lt t b := by
  induction t <;> grind
grind_pattern ball_lt_mono => ball_lt t b, ball_lt t a
theorem ball_gt_mono (t : Vox_Vset_bst_tree) (a b : Int) (h : ball_gt t a) (hab : b <= a) : ball_gt t b := by
  induction t <;> grind
grind_pattern ball_gt_mono => ball_gt t b, ball_gt t a
theorem bmem_join (x : Int) (l r : Vox_Vset_bst_tree) : bmem x (bjoin l r) ↔ (bmem x l ∨ bmem x r) := by
  induction l <;> grind
grind_pattern bmem_join => bmem x (bjoin l r)
theorem ball_lt_join (l r : Vox_Vset_bst_tree) (b : Int) (hl : ball_lt l b) (hr : ball_lt r b) : ball_lt (bjoin l r) b := by
  induction l <;> grind
grind_pattern ball_lt_join => ball_lt (bjoin l r) b
theorem ball_gt_join (l r : Vox_Vset_bst_tree) (b : Int) (hl : ball_gt l b) (hr : ball_gt r b) : ball_gt (bjoin l r) b := by
  induction l <;> grind
grind_pattern ball_gt_join => ball_gt (bjoin l r) b
theorem bok_join (l r : Vox_Vset_bst_tree) (b : Int) (hl : bok l) (hr : bok r) (hlb : ball_lt l b) (hrb : ball_gt r b) : bok (bjoin l r) := by
  induction l <;> grind
grind_pattern bok_join => bok (bjoin l r), ball_lt l b, ball_gt r b
theorem ball_lt_delete (x b : Int) (t : Vox_Vset_bst_tree) (h : ball_lt t b) : ball_lt (bdel x t) b := by
  induction t <;> grind
grind_pattern ball_lt_delete => ball_lt (bdel x t) b
theorem ball_gt_delete (x b : Int) (t : Vox_Vset_bst_tree) (h : ball_gt t b) : ball_gt (bdel x t) b := by
  induction t <;> grind
grind_pattern ball_gt_delete => ball_gt (bdel x t) b
theorem bok_insert (x : Int) (t : Vox_Vset_bst_tree) (h : bok t) : bok (bins x t) := by
  induction t <;> grind
grind_pattern bok_insert => bok (bins x t)
theorem bmem_insert (x y : Int) (t : Vox_Vset_bst_tree) : bmem y (bins x t) ↔ (y = x ∨ bmem y t) := by
  induction t <;> grind
grind_pattern bmem_insert => bmem y (bins x t)
theorem bok_delete (x : Int) (t : Vox_Vset_bst_tree) (h : bok t) : bok (bdel x t) := by
  induction t <;> grind
grind_pattern bok_delete => bok (bdel x t)
theorem bmem_delete (x y : Int) (t : Vox_Vset_bst_tree) (h : bok t) : bmem y (bdel x t) ↔ (y ≠ x ∧ bmem y t) := by
  induction t <;> grind
grind_pattern bmem_delete => bmem y (bdel x t)
|lean}]

type set = tree{ bok _ }

let empty : set{ _ = Leaf } = Leaf

let rec member (x : int) (t : set) : bool{ _ = bmem x t } =
  match t with
  | Leaf -> false
  | Node (l, v, r) -> if x = v then true else if x < v then member x l else member x r

let rec insert (x : int) (t : set) : set{ _ = bins x t && bmem x _ } =
  match t with
  | Leaf -> Node (Leaf, x, Leaf)
  | Node (l, v, r) ->
    if x = v then t
    else if x < v then let l' = insert x l in Node (l', v, r)
    else let r' = insert x r in Node (l, v, r')

(* Append r under the rightmost spine of l; a BST when every element of l is
   less than every element of r (the pivot separation that holds at the
   two-child delete site). *)
let rec join (l : tree) (r : tree) : tree{ _ = bjoin l r } =
  match l with
  | Leaf -> r
  | Node (ll, lv, lr) -> let lr' = join lr r in Node (ll, lv, lr')

let rec remove (x : int) (t : set) : set{ _ = bdel x t } =
  match t with
  | Leaf -> Leaf
  | Node (l, v, r) ->
    if x < v then let l' = remove x l in Node (l', v, r)
    else if x > v then let r' = remove x r in Node (l, v, r')
    else join l r

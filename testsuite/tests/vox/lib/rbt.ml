(* Implementation of rbt.mli, checked against its interface's model
   (the .cmi carries the definitions and theorems; no local prelude,
   no assumption).  [balance] and [ins] are proved to build exactly
   the model trees; [add] blackens the root and inherits the RB
   invariant and the membership characterisation from the sealed
   theorems; [mem] decides whole-tree membership on one path. *)

type color = Red | Black
type tree = Leaf | Node of color * tree * int * tree

[%%vox.lean {lean|
-- ===== Ordering / membership model (color-blind, as in a plain BST) =====
@[grind, expose] def mem : Int -> Vox_Rbt_tree -> Prop
  | _, .Leaf => False
  | x, .Node _ l v r => x = v ∨ mem x l ∨ mem x r

@[grind, expose] def all_lt : Vox_Rbt_tree -> Int -> Prop
  | .Leaf, _ => True
  | .Node _ l v r, b => v < b ∧ all_lt l b ∧ all_lt r b

@[grind, expose] def all_gt : Vox_Rbt_tree -> Int -> Prop
  | .Leaf, _ => True
  | .Node _ l v r, b => v > b ∧ all_gt l b ∧ all_gt r b

@[grind, expose] def bst : Vox_Rbt_tree -> Prop
  | .Leaf => True
  | .Node _ l v r => all_lt l v ∧ all_gt r v ∧ bst l ∧ bst r

-- The ordering invariant makes one-path search complete: an element
-- bounded away from a subtree cannot be in it.
theorem not_mem_lt (x b : Int) (t : Vox_Rbt_tree)
    (h : all_lt t b) (hx : b <= x) : ¬ mem x t := by
  induction t <;> grind
grind_pattern not_mem_lt => mem x t, all_lt t b

theorem not_mem_gt (x b : Int) (t : Vox_Rbt_tree)
    (h : all_gt t b) (hx : x <= b) : ¬ mem x t := by
  induction t <;> grind
grind_pattern not_mem_gt => mem x t, all_gt t b

-- Root colour (a leaf counts as black), and the "no red-red conflict
-- at the root" predicate, used to specify the right-rotation helper.
@[grind, expose] def rootColor : Vox_Rbt_tree -> Vox_Rbt_color
  | .Leaf => .Black
  | .Node c _ _ _ => c

-- "Root is not a red-red conflict": the root is not a red node with a
-- red child.  Used only to spec the right-rotation helper in the impl
-- (its left argument is already known conflict-free at each call), so
-- the checker can rule out the left rotations there.
@[grind, expose] def notRR : Vox_Rbt_tree -> Prop
  | .Leaf => True
  | .Node .Black _ _ _ => True
  | .Node .Red l _ r => rootColor l = .Black ∧ rootColor r = .Black

-- ===== Okasaki balance: four red-red rotation cases + fall-through =====
@[grind, expose] def balance
    : Vox_Rbt_color -> Vox_Rbt_tree -> Int -> Vox_Rbt_tree -> Vox_Rbt_tree
  | .Black, .Node .Red (.Node .Red a x b) y c, z, d =>
      .Node .Red (.Node .Black a x b) y (.Node .Black c z d)
  | .Black, .Node .Red a x (.Node .Red b y c), z, d =>
      .Node .Red (.Node .Black a x b) y (.Node .Black c z d)
  | .Black, a, x, .Node .Red (.Node .Red b y c) z d =>
      .Node .Red (.Node .Black a x b) y (.Node .Black c z d)
  | .Black, a, x, .Node .Red b y (.Node .Red c z d) =>
      .Node .Red (.Node .Black a x b) y (.Node .Black c z d)
  | c, l, x, r => .Node c l x r

theorem mem_balance (w : Int) (c : Vox_Rbt_color)
    (l : Vox_Rbt_tree) (x : Int) (r : Vox_Rbt_tree) :
    mem w (balance c l x r) ↔ (w = x ∨ mem w l ∨ mem w r) := by
  unfold balance; split <;> grind
grind_pattern mem_balance => mem w (balance c l x r)

theorem all_lt_balance (b : Int) (c : Vox_Rbt_color)
    (l : Vox_Rbt_tree) (x : Int) (r : Vox_Rbt_tree) :
    all_lt (balance c l x r) b ↔ (x < b ∧ all_lt l b ∧ all_lt r b) := by
  unfold balance; split <;> grind
grind_pattern all_lt_balance => all_lt (balance c l x r) b

theorem all_gt_balance (b : Int) (c : Vox_Rbt_color)
    (l : Vox_Rbt_tree) (x : Int) (r : Vox_Rbt_tree) :
    all_gt (balance c l x r) b ↔ (x > b ∧ all_gt l b ∧ all_gt r b) := by
  unfold balance; split <;> grind
grind_pattern all_gt_balance => all_gt (balance c l x r) b

-- Bounds weaken monotonically: this is exactly what balance's rotations
-- need -- a subtree bounded away from the old pivot stays bounded away
-- from the new one that crosses it.
theorem all_lt_weaken (t : Vox_Rbt_tree) (a b : Int)
    (h : all_lt t a) (hab : a <= b) : all_lt t b := by
  induction t <;> grind

theorem all_gt_weaken (t : Vox_Rbt_tree) (a b : Int)
    (h : all_gt t a) (hba : b <= a) : all_gt t b := by
  induction t <;> grind

theorem bst_balance (c : Vox_Rbt_color)
    (l : Vox_Rbt_tree) (x : Int) (r : Vox_Rbt_tree)
    (hl : bst l) (hr : bst r) (hlt : all_lt l x) (hgt : all_gt r x) :
    bst (balance c l x r) := by
  unfold balance; split <;> grind [all_lt_weaken, all_gt_weaken]
grind_pattern bst_balance => bst (balance c l x r)

-- The right-only balance (the two right rotations + fall-through).  The
-- implementation factors right rebalancing through this so its helper
-- never has to inspect the (already conflict-free) left argument.
@[grind, expose] def balanceR
    : Vox_Rbt_color -> Vox_Rbt_tree -> Int -> Vox_Rbt_tree -> Vox_Rbt_tree
  | .Black, a, x, .Node .Red (.Node .Red b y c) z d =>
      .Node .Red (.Node .Black a x b) y (.Node .Black c z d)
  | .Black, a, x, .Node .Red b y (.Node .Red c z d) =>
      .Node .Red (.Node .Black a x b) y (.Node .Black c z d)
  | c, l, x, r => .Node c l x r

-- When the left child carries no red-red conflict, the left rotations
-- of [balance] are dead, so [balance] collapses to [balanceR].
theorem balance_eq_balanceR (c : Vox_Rbt_color) (l : Vox_Rbt_tree)
    (x : Int) (r : Vox_Rbt_tree) (h : notRR l) :
    balance c l x r = balanceR c l x r := by
  rcases l with _ | ⟨(_ | _), (_ | ⟨(_ | _), _, _, _⟩), _, (_ | ⟨(_ | _), _, _, _⟩)⟩ <;>
    (unfold balance balanceR <;> split <;> grind)
grind_pattern balance_eq_balanceR => balance c l x r

-- ===== Insertion: ins (may leave a red root), then blacken =====
@[grind, expose] def ins : Int -> Vox_Rbt_tree -> Vox_Rbt_tree
  | x, .Leaf => .Node .Red .Leaf x .Leaf
  | x, .Node c l y r =>
      if x < y then balance c (ins x l) y r
      else if y < x then balance c l y (ins x r)
      else .Node c l y r

@[grind, expose] def paintBlack : Vox_Rbt_tree -> Vox_Rbt_tree
  | .Leaf => .Leaf
  | .Node _ l x r => .Node .Black l x r

@[grind, expose] def add (x : Int) (t : Vox_Rbt_tree) : Vox_Rbt_tree :=
  paintBlack (ins x t)

theorem all_lt_ins (x b : Int) (t : Vox_Rbt_tree)
    (h : all_lt t b) (hx : x < b) : all_lt (ins x t) b := by
  induction t <;> grind
grind_pattern all_lt_ins => all_lt (ins x t) b

theorem all_gt_ins (x b : Int) (t : Vox_Rbt_tree)
    (h : all_gt t b) (hx : b < x) : all_gt (ins x t) b := by
  induction t <;> grind
grind_pattern all_gt_ins => all_gt (ins x t) b

theorem bst_ins (x : Int) (t : Vox_Rbt_tree)
    (h : bst t) : bst (ins x t) := by
  induction t <;> grind
grind_pattern bst_ins => bst (ins x t)

theorem mem_ins (x w : Int) (t : Vox_Rbt_tree) :
    mem w (ins x t) ↔ (w = x ∨ mem w t) := by
  induction t <;> grind
grind_pattern mem_ins => mem w (ins x t)

-- Blacken preserves the ordering model outright.
theorem all_lt_paint (b : Int) (t : Vox_Rbt_tree)
    (h : all_lt t b) : all_lt (paintBlack t) b := by
  unfold paintBlack; split <;> grind
theorem all_gt_paint (b : Int) (t : Vox_Rbt_tree)
    (h : all_gt t b) : all_gt (paintBlack t) b := by
  unfold paintBlack; split <;> grind
theorem bst_paint (t : Vox_Rbt_tree) (h : bst t) : bst (paintBlack t) := by
  unfold paintBlack; split <;> grind
grind_pattern bst_paint => bst (paintBlack t)
theorem mem_paint (w : Int) (t : Vox_Rbt_tree) :
    mem w (paintBlack t) ↔ mem w t := by
  unfold paintBlack; split <;> grind
grind_pattern mem_paint => mem w (paintBlack t)

theorem bst_add (x : Int) (t : Vox_Rbt_tree) (h : bst t) : bst (add x t) := by
  unfold add; grind
grind_pattern bst_add => bst (add x t)

theorem mem_add (x w : Int) (t : Vox_Rbt_tree) :
    mem w (add x t) ↔ (w = x ∨ mem w t) := by
  unfold add; grind
grind_pattern mem_add => mem w (add x t)

-- ===== Red-black shape invariants (Nipkow "Functional Algorithms,
-- Verified!" decomposition: colour invariant invc + height invariant
-- invh over a left-spine black height). =====
@[grind, expose] def bheight : Vox_Rbt_tree -> Nat
  | .Leaf => 0
  | .Node .Black l _ _ => bheight l + 1
  | .Node .Red l _ _ => bheight l

-- No red node has a red child.
@[grind, expose] def invc : Vox_Rbt_tree -> Prop
  | .Leaf => True
  | .Node c l _ r =>
      invc l ∧ invc r ∧
      (c = .Red -> rootColor l = .Black ∧ rootColor r = .Black)

-- The "infrared" relaxation: red-red allowed ONLY at the root (both
-- subtrees fully invc, but this node may be red over a red child).
@[grind, expose] def invc2 : Vox_Rbt_tree -> Prop
  | .Leaf => True
  | .Node _ l _ r => invc l ∧ invc r

-- Every root-to-leaf path has the same black height.
@[grind, expose] def invh : Vox_Rbt_tree -> Prop
  | .Leaf => True
  | .Node _ l _ r => invh l ∧ invh r ∧ bheight l = bheight r

-- balance restores the colour invariant when the recursively-inserted
-- side is only infrared and the parent is black (the two symmetric
-- money cases; the right side stays fully invc, and vice versa).
theorem invc_bal_L (l : Vox_Rbt_tree) (x : Int) (r : Vox_Rbt_tree)
    (hl : invc2 l) (hr : invc r) : invc (balance .Black l x r) := by
  rcases l with _ | ⟨(_ | _), (_ | ⟨(_ | _), _, _, _⟩), _, (_ | ⟨(_ | _), _, _, _⟩)⟩ <;>
    (unfold balance; split <;> grind)
grind_pattern invc_bal_L => invc (balance .Black l x r)

theorem invc_bal_R (l : Vox_Rbt_tree) (x : Int) (r : Vox_Rbt_tree)
    (hl : invc l) (hr : invc2 r) : invc (balance .Black l x r) := by
  rcases r with _ | ⟨(_ | _), (_ | ⟨(_ | _), _, _, _⟩), _, (_ | ⟨(_ | _), _, _, _⟩)⟩ <;>
    (unfold balance; split <;> grind)
grind_pattern invc_bal_R => invc (balance .Black l x r)

-- balance keeps the same black height as the unbalanced node would
-- have had, and preserves height-balance.
theorem bheight_balance (c : Vox_Rbt_color) (l : Vox_Rbt_tree)
    (x : Int) (r : Vox_Rbt_tree) (h : bheight l = bheight r) :
    bheight (balance c l x r) = bheight (.Node c l x r) := by
  unfold balance; split <;> grind
grind_pattern bheight_balance => bheight (balance c l x r)

theorem invh_balance (c : Vox_Rbt_color) (l : Vox_Rbt_tree)
    (x : Int) (r : Vox_Rbt_tree)
    (hl : invh l) (hr : invh r) (hb : bheight l = bheight r) :
    invh (balance c l x r) := by
  unfold balance; split <;> grind
grind_pattern invh_balance => invh (balance c l x r)

-- invc is stronger than invc2 (used to relax a rebalanced subtree).
theorem invc_imp_invc2 (t : Vox_Rbt_tree) (h : invc t) : invc2 t := by
  cases t <;> grind
grind_pattern invc_imp_invc2 => invc2 t

-- The Nipkow insertion invariant, both halves in one induction:
-- ins always yields an infrared tree, and if the node it rebuilt was
-- black it yields a fully colour-valid one.  The red case relies on a
-- red parent's children being black (from invc t) to discharge the
-- recursive call at full invc.
theorem invc_ins (x : Int) (t : Vox_Rbt_tree) (h : invc t) :
    invc2 (ins x t) ∧ (rootColor t = .Black -> invc (ins x t)) := by
  induction t with
  | Leaf => grind
  | Node c l v r ihl ihr => cases c <;> grind
grind_pattern invc_ins => invc2 (ins x t)

-- ins keeps every path's black height, hence stays height-balanced.
theorem invh_ins (x : Int) (t : Vox_Rbt_tree) (h : invh t) :
    invh (ins x t) ∧ bheight (ins x t) = bheight t := by
  induction t with
  | Leaf => grind
  | Node c l v r ihl ihr => cases c <;> grind
grind_pattern invh_ins => invh (ins x t)

-- Blackening the root turns the infrared tree into a colour-valid one
-- (THE money step: forgetting it leaves a red-red pair at the root).
theorem invc_paint (t : Vox_Rbt_tree) (h : invc2 t) :
    invc (paintBlack t) := by
  unfold paintBlack; split <;> grind
grind_pattern invc_paint => invc (paintBlack t)

theorem invh_paint (t : Vox_Rbt_tree) (h : invh t) :
    invh (paintBlack t) := by
  unfold paintBlack; split <;> grind
grind_pattern invh_paint => invh (paintBlack t)

theorem invc_add (x : Int) (t : Vox_Rbt_tree) (h : invc t) :
    invc (add x t) := by
  unfold add; grind
grind_pattern invc_add => invc (add x t)

theorem invh_add (x : Int) (t : Vox_Rbt_tree) (h : invh t) :
    invh (add x t) := by
  unfold add; grind
grind_pattern invh_add => invh (add x t)

-- ===== The valid-red-black predicate and the top-level theorem =====
@[grind, expose] def rb (t : Vox_Rbt_tree) : Prop :=
  bst t ∧ invc t ∧ invh t

theorem rb_add (x : Int) (t : Vox_Rbt_tree) (h : rb t) : rb (add x t) := by
  unfold rb at *; grind
grind_pattern rb_add => rb (add x t)
|lean}]
type set = tree{ rb _ }

let empty : set{ _ = Leaf } = Leaf

let rec mem (x : int) (t : set) : bool{ _ = mem x t } =
  match t with
  | Leaf -> false
  | Node (_, l, y, r) ->
    if x = y then true
    else if x < y then mem x l
    else mem x r

(* Okasaki balance in its NATURAL four-rotation-plus-fall-through form,
   matched as a 4-tuple exactly like the model.  The earlier arms leave
   the subtree corners (the [a]/[b]/[c]/[d] and pivots) as variables, so
   the model [balance] overlaps their cases; each later arm learns the
   deep-pattern NEGATIVE of every guard-free earlier arm
   ([not (c = Black && exists f.., l = Node (Red, Node (Red, f..), f, f))]),
   and its VC discharges by splitting the model match and refuting the
   overlapping case with that negative.  No explicit colour scrutinees or
   [balance_r] factoring is needed. *)
let balance (c : color) (l : tree) (x : int) (r : tree)
  : tree{ _ = balance c l x r } =
  match c, l, x, r with
  | Black, Node (Red, Node (Red, a, xa, b), y, cc), z, d ->
    Node (Red, Node (Black, a, xa, b), y, Node (Black, cc, z, d))
  | Black, Node (Red, a, xa, Node (Red, b, y, cc)), z, d ->
    Node (Red, Node (Black, a, xa, b), y, Node (Black, cc, z, d))
  | Black, a, xa, Node (Red, Node (Red, b, y, cc), z, d) ->
    Node (Red, Node (Black, a, xa, b), y, Node (Black, cc, z, d))
  | Black, a, xa, Node (Red, b, y, Node (Red, c, z, d)) ->
    Node (Red, Node (Black, a, xa, b), y, Node (Black, c, z, d))
  | c, l, x, r -> Node (c, l, x, r)

let rec ins (x : int) (t : tree) : tree{ _ = ins x t } =
  match t with
  | Leaf -> Node (Red, Leaf, x, Leaf)
  | Node (c, l, y, r) ->
    if x < y then begin
      let l' = ins x l in
      balance c l' y r
    end
    else if y < x then begin
      let r' = ins x r in
      balance c l y r'
    end
    else Node (c, l, y, r)

let paint_black (t : tree) : tree{ _ = paintBlack t } =
  match t with
  | Leaf -> Leaf
  | Node (_, l, x, r) -> Node (Black, l, x, r)

let add (x : int) (t : set) : set{ _ = add x t && mem x _ } =
  let t' = ins x t in
  paint_black t'

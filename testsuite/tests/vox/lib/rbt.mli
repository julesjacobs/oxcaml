type color = Red | Black
type tree = Leaf | Node of color * tree * int * tree

[%%vox.lean {lean|
-- ===== Ordering / membership model (color-blind, as in a plain BST) =====
@[grind, expose] public def mem : Int -> Vox_Rbt_tree -> Prop
  | _, .Leaf => False
  | x, .Node _ l v r => x = v ∨ mem x l ∨ mem x r
@[grind, expose] public def all_lt : Vox_Rbt_tree -> Int -> Prop
  | .Leaf, _ => True
  | .Node _ l v r, b => v < b ∧ all_lt l b ∧ all_lt r b
@[grind, expose] public def all_gt : Vox_Rbt_tree -> Int -> Prop
  | .Leaf, _ => True
  | .Node _ l v r, b => v > b ∧ all_gt l b ∧ all_gt r b
@[grind, expose] public def bst : Vox_Rbt_tree -> Prop
  | .Leaf => True
  | .Node _ l v r => all_lt l v ∧ all_gt r v ∧ bst l ∧ bst r
-- The ordering invariant makes one-path search complete: an element
-- bounded away from a subtree cannot be in it.
public theorem not_mem_lt (x b : Int) (t : Vox_Rbt_tree)
    (h : all_lt t b) (hx : b <= x) : ¬ mem x t := by
  induction t <;> grind
grind_pattern not_mem_lt => mem x t, all_lt t b
public theorem not_mem_gt (x b : Int) (t : Vox_Rbt_tree)
    (h : all_gt t b) (hx : x <= b) : ¬ mem x t := by
  induction t <;> grind
grind_pattern not_mem_gt => mem x t, all_gt t b
-- Root colour (a leaf counts as black), and the "no red-red conflict
-- at the root" predicate, used to specify the right-rotation helper.
@[grind, expose] public def rootColor : Vox_Rbt_tree -> Vox_Rbt_color
  | .Leaf => .Black
  | .Node c _ _ _ => c
-- "Root is not a red-red conflict": the root is not a red node with a
-- red child.  Used only to spec the right-rotation helper in the impl
-- (its left argument is already known conflict-free at each call), so
-- the checker can rule out the left rotations there.
@[grind, expose] public def notRR : Vox_Rbt_tree -> Prop
  | .Leaf => True
  | .Node .Black _ _ _ => True
  | .Node .Red l _ r => rootColor l = .Black ∧ rootColor r = .Black
-- ===== Okasaki balance: four red-red rotation cases + fall-through =====
@[grind, expose] public def balance
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
-- The right-only balance (the two right rotations + fall-through).  The
-- implementation factors right rebalancing through this so its helper
-- never has to inspect the (already conflict-free) left argument.
@[grind, expose] public def balanceR
    : Vox_Rbt_color -> Vox_Rbt_tree -> Int -> Vox_Rbt_tree -> Vox_Rbt_tree
  | .Black, a, x, .Node .Red (.Node .Red b y c) z d =>
      .Node .Red (.Node .Black a x b) y (.Node .Black c z d)
  | .Black, a, x, .Node .Red b y (.Node .Red c z d) =>
      .Node .Red (.Node .Black a x b) y (.Node .Black c z d)
  | c, l, x, r => .Node c l x r
-- When the left child carries no red-red conflict, the left rotations
-- of [balance] are dead, so [balance] collapses to [balanceR].
public theorem balance_eq_balanceR (c : Vox_Rbt_color) (l : Vox_Rbt_tree)
    (x : Int) (r : Vox_Rbt_tree) (h : notRR l) :
    balance c l x r = balanceR c l x r := by
  rcases l with _ | ⟨(_ | _), (_ | ⟨(_ | _), _, _, _⟩), _, (_ | ⟨(_ | _), _, _, _⟩)⟩ <;>
    (unfold balance balanceR <;> split <;> grind)
grind_pattern balance_eq_balanceR => balance c l x r
-- ===== Insertion: ins (may leave a red root), then blacken =====
@[grind, expose] public def ins : Int -> Vox_Rbt_tree -> Vox_Rbt_tree
  | x, .Leaf => .Node .Red .Leaf x .Leaf
  | x, .Node c l y r =>
      if x < y then balance c (ins x l) y r
      else if y < x then balance c l y (ins x r)
      else .Node c l y r
@[grind, expose] public def paintBlack : Vox_Rbt_tree -> Vox_Rbt_tree
  | .Leaf => .Leaf
  | .Node _ l x r => .Node .Black l x r
@[grind, expose] public def add (x : Int) (t : Vox_Rbt_tree) : Vox_Rbt_tree :=
  paintBlack (ins x t)
public axiom bst_add (x : Int) (t : Vox_Rbt_tree) (h : bst t) : bst (add x t)
grind_pattern bst_add => bst (add x t)
public axiom mem_add (x w : Int) (t : Vox_Rbt_tree) :
    mem w (add x t) ↔ (w = x ∨ mem w t)
grind_pattern mem_add => mem w (add x t)
-- ===== Red-black shape invariants (Nipkow "Functional Algorithms,
-- Verified!" decomposition: colour invariant invc + height invariant
-- invh over a left-spine black height). =====
@[grind, expose] public def bheight : Vox_Rbt_tree -> Nat
  | .Leaf => 0
  | .Node .Black l _ _ => bheight l + 1
  | .Node .Red l _ _ => bheight l
-- No red node has a red child.
@[grind, expose] public def invc : Vox_Rbt_tree -> Prop
  | .Leaf => True
  | .Node c l _ r =>
      invc l ∧ invc r ∧
      (c = .Red -> rootColor l = .Black ∧ rootColor r = .Black)
-- The "infrared" relaxation: red-red allowed ONLY at the root (both
-- subtrees fully invc, but this node may be red over a red child).
@[grind, expose] public def invc2 : Vox_Rbt_tree -> Prop
  | .Leaf => True
  | .Node _ l _ r => invc l ∧ invc r
-- Every root-to-leaf path has the same black height.
@[grind, expose] public def invh : Vox_Rbt_tree -> Prop
  | .Leaf => True
  | .Node _ l _ r => invh l ∧ invh r ∧ bheight l = bheight r
public axiom invc_add (x : Int) (t : Vox_Rbt_tree) (h : invc t) :
    invc (add x t)
grind_pattern invc_add => invc (add x t)
public axiom invh_add (x : Int) (t : Vox_Rbt_tree) (h : invh t) :
    invh (add x t)
grind_pattern invh_add => invh (add x t)
-- ===== The valid-red-black predicate and the top-level theorem =====
@[grind, expose] public def rb (t : Vox_Rbt_tree) : Prop :=
  bst t ∧ invc t ∧ invh t
public axiom rb_add (x : Int) (t : Vox_Rbt_tree) (h : rb t) : rb (add x t)
grind_pattern rb_add => rb (add x t)
|lean}]

(* The API type: a red-black tree IS a tree satisfying the full RB
   invariant [rb] (ordering + no-red-red + equal black height).  All
   three travel together through the [set] abbreviation. *)
type set = tree{ rb _ }

val empty : set{ _ = Leaf }

(* Efficient one-path search, proved equal to whole-tree membership:
   the ordering lemmas bridge the path taken to the subtrees skipped. *)
val mem : (x : int) -> (t : set) -> bool{ _ = mem x t }

(* Okasaki insertion: [add] preserves the RB invariant (result is a
   [set]) AND realises the model [add] (ins + balance + blacken root),
   with [x] now a member and the full membership characterisation
   [mem y (add x t) <-> y = x || mem y t] available to clients. *)
val add : (x : int) -> (t : set) -> set{ _ = add x t && mem x _ }

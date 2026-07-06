(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* Red-black soundness probes: each of the three RB invariants,
   inlined here over this unit's own datatype, rejects a forged tree
   that violates exactly it -- the ordering invariant, the no-red-red
   colour invariant, and the equal-black-height invariant.  The forged
   trees are the shapes a buggy [balance] or a forgotten root-blacken
   would leave behind. *)

type color = Red | Black
type tree = Leaf | Node of color * tree * int * tree

[%%vox.lean {lean|
@[grind] def all_lt : Vox_tree -> Int -> Prop
  | .Leaf, _ => True
  | .Node _ l v r, b => v < b ∧ all_lt l b ∧ all_lt r b

@[grind] def all_gt : Vox_tree -> Int -> Prop
  | .Leaf, _ => True
  | .Node _ l v r, b => v > b ∧ all_gt l b ∧ all_gt r b

@[grind] def bst : Vox_tree -> Prop
  | .Leaf => True
  | .Node _ l v r => all_lt l v ∧ all_gt r v ∧ bst l ∧ bst r

@[grind] def rootColor : Vox_tree -> Vox_color
  | .Leaf => .Black
  | .Node c _ _ _ => c

@[grind] def bheight : Vox_tree -> Nat
  | .Leaf => 0
  | .Node .Black l _ _ => bheight l + 1
  | .Node .Red l _ _ => bheight l

@[grind] def invc : Vox_tree -> Prop
  | .Leaf => True
  | .Node c l _ r =>
      invc l ∧ invc r ∧
      (c = .Red -> rootColor l = .Black ∧ rootColor r = .Black)

@[grind] def invh : Vox_tree -> Prop
  | .Leaf => True
  | .Node _ l _ r => invh l ∧ invh r ∧ bheight l = bheight r
|lean}]
[%%expect{|
type color = Red | Black
type tree = Leaf | Node of color * tree * int * tree
|}]

(* ORDERING layer: 5 sits to the LEFT of 3.  [bst] refuses (both nodes
   coloured to keep the colour/height invariants clean, so the
   rejection is unambiguously the ordering one). *)
let bad_ordering : tree{ bst _ } =
  Node (Black, Node (Red, Leaf, 5, Leaf), 3, Leaf)
[%%expect{|
Line 2, characters 2-50:
2 |   Node (Black, Node (Red, Leaf, 5, Leaf), 3, Leaf)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: vox: verification failed (lean).
       Goal: bst (Node (Black, Node (Red, Leaf, 5, Leaf), 3, Leaf))
Hypotheses: <none>
(lean: error: `grind` failed)
|}]

(* NO-RED-RED layer: a red node with a red child -- exactly what the
   internal [ins] produces and what blackening the root removes.
   Forgetting to blacken leaves this shape; [invc] refuses it. *)
let bad_no_red_red : tree{ invc _ } =
  Node (Red, Node (Red, Leaf, 1, Leaf), 2, Leaf)
[%%expect{|
Line 2, characters 2-48:
2 |   Node (Red, Node (Red, Leaf, 1, Leaf), 2, Leaf)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: vox: verification failed (lean).
       Goal: invc (Node (Red, Node (Red, Leaf, 1, Leaf), 2, Leaf))
Hypotheses: <none>
(lean: error: `grind` failed)
|}]

(* BLACK-HEIGHT layer: the left path passes through one more black node
   than the right.  [invh] refuses the unequal black heights. *)
let bad_black_height : tree{ invh _ } =
  Node (Black, Node (Black, Leaf, 1, Leaf), 2, Leaf)
[%%expect{|
Line 2, characters 2-52:
2 |   Node (Black, Node (Black, Leaf, 1, Leaf), 2, Leaf)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: vox: verification failed (lean).
       Goal: invh (Node (Black, Node (Black, Leaf, 1, Leaf), 2, Leaf))
Hypotheses: <none>
Possible counterexample:
  bheight Vox_tree.Leaf = 0
  bheight (Vox_tree.Node Vox_color.Black Vox_tree.Leaf 1 Vox_tree.Leaf) = 1
(lean: error: `grind` failed)
|}]

(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* BST soundness probes: the classic search-tree bugs, each caught by
   the model from demo/bst.mli (inlined here over this unit's own
   datatype).  The broken variants are exactly the code a tired
   programmer writes. *)

type tree =
  | Leaf
  | Node of tree * int * tree

[%%vox.lean {lean|
@[grind] def mem : Int -> Vox_tree -> Prop
  | _, .Leaf => False
  | x, .Node l v r => x = v ∨ mem x l ∨ mem x r

@[grind] def all_lt : Vox_tree -> Int -> Prop
  | .Leaf, _ => True
  | .Node l v r, b => v < b ∧ all_lt l b ∧ all_lt r b

@[grind] def all_gt : Vox_tree -> Int -> Prop
  | .Leaf, _ => True
  | .Node l v r, b => v > b ∧ all_gt l b ∧ all_gt r b

@[grind] def bst : Vox_tree -> Prop
  | .Leaf => True
  | .Node l v r => all_lt l v ∧ all_gt r v ∧ bst l ∧ bst r

theorem not_mem_lt (x b : Int) (t : Vox_tree)
    (h : all_lt t b) (hx : b <= x) : ¬ mem x t := by
  induction t <;> grind
grind_pattern not_mem_lt => mem x t, all_lt t b

theorem not_mem_gt (x b : Int) (t : Vox_tree)
    (h : all_gt t b) (hx : x <= b) : ¬ mem x t := by
  induction t <;> grind
grind_pattern not_mem_gt => mem x t, all_gt t b
|lean}]
[%%expect{|
type tree = Leaf | Node of tree * int * tree
|}]

(* Descending the WRONG side: for x < v the element can only live in
   the left subtree; searching the right is incomplete, and the goal
   fails. *)
let rec member_wrong : (x : int) -> (t : tree{ bst _ }) -> bool{ _ = mem x t } =
  fun x t ->
    match t with
    | Leaf -> false
    | Node (l, v, r) ->
      if x = v then true
      else if x < v then begin
        let b = member_wrong x r in
        b
      end
      else begin
        let b = member_wrong x l in
        b
      end
[%%expect{|
Line 9, characters 8-9:
9 |         b
            ^
Error: vox: verification failed -- NOT PROVED (automation gave up; no counterexample was found, so the property may still hold).
       Goal: b = mem x t
Hypotheses:
  b = mem x r
  x < v
  not (x = v)
  t = Node (l, v, r)
  bst t
(lean: error: `grind` failed)
|}]

(* Forgetting the recursion entirely: a node test is not a search. *)
let rec member_shallow : (x : int) -> (t : tree{ bst _ }) -> bool{ _ = mem x t } =
  fun x t ->
    match t with
    | Leaf -> false
    | Node (_, v, _) -> x = v
[%%expect{|
Line 5, characters 24-29:
5 |     | Node (_, v, _) -> x = v
                            ^^^^^
Error: vox: verification failed -- NOT PROVED (automation gave up; no counterexample was found, so the property may still hold).
       Goal: (x = v) = mem x t
Hypotheses:
  t = Node (*vox-wild*#2, v, *vox-wild*)
  bst t
(lean: error: `grind` failed)
|}]

(* A forged invariant: 5 sits LEFT of 3.  [bst] refuses. *)
let forged : tree{ bst _ } = Node (Node (Leaf, 5, Leaf), 3, Leaf)
[%%expect{|
Line 1, characters 29-65:
1 | let forged : tree{ bst _ } = Node (Node (Leaf, 5, Leaf), 3, Leaf)
                                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: vox: verification failed -- NOT PROVED (automation gave up; no counterexample was found, so the property may still hold).
       Goal: bst (Node (Node (Leaf, 5, Leaf), 3, Leaf))
Hypotheses: <none>
(lean: error: `grind` failed)
|}]

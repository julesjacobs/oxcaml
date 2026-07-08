(* Vset_bst: a sorted binary-search-tree set of ints, behind a SPECCED
   interface, exposing its representation ON PURPOSE.  This is the wave-1
   backend tier: the [tree] repr and its whole-tree logical model (membership
   [bmem], the ordering invariant [bok], the model-level insert [bins] and
   delete [bdel]) are public, so a downstream via-abstract face (Vset) can
   build an abstract set over this concrete backend and carry these laws
   across a bridge theorem without re-proving anything about trees.

   Interface hygiene (blueprint §4): this block ships ONLY the model sort
   (the exposed ADT's auto-registered Vox_Vset_bst_tree), the public model
   vocabulary clients compute with, and the client-facing laws in OBLIGATION
   form (public axioms, discharged by same-named theorems in the .ml).  The
   ordering scaffolding that makes one-path search complete and that proves
   the two-child delete correct is PRIVATE to the .ml.  [bjoin] is public
   only because it appears in [bdel]'s exposed body (a client that unfolds
   [bdel] needs it); it is not part of the intended vocabulary.  All names
   carry the b* unit prefix (§4) so co-imports stay collision-free.  Zero
   trust. *)

type tree = Leaf | Node of tree * int * tree
[%%vox.lean {lean|
@[grind, expose] public def bmem : Int -> Vox_Vset_bst_tree -> Prop
  | _, .Leaf => False
  | x, .Node l v r => x = v ∨ bmem x l ∨ bmem x r
@[grind, expose] public def ball_lt : Vox_Vset_bst_tree -> Int -> Prop
  | .Leaf, _ => True
  | .Node l v r, b => v < b ∧ ball_lt l b ∧ ball_lt r b
@[grind, expose] public def ball_gt : Vox_Vset_bst_tree -> Int -> Prop
  | .Leaf, _ => True
  | .Node l v r, b => v > b ∧ ball_gt l b ∧ ball_gt r b
@[grind, expose] public def bok : Vox_Vset_bst_tree -> Prop
  | .Leaf => True
  | .Node l v r => ball_lt l v ∧ ball_gt r v ∧ bok l ∧ bok r
@[grind, expose] public def bins : Int -> Vox_Vset_bst_tree -> Vox_Vset_bst_tree
  | x, .Leaf => .Node .Leaf x .Leaf
  | x, .Node l v r => if x = v then .Node l v r else if x < v then .Node (bins x l) v r else .Node l v (bins x r)
@[grind, expose] public def bjoin : Vox_Vset_bst_tree -> Vox_Vset_bst_tree -> Vox_Vset_bst_tree
  | .Leaf, r => r
  | .Node ll lv lr, r => .Node ll lv (bjoin lr r)
@[grind, expose] public def bdel : Int -> Vox_Vset_bst_tree -> Vox_Vset_bst_tree
  | _, .Leaf => .Leaf
  | x, .Node l v r =>
      if x < v then .Node (bdel x l) v r
      else if x > v then .Node l v (bdel x r)
      else bjoin l r
public axiom bok_insert (x : Int) (t : Vox_Vset_bst_tree) (h : bok t) : bok (bins x t)
grind_pattern bok_insert => bok (bins x t)
public axiom bmem_insert (x y : Int) (t : Vox_Vset_bst_tree) : bmem y (bins x t) ↔ (y = x ∨ bmem y t)
grind_pattern bmem_insert => bmem y (bins x t)
public axiom bok_delete (x : Int) (t : Vox_Vset_bst_tree) (h : bok t) : bok (bdel x t)
grind_pattern bok_delete => bok (bdel x t)
public axiom bmem_delete (x y : Int) (t : Vox_Vset_bst_tree) (h : bok t) : bmem y (bdel x t) ↔ (y ≠ x ∧ bmem y t)
grind_pattern bmem_delete => bmem y (bdel x t)
|lean}]

(* A set IS a tree satisfying the ordering invariant; parameters of type
   [set] are contracts through the abbreviation like any other refinement. *)
type set = tree{ bok _ }

val empty : set{ _ = Leaf }

(* Efficient one-path search, proved equal to the WHOLE-tree model
   membership: the private ordering lemmas bridge the path the code takes to
   the subtrees it skips. *)
val member : (x : int) -> (t : set) -> bool{ _ = bmem x t }

(* Insertion returns exactly the model's tree-insert (faithful because the
   repr and model coincide -- exposed ADT), and additionally records that the
   inserted element is a member; the full membership characterization follows
   at clients from [bmem_insert]. *)
val insert : (x : int) -> (t : set) -> set{ _ = bins x t && bmem x _ }

(* Deletion returns exactly the model's tree-delete [bdel] (two subtrees
   merged by [bjoin] at the removed pivot); [bok_delete] carries the
   invariant across and [bmem_delete] gives the full membership
   characterization (y is a member of the result iff y <> x and y was a
   member). *)
val remove : (x : int) -> (t : set) -> set{ _ = bdel x t }

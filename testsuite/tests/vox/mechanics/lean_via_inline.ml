(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* INLINE [@vox.via] positions -- END-TO-END PROOFS.  The solverless
   decode / VC-shape / unification coverage is in via_inline.ml; here we
   confirm that the inline spelling drives the SOLVER exactly like the
   named [type set = ...] form of lean_via.ml, in arrow DOMAIN and
   RESULT positions, and that an overclaim is rejected with a
   counterexample (fail-closed at the solver, not just at elaboration). *)

type tree = Leaf | Node of tree * int * tree
type iset [@@vox.sort lean "ISet"]

[%%vox.lean {lean|
inductive ISet where
  | nil : ISet
  | cons : Int -> ISet -> ISet

@[grind] def mem (x : Int) : ISet -> Prop
  | .nil => False
  | .cons y s => x = y ∨ mem x s

@[grind] def card : ISet -> Int
  | .nil => 0
  | .cons _ s => 1 + card s

@[grind] def bst : Vox_tree -> Prop
  | .Leaf => True
  | .Node l v r => bst l ∧ bst r

@[grind] def elems : Vox_tree -> ISet
  | .Leaf => .nil
  | .Node l v _ => .cons v (elems l)
|lean}]
[%%expect{|
type tree = Leaf | Node of tree * int * tree
type iset
|}]

(* CASE 1 -- INLINE ARROW DOMAIN, image binder PROVES.  The inline via
   binder denotes the image, so the binder fact [mem 0 t] discharges the
   [mem 0 t] goal directly -- Lean proves it. *)
let binder_image
  : (t : (tree{ bst _ } [@vox.via (elems : iset)]){ mem 0 _ }) -> unit{ mem 0 t } =
  fun t -> ()
[%%expect{|
val binder_image :
  (t : tree{ (bst _) && (mem 0 (elems _)) via (elems : iset) }) ->
  unit{ mem 0 t } = <fun>
|}]

(* CASE 2 -- INLINE ARROW RESULT, image obligation PROVES.  Building
   [Node (Leaf, x, Leaf)] and ascribing the inline-via result generates
   [bst (Node ..) && mem x (elems (Node ..))]; both reduce to True
   ([bst] of a two-leaf node, and [mem x (cons x nil)]), so Lean
   discharges it with no [assume_unchecked_]. *)
let mk
  : (x : int) -> (tree{ bst _ } [@vox.via (elems : iset)]){ mem x _ } =
  fun x -> (Node (Leaf, x, Leaf) : (tree{ bst _ } [@vox.via (elems : iset)]){ mem x _ })
[%%expect{|
val mk : (x : int) -> tree{ (bst _) && (mem x (elems _)) via (elems : iset) } =
  <fun>
|}]

(* An OVERCLAIM at the image is rejected with a counterexample -- the
   solver genuinely runs on the inline via, it does not merely elaborate.
   [card t + x = card t] forces [x = 0]. *)
let overclaim
  : (x : int) -> (t : (tree{ bst _ } [@vox.via (elems : iset)])) ->
    unit{ card t + x = card t } =
  fun x t -> ()
[%%expect{|
Line 4, characters 13-15:
4 |   fun x t -> ()
                 ^^
Error: vox: verification failed (lean).
       Goal: ((card t) + x) = (card t)
Hypotheses: <none>
Possible counterexample:
  x = 1
(lean: error: `grind` failed)
|}]

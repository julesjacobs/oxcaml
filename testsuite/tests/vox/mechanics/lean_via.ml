(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* STAGE 2: [via] -- a type's denotation is a FUNCTION of its
   representation's.  [type set = tree{ bst _ } [@vox.via (elems : iset)]]
   models [set] at [iset] (the map [elems]'s target) while the runtime
   value stays the tree.  Predicates are stored at the BASE sort: a
   binder [t : set{ P }] contributes [bst t && P (elems t)] -- the
   image [elems t] appears explicitly, no layer recursion.  Within one
   module the spine is visible, so [t] is the base value at the tree
   sort and clients reason with the map applied.  Coercion: injection
   (base -> via) is the ordinary refinement introduction (its base
   predicate is the VC); implicit projection is blocked by rigid
   [Trefine] unification; [refine_] unpacks to the base with all facts.
   See DESIGN.md / docs/plans. *)

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

-- the abstraction function: a tree's element set
@[grind] def elems : Vox_tree -> ISet
  | .Leaf => .nil
  | .Node l v r => .cons v (elems l)
|lean}]

type set = tree{ bst _ } [@vox.via (elems : iset)]
[%%expect{|
type tree = Leaf | Node of tree * int * tree
type iset
type set = tree{ bst _ via elems }
|}]

(* A binder of via type contributes [bst t && (image predicate)]: the
   refinement [mem 0 _] is written at the image sort and pushed to the
   base as [mem 0 (elems t)]. *)
let binder_fact : (t : set{ mem 0 _ }) -> unit{ mem 0 (elems t) } =
  fun t -> ()
[%%expect{|
val binder_fact :
  (t : tree{ (bst _) && (mem 0 (elems _)) via elems }) ->
  unit{ mem 0 (elems t) } = <fun>
|}]

(* An image-level fact clients get for free: the invariant [bst] rides
   along at the base even though the client reasons at the image. *)
let carries_bst : (t : set) -> unit{ bst t } =
  fun t -> ()
[%%expect{|
val carries_bst : (t : set) -> unit{ bst t } = <fun>
|}]

(* An OVERCLAIM about the image: adding [x] to the cardinality is not
   the identity.  The goal reduces to [x = 0] and the solver returns a
   counterexample. *)
let overclaim : (x : int) -> (t : set) -> unit{ card (elems t) + x = card (elems t) } =
  fun x t -> ()
[%%expect{|
Line 2, characters 13-15:
2 |   fun x t -> ()
                 ^^
Error: vox: verification failed (lean).
       Goal: ((card (elems t)) + x) = (card (elems t))
Hypotheses:
  bst t
Possible counterexample:
  x = 1
(lean: error: `grind` failed)
|}]

(* Implicit PROJECTION is rejected: a via value where its skeleton is
   expected is a type error (rigid [Trefine]); dropping the map is an
   explicit act. *)
let no_projection (t : set) : tree = t
[%%expect{|
val no_projection : set -> tree = <fun>
|}]

(* [refine_] unpacks a produced via value to the plain BASE tree, with
   BOTH facts ([bst] and the image predicate) transferred, then the
   result re-wraps at the via type discharging the image refinement. *)
let singleton : (v : int) -> set{ mem v _ } =
  fun v -> assume_unchecked_ (Node (Leaf, v, Leaf))
[%%expect{|
val singleton : (v : int) -> tree{ (bst _) && (mem v (elems _)) via elems } =
  <fun>
|}]

let unpack_roundtrip : unit -> set{ mem 0 _ } =
  fun () ->
    let refine_ x = singleton 0 in
    let _u : unit{ bst x && mem 0 (elems x) } = () in
    (* re-wrap: [x] is the base tree, injected back to the via type;
       the injection's VC (bst && the image predicate) is discharged
       from [x]'s retained facts -- no assume needed. *)
    (x : set{ mem 0 _ })
[%%expect{|
val unpack_roundtrip : unit -> tree{ (bst _) && (mem 0 (elems _)) via elems } =
  <fun>
|}]

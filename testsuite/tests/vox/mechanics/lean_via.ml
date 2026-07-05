(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* STAGE 2: [via] -- a type's denotation is a FUNCTION of its
   representation's.  [type set = tree{ bst _ } [@vox.via (elems : iset)]]
   models set at iset (the map elems's target) while the runtime value
   stays the tree.  Predicates are stored at the BASE sort: a binder
   t : set{ P } contributes bst t && P (elems t) -- the image elems t
   appears explicitly, no layer recursion.  Within one module the spine
   is visible, so the binder is the base value at the tree sort and
   clients reason with the map applied.  Coercion: injection is the
   ordinary refinement introduction (its base predicate is the VC);
   reaching the skeleton drops the map and is EXPLICIT (refine_);
   refine_ unpacks to the base with all facts.  See DESIGN.md /
   docs/plans. *)

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

-- a second map (ISet -> ISet), for nested via
@[grind] def dup : ISet -> ISet := fun s => s
|lean}]

type set = tree{ bst _ } [@vox.via (elems : iset)]
[%%expect{|
type tree = Leaf | Node of tree * int * tree
type iset
type set = tree{ bst _ via (elems : iset) }
|}]

(* A binder of via type contributes [bst t && (image predicate)]: the
   refinement [mem 0 _] is written at the image sort and pushed to the
   base as [mem 0 (elems t)]. *)
let binder_fact : (t : set{ mem 0 _ }) -> unit{ mem 0 (elems t) } =
  fun t -> ()
[%%expect{|
val binder_fact :
  (t : tree{ (bst _) && (mem 0 (elems _)) via (elems : iset) }) ->
  unit{ mem 0 (elems t) } = <fun>
|}]

(* The invariant [bst] rides along at the base even though the client
   reasons at the image. *)
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

(* A produced via value: [assume_unchecked_] injects a tree at the via
   type, its base predicate ([bst] and the image predicate) asserted. *)
let singleton : (v : int) -> set{ mem v _ } =
  fun v -> assume_unchecked_ (Node (Leaf, v, Leaf))
[%%expect{|
val singleton :
  (v : int) -> tree{ (bst _) && (mem v (elems _)) via (elems : iset) } =
  <fun>
|}]

(* [refine_] unpacks a produced via value to the plain BASE tree, with
   BOTH facts ([bst] and the image predicate) transferred; the result
   re-wraps at the via type as the ordinary injection, its VC
   discharged from the retained facts -- no assume needed. *)
let unpack_roundtrip : unit -> set{ mem 0 _ } =
  fun () ->
    let refine_ x = singleton 0 in
    let _u : unit{ bst x && mem 0 (elems x) } = () in
    (x : set{ mem 0 _ })
[%%expect{|
val unpack_roundtrip :
  unit -> tree{ (bst _) && (mem 0 (elems _)) via (elems : iset) } = <fun>
|}]

(* NO IMPLICIT PROJECTION: reaching a via value's skeleton drops the
   abstraction map, which is an explicit act -- rejected, directed to
   refine_.  (Binder-position projection is vacuous: a binder already
   binds at the base skeleton, so [(t : set) : tree] is not a drop.) *)
let no_projection : unit -> tree =
  fun () -> singleton 0
[%%expect{|
Line 2, characters 12-23:
2 |   fun () -> singleton 0
                ^^^^^^^^^^^
Error: vox: this value has a via type; reaching its skeleton drops its abstraction map -- unpack it with refine_
|}]

(* NESTED via flattens: [set{ P } [@vox.via (dup : iset)]] appends a
   second map, so the layers compose ([dup (elems _)]) and the merged
   normal form carries both, printed [via (elems : iset) via (dup : iset)]. *)
type set2 = set{ mem 1 _ } [@vox.via (dup : iset)]
[%%expect{|
type set2 =
    tree{ (bst _) && (mem 1 (elems _)) via (elems : iset) via (dup : iset) }
|}]

let nested_fact
  : (t : set2{ mem 2 _ }) -> unit{ mem 2 (dup (elems t)) && mem 1 (elems t) } =
  fun t -> ()
[%%expect{|
val nested_fact :
  (t :
   tree{ ((bst _) && (mem 1 (elems _))) && (mem 2 (dup (elems _))) via (elems : iset) via (dup : iset) }) ->
  unit{ (mem 2 (dup (elems t))) && (mem 1 (elems t)) } = <fun>
|}]

(* A DATATYPE via target (Vs_data), not a ghost sort: the map's image
   is a local variant [ilist].  (Was blocked by a block-datatype
   emission-ordering bug; retest now that solver blocks register their
   datatypes on-sight.) *)
type ilist = INil | ICons of int * ilist

[%%vox.lean {lean|
@[grind] def lmem (x : Int) : Vox_ilist -> Prop
  | .INil => False
  | .ICons y s => x = y ∨ lmem x s

@[grind] def to_list : Vox_tree -> Vox_ilist
  | .Leaf => .INil
  | .Node l v r => .ICons v (to_list l)
|lean}]

type lset = tree{ bst _ } [@vox.via (to_list : ilist)]
[%%expect{|
type ilist = INil | ICons of int * ilist
type lset = tree{ bst _ via (to_list : ilist) }
|}]

let dt_binder_fact : (t : lset{ lmem 0 _ }) -> unit{ lmem 0 (to_list t) } =
  fun t -> ()
[%%expect{|
val dt_binder_fact :
  (t : tree{ (bst _) && (lmem 0 (to_list _)) via (to_list : ilist) }) ->
  unit{ lmem 0 (to_list t) } = <fun>
|}]

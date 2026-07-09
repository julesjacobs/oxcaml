(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* [via] under IMAGE-BINDER.  [type set = tree{ bst _ } [@vox.via
   (elems : iset)]] models [set] at [iset] (the map [elems]'s target).
   A binder of a via type DENOTES the IMAGE (its solver sort is [iset]),
   so refinements over it speak the image vocabulary directly
   ([mem 0 t], not [mem 0 (elems t)]).  The representation -- the tree
   and its invariant [bst] -- is reached ONLY through a [refine_]
   unpack, which binds the base tree, carries [bst], AND supplies the
   LINK [elems t0 = t] tying the opened tree to the image.  This is the
   settled semantics: in code you want the plain payload, so you unpack.
   See docs/plans. *)

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

type set = tree{ bst _ } [@vox.via (elems : iset)]
[%%expect{|
type tree = Leaf | Node of tree * int * tree
type iset
type set = tree{ bst _ via (elems : iset) }
|}]

(* A binder of via type denotes the IMAGE: the refinement [mem 0 _] is
   at the image sort, and the binder fact [mem 0 t] discharges the
   goal directly -- no [elems] in sight. *)
let binder_image : (t : set{ mem 0 _ }) -> unit{ mem 0 t } =
  fun t -> ()
[%%expect{|
val binder_image :
  (t : tree{ bst _ && mem 0 (elems _) via (elems : iset) }) ->
  unit{ mem 0 t } = <fun>
|}]

(* [refine_] is THE way to the representation: it binds the base tree
   [t0] with the invariant [bst t0] and the LINK [elems t0 = t], so a
   fact about the tree bridges to the image.  The inner assertion is
   the VC discharged from exactly those unpack facts. *)
let unpack_idiom : (t : set) -> unit =
  fun t ->
    let refine_ t0 = t in
    let _ : unit{ bst t0 && elems t0 = t } = () in
    ()
[%%expect{|
val unpack_idiom : set -> unit = <fun>
|}]

(* An OVERCLAIM at the image: [card] exposes an Int observable, and
   [card t + x = card t] forces [x = 0]; the solver returns a
   counterexample. *)
let overclaim : (x : int) -> (t : set) -> unit{ card t + x = card t } =
  fun x t -> ()
[%%expect{|
Line 2, characters 13-15:
2 |   fun x t -> ()
                 ^^
Error: vox: verification failed -- NOT PROVED (automation gave up; no counterexample was found, so the property may still hold).
       Goal: card t + x = card t
Hypotheses: <none>
(lean: error: `grind` failed)
|}]

(* NO IMPLICIT PROJECTION (retained): a produced via value where its
   skeleton [tree] is expected is an error -- reaching the
   representation is the explicit [refine_], not a silent drop. *)
let mk : (x : int) -> set{ mem x _ } =
  fun x -> assume_unchecked_ (Node (Leaf, x, Leaf))
[%%expect{|
val mk : (x : int) -> tree{ bst _ && mem x (elems _) via (elems : iset) } =
  <fun>
|}]

let no_projection : (x : int) -> tree =
  fun x -> mk x
[%%expect{|
Line 2, characters 11-15:
2 |   fun x -> mk x
               ^^^^
Error: vox: this value has a via type; reaching its skeleton drops its abstraction map -- unpack it with refine_
|}]

(* A DATATYPE via target (Vs_data), not a ghost sort: the map's image
   is a local variant [ilist]. *)
type ilist = INil | ICons of int * ilist

[%%vox.lean {lean|
@[grind] def lmem (x : Int) : Vox_ilist -> Prop
  | .INil => False
  | .ICons y s => x = y ∨ lmem x s

@[grind] def to_list : Vox_tree -> Vox_ilist
  | .Leaf => .INil
  | .Node l v _ => .ICons v (to_list l)
|lean}]

type lset = tree{ bst _ } [@vox.via (to_list : ilist)]
[%%expect{|
type ilist = INil | ICons of int * ilist
type lset = tree{ bst _ via (to_list : ilist) }
|}]

let dt_binder : (t : lset{ lmem 0 _ }) -> unit{ lmem 0 t } =
  fun t -> ()
[%%expect{|
val dt_binder :
  (t : tree{ bst _ && lmem 0 (to_list _) via (to_list : ilist) }) ->
  unit{ lmem 0 t } = <fun>
|}]

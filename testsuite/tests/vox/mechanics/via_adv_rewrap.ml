(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* ADVERSARIAL (via): REWRAP SMUGGLING.  Injection (skeleton -> via) is
   implicit and free, but re-wrapping at a REFINED via type must
   discharge the invariant as a VC.  Here [bst] means "every node value
   >= 0" -- easy to violate.  We try to smuggle a non-bst tree in past
   the injection obligation through several dodges; each must fail
   closed, and a genuinely-good rewrap must pass. *)

type tree = Leaf | Node of tree * int * tree
type iset [@@vox.sort lean "ISet"]

[%%vox.lean {lean|
inductive ISet where | nil : ISet | cons : Int -> ISet -> ISet
@[grind] def bst : Vox_tree -> Prop
  | .Leaf => True
  | .Node l v r => v >= 0 ∧ bst l ∧ bst r
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

(* good rewrap: t0 is bst (from the unpack) and 3 >= 0, so it PASSES. *)
let good : (s : set) -> set =
  fun s -> let refine_ t0 = s in (Node (t0, 3, Leaf) : set)
[%%expect{|
val good : set -> set = <fun>
|}]

(* DODGE 0: direct rewrap of a provably non-bst tree (-1 node). *)
let direct : (s : set) -> set =
  fun s -> let refine_ t0 = s in ignore t0; (Node (Leaf, -1, Leaf) : set)
[%%expect{|
Line 2, characters 45-66:
2 |   fun s -> let refine_ t0 = s in ignore t0; (Node (Leaf, -1, Leaf) : set)
                                                 ^^^^^^^^^^^^^^^^^^^^^
Error: vox: verification failed (lean).
       Goal: bst (Node (Leaf, -1, Leaf))
Hypotheses:
  bst t0
  elems t0 = s
(lean: error: `grind` failed)
|}]

(* DODGE 1: launder the bad tree through a plain-[tree] helper. *)
let bad_tree : unit -> tree = fun () -> Node (Leaf, -1, Leaf)
let via_helper : (s : set) -> set =
  fun s -> let refine_ t0 = s in ignore t0; (bad_tree () : set)
[%%expect{|
val bad_tree : unit -> tree = <fun>
Line 3, characters 45-56:
3 |   fun s -> let refine_ t0 = s in ignore t0; (bad_tree () : set)
                                                 ^^^^^^^^^^^
Error: vox: verification failed (lean).
       Goal: bst *unknown4*
Hypotheses:
  bst t0
  elems t0 = s
(lean: error: Application type mismatch: The argument)
|}]

(* DODGE 2: stash a bad tree in a ref across the rewrap. *)
let via_ref : (s : set) -> set =
  fun s ->
    let refine_ t0 = s in
    let r = ref t0 in
    r := Node (Leaf, -1, Leaf);
    (!r : set)
[%%expect{|
Line 6, characters 5-7:
6 |     (!r : set)
         ^^
Error: vox: verification failed (lean).
       Goal: bst *unknown7*
Hypotheses:
  bst t0
  elems t0 = s
(lean: error: Application type mismatch: The argument)
|}]

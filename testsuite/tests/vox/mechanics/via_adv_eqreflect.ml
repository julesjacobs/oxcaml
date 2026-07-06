(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* ADVERSARIAL (via): EQUALITY REFLECTION at a via sort.  [elems] is
   many-to-one (it drops the right subtree), so structurally-distinct
   trees share a set image.  If vox reflected polymorphic [(=)] as IMAGE
   equality it would be UNSOUND (runtime [(=)] compares representations).
   Verdict: it does NOT.  A raw via binder cannot be compared (projection
   VC); after unpacking, structural [(=)] on the trees reflects as an
   UNINTERPRETED boolean, so no image-equality claim can be discharged --
   fail closed, even when the skeleton invariant is trivial. *)

type tree = Leaf | Node of tree * int * tree
type iset [@@vox.sort lean "ISet"]

[%%vox.lean {lean|
inductive ISet where | nil : ISet | cons : Int -> ISet -> ISet
@[grind] def triv : Vox_tree -> Prop := fun _ => True
@[grind] def elems : Vox_tree -> ISet
  | .Leaf => .nil
  | .Node l v _ => .cons v (elems l)
|lean}]

type set = tree{ triv _ } [@vox.via (elems : iset)]
[%%expect{|
type tree = Leaf | Node of tree * int * tree
type iset
type set = tree{ triv _ via (elems : iset) }
|}]

(* Claim the boolean [(=)] on unpacked trees equals IMAGE equality.
   Since [elems] is many-to-one this is false; the tree [(=)] reflects
   as uninterpreted, so it cannot be proved equal to the image eq. *)
let eq_smuggle : (a : set) -> (b : set) -> bool{ _ = (elems a = elems b) } =
  fun a b ->
    let refine_ ta = a in
    let refine_ tb = b in
    ta = tb
[%%expect{|
Line 5, characters 4-11:
5 |     ta = tb
        ^^^^^^^
Error: vox: verification failed -- NOT PROVED (automation gave up; no counterexample was found, so the property may still hold).
       Goal: *unknown1* = (elems a = elems b)
Hypotheses:
  triv tb
  elems tb = b
  triv ta
  elems ta = a
(lean: error: Application type mismatch: The argument)
|}]

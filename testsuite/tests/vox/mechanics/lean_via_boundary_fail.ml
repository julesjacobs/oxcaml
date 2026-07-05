(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* STAGE 3 fail-closed (Guard 2): honest impl PROVING across a
   [refines]-over-[via] boundary is not yet supported and must FAIL
   CLOSED, never silently mis-verify.  The interface reads the via
   binder at the IMAGE (bare [ins x s]); the manifest binder is the
   BASE tree, and the result is CONSTRUCTED from it ([Node (s, ..)]).
   One via param thus lands at BOTH sorts in the single VC
   [elems (Node (s,..)) = ins x s] -- no push can split a variable
   across sorts, so the solver rejects the ill-sorted goal.  (The
   clean fix is image-binder; see docs/plans STAGE 3 STATUS.  Until
   then, a real via abstraction is either sealed with trusted
   [assume_unchecked_] operations -- lib/via_set.ml, the declared-
   interpretation trust class -- or reasons in explicit [elems]
   vocabulary within the module.) *)

type tree = Leaf | Node of tree * int * tree
type iset [@@vox.sort lean "ISet"]

[%%vox.lean {lean|
inductive ISet where
  | nil : ISet
  | cons : Int -> ISet -> ISet

@[grind] def ins (x : Int) (s : ISet) : ISet := ISet.cons x s

@[grind] def bst : Vox_tree -> Prop
  | .Leaf => True
  | .Node l _ _ => bst l

@[grind] def elems : Vox_tree -> ISet
  | .Leaf => .nil
  | .Node l v _ => .cons v (elems l)
|lean}]

module M : sig
  type t : value refines (iset)
  val add : (x : int) -> (s : t) -> t{ _ = ins x s }
end = struct
  type t = tree{ bst _ } [@vox.via (elems : iset)]
  let add : (x : int) -> (s : t) -> t{ _ = ins x s } =
    fun x s -> (Node (s, x, Leaf) : t{ _ = ins x s })
end
[%%expect{|
type tree = Leaf | Node of tree * int * tree
type iset
Line 26, characters 16-33:
26 |     fun x s -> (Node (s, x, Leaf) : t{ _ = ins x s })
                     ^^^^^^^^^^^^^^^^^
Error: vox: verification failed (lean).
       Goal: (bst (Node (s, x, Leaf))) && ((elems (Node (s, x, Leaf))) = (ins x s))
Hypotheses:
  bst s
(lean: error: Application type mismatch: The argument)
|}]

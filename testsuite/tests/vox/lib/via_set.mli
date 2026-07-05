(* A finite set behind a via-ABSTRACTED sealed interface.  The
   representation is a tree, but the .mli hides it entirely: [t] is
   declared [refines (iset)], so a client binds [t] at the Lean set
   sort ISet and reasons in set vocabulary ([mem]/[ins]) -- the tree,
   its invariant, and the abstraction function [elems] never leave the
   unit.  The interface text never mentions [elems].  Under image-binder
   the .ml PROVES these specs honestly (no [assume_unchecked_]): a via
   binder denotes the image, and the implementation reaches the tree
   through a [refine_] unpack that  supplies the link [elems t0 = s].

   See lib/xset.mli for the EXTENSIONAL counterpart, which models [ISet]
   as [Int -> Prop] (Lean's set type): it proves set-level equalities
   such as [ins_idem] that this inductive-list model cannot, at the cost
   of the decidable [card] observable and concrete counterexamples this
   model keeps.  The two are the ends of the via modeling spectrum. *)
type iset [@@vox.sort lean "ISet"]
type t : value refines (iset)

[%%vox.lean {lean|
public inductive ISet where
  | nil : ISet
  | cons : Int -> ISet -> ISet

@[grind, expose] public def mem (x : Int) : ISet -> Prop
  | .nil => False
  | .cons y s => x = y ∨ mem x s

@[grind, expose] public def ins (x : Int) (s : ISet) : ISet := ISet.cons x s
|lean}]

val add : (x : int) -> (s : t) -> t{ _ = ins x s }
val member : (x : int) -> (s : t) -> bool{ _ = mem x s }

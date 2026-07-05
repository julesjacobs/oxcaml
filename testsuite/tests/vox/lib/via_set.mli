(* A finite set behind a via-ABSTRACTED sealed interface.  The
   representation is a tree, but the .mli hides it entirely: [t] is
   declared [refines (iset)], so a client binds [t] at the Lean set
   sort ISet and reasons in set vocabulary ([mem]/[ins]/[card]) -- the
   tree, its ordering invariant, and the abstraction function [elems]
   never leave the unit.  The interface text never mentions [elems]:
   the boundary reconciliation (typing/ctype.ml vox_flatten_view)
   relates the manifest's [via] form to this abstract [refines] claim.
   The model below is exported as the unit's VoxSig. *)
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

@[grind, expose] public def card : ISet -> Int
  | .nil => 0
  | .cons _ s => 1 + card s
|lean}]

val add : (x : int) -> (s : t) -> t{ _ = ins x s }
val member : (x : int) -> (s : t) -> bool{ _ = mem x s }
val card : (s : t) -> int{ _ = card s }

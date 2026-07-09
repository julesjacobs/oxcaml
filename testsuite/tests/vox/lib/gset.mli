(* A set whose values are modelled at a GHOST SORT: [@@vox.sort lean
   "GSet"] declares t's logical representative to be the block-defined
   Lean type GSet (TRUSTED), and this .mli block -- exported as the
   unit's VoxSig -- is where GSet and its spec functions live.  A
   client binds Gset.t at sort GSet and reasons in that vocabulary; the
   representation never leaves the unit.  Confirms a ghost-sort
   declaration travels through a sealed .cmi. *)
type t [@@vox.sort lean "GSet"]

[%%vox.lean {lean|
public inductive GSet where
  | nil : GSet
  | cons : Int -> GSet -> GSet

@[grind, expose] public def mem (x : Int) : GSet -> Prop
  | .nil => False
  | .cons y s => x = y ∨ mem x s

@[grind, expose] public def ins (x : Int) (s : GSet) : GSet := GSet.cons x s
|lean}]

val add : (x : int) -> (s : t) -> t{ _ = ins x s }
val member : (x : int) -> (s : t) -> bool{ _ = mem x s }

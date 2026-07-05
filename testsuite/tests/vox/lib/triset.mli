(* A finite-set FACE over the compiler's own Patricia trie (lib/ptrie),
   behind a via-abstracted sealed interface.  [t] is declared
   [refines (iset)], so a client binds it at the Lean set sort [ISet]
   and reasons in pure set vocabulary ([smem]/[addspec])* -- the trie,
   its Patricia invariant, and the abstraction function [elemset] never
   leave the unit.  This is the SAME signature shape as lib/via_set (a
   BST), so the toy tree and the real compiler structure are
   interchangeable behind [refines (iset)].

   *Specs are membership-based (extensional): the model [ISet] is an
   inductive list whose structural [=] is NOT set equality, so a trie
   insert's element list is not literally [sins x s].  Set equality is
   stated as agreement of [smem] at every point (see findings). *)

type iset [@@vox.sort lean "ISet"]
type t : value refines (iset)

[%%vox.lean {lean|
public inductive ISet where
  | snil : ISet
  | scons : Int -> ISet -> ISet

@[grind, expose] public def smem (x : Int) : ISet -> Prop
  | .snil => False
  | .scons y s => x = y ∨ smem x s

-- [s] is empty: nothing is a member.
@[grind, expose] public def isempty (s : ISet) : Prop := ∀ y, ¬ smem y s

-- [r] is [s] with [x] added: membership agrees pointwise.
@[grind, expose] public def addspec (r : ISet) (x : Int) (s : ISet) : Prop :=
  ∀ y, smem y r = (y = x ∨ smem y s)
|lean}]

val empty : (u : unit) -> t{ isempty _ }
val add : (x : int) -> (s : t) -> t{ addspec _ x s }
val mem : (x : int) -> (s : t) -> bool{ _ = smem x s }

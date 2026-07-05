(* A PARAMETERIZED finite set behind a via-abstracted sealed interface
   (the [via_set] demo generalized to ['a]).  The representation is a
   binary tree, but the .mli hides it entirely: ['a t] is declared
   [refines ('a iset)], so a client binds it at the parameterized Lean
   set sort [ISet a] and reasons in set vocabulary -- the tree, its
   invariant, and the abstraction function [elems] never leave the unit.

   The ghost sort ['a iset [@@vox.sort lean "ISet"]] carries an argument
   sort ([int iset] models at [(ISet Int)]).  Under image-binder the .ml
   PROVES [add] honestly (no [assume_unchecked_]) at the GENERIC level:
   the equation [elems (Node ..) = ins x s] holds at the abstract element
   sort with no decidable-equality obligation, so one proof serves every
   instantiation. *)
type 'a iset [@@vox.sort lean "ISet"]
type 'a t : value refines ('a iset)

[%%vox.lean {lean|
public inductive ISet (a : Type) where
  | nil : ISet a
  | cons : a -> ISet a -> ISet a

@[grind, expose] public def mem {a : Type} (x : a) : ISet a -> Prop
  | .nil => False
  | .cons y s => x = y ∨ mem x s

@[grind, expose] public def ins {a : Type} (x : a) (s : ISet a) : ISet a :=
  ISet.cons x s

@[grind] public theorem mem_ins {a : Type} (x : a) (s : ISet a) :
    mem x (ins x s) := by grind
|lean}]

val add : (x : 'a) -> (s : 'a t) -> 'a t{ _ = ins x s }

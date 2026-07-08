(* Vset: a via-ABSTRACT finite-set FACE over the Vset_bst BST backend
   (wave 2).  This is the stdlib's deliberate in-stdlib cross-module
   composition exhibit (blueprint R7): [t] is declared [refines (iset)], so a
   client binds it at the Lean set sort [ISet] and reasons in pure set
   vocabulary ([vs_mem]/[vs_isempty]/[vs_addspec]) -- the backend tree, its
   ordering invariant [bok], and the abstraction function [vs_elems] never
   leave this unit.  The whole point is that a downstream unit CANNOT unpack
   the backend's hidden repr, so the face is built over Vset_bst's EXPOSED
   representation and carries its laws across a bridge theorem.

   Specs are MEMBERSHIP-BASED (extensional), never structural { _ = vs_ins x s
   }: the model [ISet] is an inductive list whose structural [=] is NOT set
   equality (a tree admits many list images of one set), so set equality is
   stated as agreement of [vs_mem] at every point.  A structural spec over
   this invariant-carrying backend is satisfiable only by a degenerate
   left-spine add that ignores the backend -- the trap the blueprint §6.1
   rejects.

   Interface hygiene (blueprint §4): this block ships ONLY the model sort, the
   public set vocabulary a client computes with, and no separate axiom laws --
   the algebra (R5) is carried by the op specs [vs_isempty] (empty),
   [vs_addspec] (add), [vs_removespec] (remove), and [vs_elements_spec]
   (elements) together with the in-.ml bridge, so removing any op's spec kills
   the smoke goal.  add/remove are the closed producer algebra over the backend.  All defs are
   recursive-over / quantified-over the abstract [ISet] argument, so [expose]
   leaves them load-bearing (a client's grind cannot discharge the spec without
   them, and cannot unfold the hidden backend at all).  Names carry the [vs_]
   unit prefix; the co-travelling [b*] names come from Vset_bst.  Zero trust.

   Eliminator + relational vocab (eliminator addendum, F-2/F-3): [elements]
   enumerates the set into the stdlib's own [Vlist.t] (a second in-stdlib R7
   edge -- this face imports Vlist's [LList]/[ll_mem] alongside its own [ISet]),
   membership-bridged by [vs_elements_spec]; the relational defs [vs_subset] and
   [vs_equal] are quantified spec vocabulary a client consumes as a goal or
   hypothesis without writing its own quantifier. *)

open Vset_bst
open Vlist

type iset [@@vox.sort lean "ISet"]
type t : value refines (iset)

[%%vox.lean {lean|
public inductive ISet where
  | snil : ISet
  | scons : Int -> ISet -> ISet

@[grind, expose] public def vs_mem (x : Int) : ISet -> Prop
  | .snil => False
  | .scons y s => x = y ∨ vs_mem x s

-- [s] is empty: nothing is a member.
@[grind, expose] public def vs_isempty (s : ISet) : Prop := ∀ y, ¬ vs_mem y s

-- [r] is [s] with [x] added: membership agrees pointwise (set equality is
-- pointwise membership, since the ISet list model's structural = is not set =).
@[grind, expose] public def vs_addspec (r : ISet) (x : Int) (s : ISet) : Prop :=
  ∀ y, vs_mem y r = (y = x ∨ vs_mem y s)

-- [r] is [s] with [x] removed: membership agrees pointwise, x excluded.
@[grind, expose] public def vs_removespec (r : ISet) (x : Int) (s : ISet) : Prop :=
  ∀ y, vs_mem y r = (y ≠ x ∧ vs_mem y s)

-- [l] enumerates [s]: the Vlist image agrees, pointwise, with set membership.
-- References Vlist's imported LList/ll_mem (R7) as well as this face's ISet.
@[grind, expose] public def vs_elements_spec (l : LList) (s : ISet) : Prop :=
  ∀ x, ll_mem x l = vs_mem x s

-- Relational set vocabulary (F-3): the quantifier lives here, not in a client
-- refinement, so a client consumes vs_subset / vs_equal as a bare goal.
@[grind, expose] public def vs_subset (a b : ISet) : Prop :=
  ∀ x, vs_mem x a -> vs_mem x b
@[grind, expose] public def vs_equal (a b : ISet) : Prop :=
  ∀ x, vs_mem x a ↔ vs_mem x b
|lean}]

val empty : (u : unit) -> t{ vs_isempty _ }
val add : (x : int) -> (s : t) -> t{ vs_addspec _ x s }
val remove : (x : int) -> (s : t) -> t{ vs_removespec _ x s }
val mem : (x : int) -> (s : t) -> bool{ _ = vs_mem x s }
val elements : (s : t) -> Vlist.t{ vs_elements_spec _ s }

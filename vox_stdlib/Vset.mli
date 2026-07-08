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
   edge -- this face imports Vlist's [Vox_Vlist_t]/[ll_mem] alongside its own [ISet]),
   membership-bridged by [vs_elements_spec]; the relational defs [vs_subset] and
   [vs_equal] are quantified spec vocabulary a client consumes as a goal or
   hypothesis without writing its own quantifier. *)

open Vhof
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
-- References Vlist's imported Vox_Vlist_t/ll_mem (R7) as well as this face's ISet.
@[grind, expose] public def vs_elements_spec (l : Vox_Vlist_t) (s : ISet) : Prop :=
  ∀ x, ll_mem x l = vs_mem x s

-- Relational set vocabulary (F-3): the quantifier lives here, not in a client
-- refinement, so a client consumes vs_subset / vs_equal as a bare goal.
@[grind, expose] public def vs_subset (a b : ISet) : Prop :=
  ∀ x, vs_mem x a -> vs_mem x b
@[grind, expose] public def vs_equal (a b : ISet) : Prop :=
  ∀ x, vs_mem x a ↔ vs_mem x b

-- [r] is the singleton {x}; and [r] is the union of [a0] and [b0].  Named to
-- MATCH Vpset's ps_singletonspec / ps_unionspec (surface reconciliation): a
-- future unification binds ISet -> PSet Int and the specs line up.
@[grind, expose] public def vs_singletonspec (r : ISet) (x : Int) : Prop :=
  ∀ y, vs_mem y r = (y = x)
@[grind, expose] public def vs_unionspec (r a0 b0 : ISet) : Prop :=
  ∀ y, vs_mem y r = (vs_mem y a0 ∨ vs_mem y b0)
-- [r] is the intersection / difference of [a0] and [b0] (Vset-only; Vpset ships
-- neither -- it has no exposed backend delete to build them on).
@[grind, expose] public def vs_interspec (r a0 b0 : ISet) : Prop :=
  ∀ y, vs_mem y r = (vs_mem y a0 ∧ vs_mem y b0)
@[grind, expose] public def vs_diffspec (r a0 b0 : ISet) : Prop :=
  ∀ y, vs_mem y r = (vs_mem y a0 ∧ ¬ vs_mem y b0)

-- cardinal is the model-list length of the set image.  Recursive over the
-- abstract ISet, so [expose] leaves it load-bearing (a client's grind cannot
-- discharge [_ = vs_card s] without it).  Because the backend maintains a BST
-- (bok, no duplicates), this coincides with the true distinct-element count --
-- see notes/vset.md (cardinal distinctness caveat).
@[grind, expose] public def vs_card : ISet -> Int
  | .snil => 0
  | .scons _ s => 1 + vs_card s

-- cardinal is nonneg (the one cardinal fact a client can consume through the
-- relational op surface -- concrete counts do NOT survive the via face, since
-- add/union/singleton carry membership specs, not structural ones; see
-- notes/vset.md).  Obligation: discharged by induction in the .ml.
public axiom vs_card_nonneg (s : ISet) : vs_card s >= 0
grind_pattern vs_card_nonneg => vs_card s

-- ===== HOF KIT: fold over the set (recipe §2, via-face scalar-result case) =====
-- IntRel3 / r3Holds are imported from Vhof (the shared substrate leaf).
-- fold_left with a TERNARY element-aware step (acc, elem, acc'), over the set's
-- enumeration order (vs_elems: node then subtrees).
@[grind, expose] public def vs_relFold (r : IntRel3) : ISet -> Int -> Int -> Prop
  | .snil, init, final => init = final
  | .scons x t, init, final => exists acc, r init x acc /\ vs_relFold r t acc final
-- element sum accessor for fold's exact sum-law.
@[grind, expose] public def vs_sum : ISet -> Int
  | .snil => 0
  | .scons x t => x + vs_sum t

-- fold EXACT-output laws (.mli-only public theorems; ride VoxSig to clients).
-- These are ORDER-INDEPENDENT (sum/count are commutative), so they survive the
-- unordered set abstraction even though the exact fold VALUE does not.  Stated
-- over an abstract r with the callback's graph as a premise (never a lambda in
-- the trigger).
public theorem vs_relFold_sum_exact (r : IntRel3)
    (hr : forall a x c, r a x c -> c = a + x) :
    forall (xs : ISet) (init final : Int),
      vs_relFold r xs init final -> final = init + vs_sum xs := by
  intro xs
  induction xs with
  | snil => intro init final h; simp only [vs_relFold, vs_sum] at *; omega
  | scons x t ih =>
      intro init final h
      simp only [vs_relFold] at h
      obtain ⟨acc, hacc, hrest⟩ := h
      have h1 := hr init x acc hacc
      have h2 := ih acc final hrest
      simp only [vs_sum]; omega
grind_pattern vs_relFold_sum_exact => vs_relFold r xs init final
public theorem vs_relFold_count_exact (r : IntRel3)
    (hr : forall a x c, r a x c -> c = a + 1) :
    forall (xs : ISet) (init final : Int),
      vs_relFold r xs init final -> final = init + vs_card xs := by
  intro xs
  induction xs with
  | snil => intro init final h; simp only [vs_relFold, vs_card] at *; omega
  | scons x t ih =>
      intro init final h
      simp only [vs_relFold] at h
      obtain ⟨acc, hacc, hrest⟩ := h
      have h1 := hr init x acc hacc
      have h2 := ih acc final hrest
      simp only [vs_card]; omega
grind_pattern vs_relFold_count_exact => vs_relFold r xs init final
|lean}]

val empty : (u : unit) -> t{ vs_isempty _ }
val add : (x : int) -> (s : t) -> t{ vs_addspec _ x s }
val remove : (x : int) -> (s : t) -> t{ vs_removespec _ x s }
val mem : (x : int) -> (s : t) -> bool{ _ = vs_mem x s }
val elements : (s : t) -> Vlist.t{ vs_elements_spec _ s }

(* ===== set algebra (WP-3) ===== *)
val singleton : (x : int) -> t{ vs_singletonspec _ x }
val union : (a : t) -> (b : t) -> t{ vs_unionspec _ a b }
val inter : (a : t) -> (b : t) -> t{ vs_interspec _ a b }
val diff : (a : t) -> (b : t) -> t{ vs_diffspec _ a b }
val cardinal : (s : t) -> int{ _ = vs_card s }

(* [subset a b] is the bool QUERY: every element of [a] is a member of [b].  The
   relational [vs_subset] def (above) is the CONSUMABLE spec; this runs it. *)
val subset : (a : t) -> (b : t) -> bool{ _ = vs_subset a b }

(* [fold r f init s] folds f over the set's elements (enumeration order).  The
   exact VALUE does not survive the unordered abstraction, but the ORDER-FREE
   exact laws vs_relFold_{sum,count}_exact do (fold sum = init + sum of elts,
   count = init + cardinal).  Same HOF shape as Vlist.fold_left. *)
val fold :
  (r : ((int -> int -> int -> bool) [@vox.total])) ->
  (f : ((acc : int) -> (x : int) -> int{ r3Holds r acc x _ })) ->
  (init : int) -> (s : t) -> int{ vs_relFold r s init _ }

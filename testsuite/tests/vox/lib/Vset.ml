(* Vset implementation: the via-abstract set face over Vset_bst.  Verified
   against ONLY Vset_bst.cmi + VoxSig_Vset_bst.olean (no backend source).

   The .ml block restates the ISet model (the model-duplication tax), the
   whole-tree abstraction function [vs_elems], the union helper it uses, and
   THE bridge theorem [vs_mem_elems] equating set membership over the
   abstraction to the backend's whole-tree [bmem].  The bridge is the only new
   proof; after it, Vset_bst's [bmem_insert]/[bok_insert] carry [add]'s spec
   across with no re-proof of trees.

   The core ops CALL the real backend (Vset_bst.member / insert / remove),
   never hand-build tree constructors to fake a set -- a face that never calls
   its backend is not a face (blueprint §6.1c). add/remove are the closed
   producer algebra, both carried across the bridge by the backend's
   bmem_insert / bmem_delete (the latter under the set's bok invariant).

   [elements] (eliminator addendum, Mech A) enumerates the backend tree into a
   Vlist by composing Vlist's own empty/cons/append, so the built list's LList
   image is exactly [vs_tolist] of the tree; [vs_tolist_spec] then bridges
   ll_mem over that list to vs_mem over the abstraction. Threading the recursive
   Vlist.t (an OPAQUE cross-unit via value) through lets does NOT hit gap #31 --
   #31 is a producing-unit transparent-via phenomenon; a client composing Vlist's
   sealed ops keeps each result's refinement across a let normally (see notes). *)

open Vset_bst
open Vlist

type iset [@@vox.sort lean "ISet"]

[%%vox.lean {lean|
inductive ISet where
  | snil : ISet
  | scons : Int -> ISet -> ISet

@[grind] def vs_mem (x : Int) : ISet -> Prop
  | .snil => False
  | .scons y s => x = y ∨ vs_mem x s

@[grind] def vs_union : ISet -> ISet -> ISet
  | .snil, s => s
  | .scons x xs, s => .scons x (vs_union xs s)

-- The abstraction fn recurses into BOTH subtrees (whole-tree elems), NOT a
-- left-spine-only form that would drop the right subtree's elements and be a
-- degenerate abstraction (blueprint §6.1a). No OCaml (* *) comments inside
-- this block -- Lean parses '(' as a term and rejects it; use -- or /- -/.
@[grind] def vs_elems : Vox_Vset_bst_tree -> ISet
  | .Leaf => .snil
  | .Node l v r => .scons v (vs_union (vs_elems l) (vs_elems r))

@[grind] theorem vs_mem_union (x : Int) (a b : ISet) :
    vs_mem x (vs_union a b) = (vs_mem x a ∨ vs_mem x b) := by
  induction a <;> grind

-- THE bridge: set membership over the abstraction equals the backend's
-- whole-tree bmem, by ordinary structural induction (both sides range over
-- the whole tree). After it, Vset_bst.bmem_insert carries add's spec across
-- with no re-proof about trees.
@[grind] theorem vs_mem_elems (x : Int) (t : Vox_Vset_bst_tree) :
    vs_mem x (vs_elems t) = bmem x t := by
  induction t <;> grind
grind_pattern vs_mem_elems => vs_mem x (vs_elems t)

@[grind] def vs_isempty (s : ISet) : Prop := ∀ y, ¬ vs_mem y s
@[grind] def vs_addspec (r : ISet) (x : Int) (s : ISet) : Prop :=
  ∀ y, vs_mem y r = (y = x ∨ vs_mem y s)
@[grind] def vs_removespec (r : ISet) (x : Int) (s : ISet) : Prop :=
  ∀ y, vs_mem y r = (y ≠ x ∧ vs_mem y s)
@[grind] def vs_elements_spec (l : LList) (s : ISet) : Prop :=
  ∀ x, ll_mem x l = vs_mem x s
@[grind] def vs_subset (a b : ISet) : Prop :=
  ∀ x, vs_mem x a -> vs_mem x b
@[grind] def vs_equal (a b : ISet) : Prop :=
  ∀ x, vs_mem x a ↔ vs_mem x b

-- vs_tolist mirrors [elements]' construction ENTIRELY in Vlist's OWN vocabulary
-- (ll_nil / ll_cons / ll_app), so the Vlist value [elements] builds carries this
-- exact LList image. Both wrappers are opaque in Vlist (ll_cons, ll_nil ship
-- non-exposed), so grind cannot unfold them to .LCons / .LNil -- the mirror MUST
-- use the wrappers or the images won't match what Vlist.empty/cons/append emit.
@[grind] def vs_tolist : Vox_Vset_bst_tree -> LList
  | .Leaf => ll_nil
  | .Node l v r => ll_cons v (ll_app (vs_tolist l) (vs_tolist r))

-- The eliminator bridge: ll_mem over the built list agrees with vs_mem over the
-- set abstraction, by induction. Base case uses Vlist's ll_nil_not_mem; cons
-- step uses ll_mem_cons / ll_mem_app (all imported, ambient by grind_pattern)
-- plus vs_mem_union. No isnil-inversion lemma is needed: empty's spec is the
-- structural { _ = ll_nil }, which matches vs_tolist Leaf = ll_nil directly.
@[grind] theorem vs_tolist_spec (t : Vox_Vset_bst_tree) :
    vs_elements_spec (vs_tolist t) (vs_elems t) := by
  induction t <;> grind
grind_pattern vs_tolist_spec => vs_tolist t
|lean}]

type t = Vset_bst.set{ 0 = 0 } [@vox.via (vs_elems : iset)]

(* [empty] is the empty set; the backend Leaf denotes snil. It is a function,
   not a value: a top-level via-typed VALUE binding records a mis-sorted
   definitional fact (image name = skeleton rhs), so via values are produced
   only inside function bodies (triset findings). *)
let empty : (u : unit) -> t{ vs_isempty _ } =
  fun u -> (Vset_bst.Leaf : t{ vs_isempty _ })

(* [mem] wraps the backend's one-path search; the bridge equates its result to
   vs_mem over the abstraction. *)
let mem : (x : int) -> (s : t) -> bool{ _ = vs_mem x s } =
  fun x s ->
    let refine_ t0 = s in
    Vset_bst.member x t0

(* [add] wraps Vset_bst.insert; vs_addspec composes over the backend's
   bmem_insert via the vs_mem_elems bridge. De-contorted on origin/vox: the
   let-bound backend result is coerced DIRECTLY into the via type -- the old
   inline-ctor re-match (a triset-era workaround for opaque-skeleton
   mis-sorting) is no longer needed. *)
let add : (x : int) -> (s : t) -> t{ vs_addspec _ x s } =
  fun x s ->
    let refine_ t0 = s in
    let r = Vset_bst.insert x t0 in
    (r : t{ vs_addspec _ x s })

(* [remove] wraps Vset_bst.remove (model-level bdel), same shape as [add] over
   bins. The bridge vs_mem_elems (= bmem) carries the backend's bmem_delete
   across: bmem_delete needs bok on the input tree, which the [set] repr
   (tree{ bok _ }) supplies via refine_. De-contorted: direct coerce of the
   let-bound result (inline-ctor re-match removed, as for [add]). *)
let remove : (x : int) -> (s : t) -> t{ vs_removespec _ x s } =
  fun x s ->
    let refine_ t0 = s in
    let r = Vset_bst.remove x t0 in
    (r : t{ vs_removespec _ x s })

(* [elements] enumerates the set into a Vlist (eliminator addendum Mech A). The
   recursive helper builds the list by composing Vlist's own empty/append/cons,
   so its result image is pinned to vs_tolist of the input tree; [elements]
   then coerces that to the membership spec via vs_tolist_spec + the via image.
   The recursive Vlist.t results are let-bound (a, b) and threaded through
   Vlist.append/cons -- this does NOT hit #31 (see the module header + notes). *)
let rec vs_go (t0 : Vset_bst.tree) : Vlist.t{ _ = vs_tolist t0 } =
  match t0 with
  | Vset_bst.Leaf -> Vlist.empty ()
  | Vset_bst.Node (l, v, r) ->
    let a = vs_go l in
    let b = vs_go r in
    let ab = Vlist.append a b in
    Vlist.cons v ab

let elements : (s : t) -> Vlist.t{ vs_elements_spec _ s } =
  fun s ->
    let refine_ t0 = s in
    vs_go t0

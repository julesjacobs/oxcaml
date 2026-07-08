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

open Vhof
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

@[grind] def vs_singletonspec (r : ISet) (x : Int) : Prop :=
  ∀ y, vs_mem y r = (y = x)
@[grind] def vs_unionspec (r a0 b0 : ISet) : Prop :=
  ∀ y, vs_mem y r = (vs_mem y a0 ∨ vs_mem y b0)
@[grind] def vs_interspec (r a0 b0 : ISet) : Prop :=
  ∀ y, vs_mem y r = (vs_mem y a0 ∧ vs_mem y b0)
@[grind] def vs_diffspec (r a0 b0 : ISet) : Prop :=
  ∀ y, vs_mem y r = (vs_mem y a0 ∧ ¬ vs_mem y b0)
@[grind] def vs_card : ISet -> Int
  | .snil => 0
  | .scons _ s => 1 + vs_card s

@[grind] theorem vs_card_nonneg (s : ISet) : vs_card s >= 0 := by
  induction s <;> grind
grind_pattern vs_card_nonneg => vs_card s

-- ===== HOF KIT: fold substrate + lifts (recipe §1-2) =====
-- IntRel3 / r3Holds imported from Vhof (the shared substrate leaf).
@[grind, expose] def vs_relFold (r : IntRel3) : ISet -> Int -> Int -> Prop
  | .snil, init, final => init = final
  | .scons x t, init, final => exists acc, r init x acc /\ vs_relFold r t acc final
@[grind, expose] def vs_sum : ISet -> Int
  | .snil => 0
  | .scons x t => x + vs_sum t

-- fold distributes over the abstraction-fn's vs_union: folding scons-then-append
-- is folding the left part then the right part (private scaffolding for the tree
-- recursion, which folds node value then left subtree then right subtree).
@[grind] theorem vs_relFold_union (r : IntRel3) (a b : ISet) :
    ∀ (init final : Int),
      vs_relFold r (vs_union a b) init final
      = (∃ mid, vs_relFold r a init mid ∧ vs_relFold r b mid final) := by
  induction a <;> grind
grind_pattern vs_relFold_union => vs_relFold r (vs_union a b) init final

-- exact laws (restated so the .ml seal re-elaborates the .mli obligations --
-- these are .mli-only public theorems, but the seal demands nothing extra; the
-- restatement keeps the two blocks textually parallel for the model-dup tax).
@[grind] theorem vs_relFold_sum_exact (r : IntRel3)
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
@[grind] theorem vs_relFold_count_exact (r : IntRel3)
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

-- vs_card distributes over the abstraction-fn's vs_union (private scaffolding):
-- lets [cardinal]'s tree recursion (1 + card l + card r) match vs_card of the
-- whole-tree image.
@[grind] theorem vs_card_union (a b : ISet) :
    vs_card (vs_union a b) = vs_card a + vs_card b := by
  induction a <;> grind
grind_pattern vs_card_union => vs_card (vs_union a b)

-- ===== union over the BACKEND tree (private scaffolding) =====
-- bunion folds a's elements into acc via the backend's bins; membership is the
-- OR and bok is preserved (bins preserves both).  The face's [union] calls the
-- real Vset_bst.insert, so bunion mirrors that fold and the bridge vs_mem_elems
-- (= bmem) carries the membership OR across to vs_unionspec.
@[grind] def bunion : Vox_Vset_bst_tree -> Vox_Vset_bst_tree -> Vox_Vset_bst_tree
  | .Leaf, acc => acc
  | .Node l v r, acc => bunion r (bunion l (bins v acc))
@[grind] theorem bmem_bunion (y : Int) (a b : Vox_Vset_bst_tree) :
    bmem y (bunion a b) = (bmem y a ∨ bmem y b) := by
  induction a generalizing b <;> grind
grind_pattern bmem_bunion => bmem y (bunion a b)
@[grind] theorem bok_bunion (a b : Vox_Vset_bst_tree) (h : bok b) :
    bok (bunion a b) := by
  induction a generalizing b <;> grind
grind_pattern bok_bunion => bok (bunion a b)

-- ===== difference over the BACKEND delete (private scaffolding) =====
-- bdiff deletes every element of the second tree from the first via the
-- backend's bdel; membership is (in a AND not in b), bok is preserved.  Both
-- laws thread bok because the backend's bmem_delete / bok_delete carry a [bok]
-- hypothesis (an unbalanced BST stays ok under delete).  inter is built as
-- diff a (diff a b), so it needs no join lemmas (which are private to Vset_bst).
@[grind] def bdiff : Vox_Vset_bst_tree -> Vox_Vset_bst_tree -> Vox_Vset_bst_tree
  | a, .Leaf => a
  | a, .Node l v r => bdiff (bdiff (bdel v a) l) r
@[grind] theorem bok_bdiff (b : Vox_Vset_bst_tree) :
    ∀ (a : Vox_Vset_bst_tree), bok a -> bok (bdiff a b) := by
  induction b <;> grind
grind_pattern bok_bdiff => bok (bdiff a b)
@[grind] theorem bmem_bdiff (y : Int) (b : Vox_Vset_bst_tree) :
    ∀ (a : Vox_Vset_bst_tree), bok a -> (bmem y (bdiff a b) = (bmem y a ∧ ¬ bmem y b)) := by
  induction b <;> grind
grind_pattern bmem_bdiff => bmem y (bdiff a b)

-- ===== subset as a bool query (private scaffolding) =====
-- bsubset is the tree-level ∀; vs_subset over the abstraction reduces to it
-- through the bridge, and it decomposes on Node so the recursion's per-node VC
-- (member v b && subset l b && subset r b) closes.
@[grind] def bsubset (a b : Vox_Vset_bst_tree) : Prop := ∀ y, bmem y a -> bmem y b
@[grind] theorem vs_subset_bridge (a b : Vox_Vset_bst_tree) :
    vs_subset (vs_elems a) (vs_elems b) = bsubset a b := by
  simp only [vs_subset, bsubset, vs_mem_elems]
grind_pattern vs_subset_bridge => vs_subset (vs_elems a) (vs_elems b)
@[grind] theorem bsubset_leaf (b : Vox_Vset_bst_tree) : bsubset .Leaf b := by
  simp only [bsubset]; grind
@[grind] theorem bsubset_node (l r b : Vox_Vset_bst_tree) (v : Int) :
    bsubset (.Node l v r) b = (bmem v b ∧ bsubset l b ∧ bsubset r b) := by
  simp only [bsubset]; grind
grind_pattern bsubset_node => bsubset (.Node l v r) b

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

(* [singleton x] = insert x into the empty backend tree; membership is [y = x]
   by bmem_insert over the empty tree. *)
let singleton : (x : int) -> t{ vs_singletonspec _ x } =
  fun x ->
    let e = Vset_bst.empty in
    let r = Vset_bst.insert x e in
    (r : t{ vs_singletonspec _ x })

(* [union a b] folds a's elements into b via the backend insert (mirrors
   [bunion]); bmem_bunion carries the membership OR across the bridge. *)
let union : (a : t) -> (b : t) -> t{ vs_unionspec _ a b } =
  fun a b ->
    let refine_ ta = a in
    let refine_ tb = b in
    let rec go : (u : Vset_bst.tree) -> (acc : Vset_bst.set)
                 -> Vset_bst.set{ _ = bunion u acc } =
      fun u acc ->
        match u with
        | Vset_bst.Leaf -> (acc : Vset_bst.set{ _ = bunion u acc })
        | Vset_bst.Node (l, v, r) ->
            let acc1 = Vset_bst.insert v acc in
            let acc2 = go l acc1 in
            (go r acc2 : Vset_bst.set{ _ = bunion u acc })
    in
    let res = go ta tb in
    (res : t{ vs_unionspec _ a b })

(* bdiff_go: shared CONCRETE-level (no via) delete-fold, deleting every element
   of [u] from [acc] via the backend remove; result image pinned to [bdiff acc u].
   Both diff and inter build on it at the concrete Vset_bst.set level (a via value
   is injected only once, at each op's end) -- calling the via-PRODUCER [diff]
   from [inter] would need to re-inject a same-unit via value across a let, which
   the compiler rejects here (see notes/vset.md). *)
let rec bdiff_go (acc : Vset_bst.set) (u : Vset_bst.tree) :
    Vset_bst.set{ _ = bdiff acc u } =
  match u with
  | Vset_bst.Leaf -> (acc : Vset_bst.set{ _ = bdiff acc u })
  | Vset_bst.Node (l, v, r) ->
    let acc1 = Vset_bst.remove v acc in
    let acc2 = bdiff_go acc1 l in
    (bdiff_go acc2 r : Vset_bst.set{ _ = bdiff acc u })

(* [diff a b] deletes each element of b from a via the backend remove; bmem_bdiff
   carries (in a AND not in b) across the bridge.  The input's [bok] (from the
   set repr) discharges bmem_bdiff's / bok_bdiff's hypotheses. *)
let diff : (a : t) -> (b : t) -> t{ vs_diffspec _ a b } =
  fun a b ->
    let refine_ ta = a in
    let refine_ tb = b in
    let res = bdiff_go ta tb in
    (res : t{ vs_diffspec _ a b })

(* [inter a b] = a \ (a \ b): an intersection built from two differences, so it
   needs only the backend's exposed delete (no bjoin lemmas, which are private
   to Vset_bst).  grind derives vs_interspec by applying bmem_bdiff twice. *)
let inter : (a : t) -> (b : t) -> t{ vs_interspec _ a b } =
  fun a b ->
    let refine_ ta = a in
    let refine_ tb = b in
    let d = bdiff_go ta tb in
    let res = bdiff_go ta d in
    (res : t{ vs_interspec _ a b })

(* [cardinal] counts nodes of the whole tree; vs_card_union makes 1 + card l +
   card r match vs_card of the whole-tree image. *)
let cardinal : (s : t) -> int{ _ = vs_card s } =
  fun s ->
    let refine_ t0 = s in
    let rec go : (u : Vset_bst.tree) -> int{ _ = vs_card (vs_elems u) } =
      fun u ->
        match u with
        | Vset_bst.Leaf -> 0
        | Vset_bst.Node (l, _, r) ->
            let cl = go l in
            let cr = go r in
            1 + cl + cr
    in
    go t0

(* [subset a b] recurses a's tree, testing each element's membership in b via
   the backend's one-path search; bsubset_node decomposes the per-node goal and
   vs_subset_bridge turns the abstract vs_subset into the tree-level bsubset. *)
let subset : (a : t) -> (b : t) -> bool{ _ = vs_subset a b } =
  fun a b ->
    let refine_ ta = a in
    let refine_ tb = b in
    let rec go : (u : Vset_bst.tree) -> bool{ _ = bsubset u tb } =
      fun u ->
        match u with
        | Vset_bst.Leaf -> true
        | Vset_bst.Node (l, v, r) ->
            if Vset_bst.member v tb
            then (let sl = go l in let sr = go r in sl && sr)
            else false
    in
    let res = go ta in
    (res : bool{ _ = vs_subset a b })

(* [fold] folds over the tree in vs_elems order (node value, then left, then
   right); vs_relFold_union splits the fold-over-append at each node so the
   per-node VC closes.  Scalar result -> no via injection needed. *)
let fold :
      (r : ((int -> int -> int -> bool) [@vox.total])) ->
      (f : ((acc : int) -> (x : int) -> int{ r3Holds r acc x _ })) ->
      (init : int) -> (s : t) -> int{ vs_relFold r s init _ } =
  fun r f init s ->
    ignore r;
    let refine_ t0 = s in
    let rec go : (acc : int) -> (u : Vset_bst.tree)
                 -> int{ vs_relFold r (vs_elems u) acc _ } =
      fun acc u ->
        match u with
        | Vset_bst.Leaf -> (acc : int{ vs_relFold r (vs_elems u) acc _ })
        | Vset_bst.Node (l, v, r0) ->
            let a1 = f acc v in
            let a2 = go a1 l in
            (go a2 r0 : int{ vs_relFold r (vs_elems u) acc _ })
    in
    go init t0

(* Vpset implementation: the order-free element-polymorphic set over a cons-list
   repr, via-abstracted to [PSet].  Every shipped op is equality-FREE (stores /
   concatenates / inspects shape only), which is exactly why they prove
   generically at the opaque element sort -- see the .mli header and
   notes/vpset.md for the mem-query / remove verdict this design routes around.

   The .ml block restates the model (the model-duplication tax), adds the
   abstraction fn [ps_elems] (cons list -> PSet, structure-preserving), the append
   helper [ps_app] and its membership lemma [ps_mem_app] (union's only new proof),
   plus [ps_isnil] so [is_empty]'s VC unfolds.  The relational specs are restated
   so the op VCs can name them. *)

type 'a pset [@@vox.sort lean "PSet"]
type 'a cell = PNil | PCons of 'a * 'a cell

[%%vox.lean {lean|
inductive PSet (a : Type) where
  | pnil : PSet a
  | pcons : a -> PSet a -> PSet a

@[grind] def ps_mem {a : Type} (x : a) : PSet a -> Prop
  | .pnil => False
  | .pcons y s => x = y ∨ ps_mem x s

@[grind] def ps_isnil {a : Type} : PSet a -> Prop
  | .pnil => True
  | .pcons _ _ => False

-- Abstraction fn: structure-preserving (the repr IS a set-as-list), so no
-- decidable-equality obligation -- one proof serves every instantiation.
@[grind] def ps_elems {a : Type} : Vox_Vpset_cell a -> PSet a
  | .PNil => .pnil
  | .PCons y r => .pcons y (ps_elems r)

-- Append on the model + its membership law: union's only new proof.
@[grind] def ps_app {a : Type} : PSet a -> PSet a -> PSet a
  | .pnil, s => s
  | .pcons x xs, s => .pcons x (ps_app xs s)

@[grind] theorem ps_mem_app {a : Type} (x : a) (p q : PSet a) :
    ps_mem x (ps_app p q) = (ps_mem x p ∨ ps_mem x q) := by
  induction p <;> grind

@[grind] def ps_isempty {a : Type} (s : PSet a) : Prop := ∀ y, ¬ ps_mem y s
@[grind] def ps_addspec {a : Type} (r : PSet a) (x : a) (s : PSet a) : Prop :=
  ∀ y, ps_mem y r = (y = x ∨ ps_mem y s)
@[grind] def ps_singletonspec {a : Type} (r : PSet a) (x : a) : Prop :=
  ∀ y, ps_mem y r = (y = x)
@[grind] def ps_unionspec {a : Type} (r a0 b0 : PSet a) : Prop :=
  ∀ y, ps_mem y r = (ps_mem y a0 ∨ ps_mem y b0)
@[grind] def ps_subset {a : Type} (a0 b0 : PSet a) : Prop :=
  ∀ y, ps_mem y a0 -> ps_mem y b0
@[grind] def ps_equal {a : Type} (a0 b0 : PSet a) : Prop :=
  ∀ y, ps_mem y a0 ↔ ps_mem y b0
|lean}]

type 'a t = 'a cell{ 0 = 0 } [@vox.via (ps_elems : 'a pset)]

(* [empty] ships UNSPECCED (F-B2): a spec'd nullary via producer
   [empty : unit -> 'a t{ ps_isempty _ }] leaves the Lean datatype's type
   parameter an unsolved metavariable.  Emptiness stays observable via [is_empty]
   ([is_empty (empty ()) = true]).  A function, not a value: a top-level via-typed
   VALUE binding records a mis-sorted definitional fact (triset finding). *)
let empty : (u : unit) -> 'a t =
  fun u -> (PNil : 'a t)

(* [singleton x] = {x}: the outer [PCons (x, PNil)] pins the Lean element param,
   so unlike [empty] it carries a spec (F-B2). *)
let singleton : (x : 'a) -> 'a t{ ps_singletonspec _ x } =
  fun x -> (PCons (x, PNil) : 'a t{ ps_singletonspec _ x })

(* [is_empty] is a STRUCTURAL query (Nil vs Cons -- no element equality), so it
   ships at the opaque element.  Its spec is [ps_isnil], bridged to
   membership-emptiness by [ps_isnil_isempty]. *)
let is_empty : (s : 'a t) -> bool{ _ = ps_isnil s } =
  fun s ->
    let refine_ c0 = s in
    (match c0 with PNil -> true | PCons (h, _) -> false)

(* [add] prepends (the closed producer algebra); [ps_addspec] holds definitionally
   ([ps_mem y (pcons x s) = (y = x | ps_mem y s)]) with NO DecidableEq -- the y = x
   here is the propositional membership disjunct, not a runtime decision. *)
let add : (x : 'a) -> (s : 'a t) -> 'a t{ ps_addspec _ x s } =
  fun x s ->
    let refine_ c0 = s in
    (PCons (x, c0) : 'a t{ ps_addspec _ x s })

(* [union] appends the two repr lists; membership over the append is the OR
   ([ps_mem_app]), again equality-free.  The recursion runs over RAW cells
   carrying a structural [ps_elems] image (a plain refined skeleton, whose fact
   survives a [let] normally) rather than threading an in-unit transparent via
   value through the recursion (which would hit the #31 skeleton-map loss); only
   [union] coerces to the abstract [t] at the boundary. *)
let rec un (p : 'a cell) (q : 'a cell)
  : 'a cell{ ps_elems _ = ps_app (ps_elems p) (ps_elems q) } =
  match p with
  | PNil -> (q : 'a cell{ ps_elems _ = ps_app (ps_elems p) (ps_elems q) })
  | PCons (x, xs) ->
    let r = un xs q in
    (PCons (x, r) : 'a cell{ ps_elems _ = ps_app (ps_elems p) (ps_elems q) })

let union : (s1 : 'a t) -> (s2 : 'a t) -> 'a t{ ps_unionspec _ s1 s2 } =
  fun s1 s2 ->
    let refine_ c1 = s1 in
    let refine_ c2 = s2 in
    let c = un c1 c2 in
    (c : 'a t{ ps_unionspec _ s1 s2 })

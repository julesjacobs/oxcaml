(* Vpset: the ORDER-FREE, element-POLYMORPHIC finite set (poly study sub-problem
   C-1, the "pset shape" generalized from the reference exemplar
   testsuite/tests/vox/lib/pset).  [`'a t : refines ('a pset)`], so a client
   binds it at the parameterized Lean set sort [PSet a] and reasons in pure set
   vocabulary; the cons-list repr and the abstraction fn [ps_elems] never leave
   the unit.  ZERO trust (no [@@vox.reflect], no assumed axioms).

   THE CENTRAL PROBE (poly study F-X1 / F-C4), confirmed by build here: a set
   over an OPAQUE element parameter has NO decidable equality at its Lean sort
   (there is no [DecidableEq a] at [S_param]/VoxU).  So this module ships the
   RELATIONAL layer -- membership is a [Prop]-valued SPEC ([ps_mem]) a client
   REASONS with, not a bool QUERY it can RUN.  Two ops are therefore deliberately
   ABSENT, each blocked at a different layer (see notes/vpset.md):

     - [mem : 'a -> 'a t -> bool]  -- the bool membership QUERY.  The Prop model
       elaborates fine, but the op's proof fails: a runtime [x = y] on an
       opaque [`'a] carries NO model fact (OCaml [=] models to a decidable Lean
       [=] only at a concrete element like [int]), so the [then]-branch goal
       [true = (x = y | ...)] has no [x = y] hypothesis.  This is NOT the #32
       branch-threading gap -- the identical recursion PROVES at [int t].
     - [remove : 'a -> 'a t -> 'a t]  -- blocked EARLIER still: the model
       deletion fn ([if x = y then ...]) fails to ELABORATE ([synthInstanceFailed]
       -- no [DecidableEq a]).  The relational [ps_removespec] Prop is
       expressible, but nothing can compute the deletion generically.

   What DOES ship (every op is equality-FREE -- it only stores / concatenates /
   inspects shape): [empty] (unspecced -- F-B2: a spec'd nullary via producer
   leaves the Lean type param unsolved), [singleton], [is_empty] (a STRUCTURAL
   query -- no element equality), [add] (cons -- the closed producer algebra),
   and [union] (list append -- membership is the OR, no equality).  The algebra
   is the honest EXTENSIONAL (pointwise-membership) Vset shape, never structural
   [{ _ = ps_ins x s }] (a cons-list's structural [=] is not set equality).

   Interface hygiene (blueprint §4): the block ships the model sort, the public
   set vocabulary a client computes with (all [expose]d -- each is recursive
   ([ps_mem]) or quantified-over the abstract [PSet], so [expose] leaves it
   load-bearing), the relational F-3 defs [ps_subset]/[ps_equal], and ONE proved
   bridge law [ps_isnil_isempty] connecting the structural query [is_empty]
   answers to membership-emptiness.  ([ps_isnil] is exposed and non-recursive but
   is NOT a dead-law hazard: its only law is the bridge, stated on a symbolic [s]
   -- grind cannot unfold a variable, so it stays live regardless of [expose];
   removal-test confirmed.)  All private scaffolding (the abstraction fn,
   [ps_app], its membership lemma) stays in the .ml. *)

type 'a pset [@@vox.sort lean "PSet"]
type 'a t : value refines ('a pset)

[%%vox.lean {lean|
public inductive PSet (a : Type) where
  | pnil : PSet a
  | pcons : a -> PSet a -> PSet a

-- Membership is PROPOSITIONAL (classical [x = y], no [DecidableEq]) -- this is
-- exactly why it states membership generically but cannot be run as a query.
@[grind, expose] public def ps_mem {a : Type} (x : a) : PSet a -> Prop
  | .pnil => False
  | .pcons y s => x = y ∨ ps_mem x s

-- STRUCTURAL emptiness (what [is_empty] answers -- no element equality).
-- Non-recursive AND exposed, yet its only law -- the [ps_isnil_isempty] bridge
-- below -- stays LIVE: the bridge is stated on a SYMBOLIC [s] ([ps_isnil s ↔
-- ps_isempty s]), which grind cannot discharge by unfolding (there is no
-- constructor to reduce on a variable).  Exposure only kills a law stated on
-- CONCRETE constructors; the symbolic bridge sidesteps that trap.  (Verified:
-- deleting the bridge leaves the module sealing but breaks the smoke.)
@[grind, expose] public def ps_isnil {a : Type} : PSet a -> Prop
  | .pnil => True
  | .pcons _ _ => False

-- MEMBERSHIP emptiness (relational spec vocabulary): nothing is a member.
@[grind, expose] public def ps_isempty {a : Type} (s : PSet a) : Prop :=
  ∀ y, ¬ ps_mem y s

-- [r] is [s] with [x] added: membership agrees pointwise (set equality is
-- pointwise membership -- a cons list's structural = is not set =).
@[grind, expose] public def ps_addspec {a : Type} (r : PSet a) (x : a) (s : PSet a) : Prop :=
  ∀ y, ps_mem y r = (y = x ∨ ps_mem y s)

-- [r] is the singleton {x}.
@[grind, expose] public def ps_singletonspec {a : Type} (r : PSet a) (x : a) : Prop :=
  ∀ y, ps_mem y r = (y = x)

-- [r] is the union of [a0] and [b0].
@[grind, expose] public def ps_unionspec {a : Type} (r a0 b0 : PSet a) : Prop :=
  ∀ y, ps_mem y r = (ps_mem y a0 ∨ ps_mem y b0)

-- Relational set vocabulary (F-3): the quantifier lives here, so a client
-- consumes [ps_subset]/[ps_equal] as a bare goal or hypothesis.
@[grind, expose] public def ps_subset {a : Type} (a0 b0 : PSet a) : Prop :=
  ∀ y, ps_mem y a0 -> ps_mem y b0
@[grind, expose] public def ps_equal {a : Type} (a0 b0 : PSet a) : Prop :=
  ∀ y, ps_mem y a0 ↔ ps_mem y b0

-- THE bridge law: the structural emptiness [is_empty] answers coincides with
-- membership-emptiness.  Proved once here (the backward direction refutes the
-- ∀ by the head witness, which grind will not self-instantiate inline), shipped
-- as a grind fact so a client turns [is_empty s = true] into [∀ y, ¬ ps_mem y s].
@[grind] public theorem ps_isnil_isempty {a : Type} (s : PSet a) :
    ps_isnil s ↔ ps_isempty s := by
  cases s with
  | pnil => grind
  | pcons y s' =>
    constructor
    · intro h; exact h.elim
    · intro h; exact (h y) (Or.inl rfl)
|lean}]

val empty : (u : unit) -> 'a t
val singleton : (x : 'a) -> 'a t{ ps_singletonspec _ x }
val is_empty : (s : 'a t) -> bool{ _ = ps_isnil s }
val add : (x : 'a) -> (s : 'a t) -> 'a t{ ps_addspec _ x s }
val union : (s1 : 'a t) -> (s2 : 'a t) -> 'a t{ ps_unionspec _ s1 s2 }

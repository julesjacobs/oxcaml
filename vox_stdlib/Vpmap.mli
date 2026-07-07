(* Vpmap: the 'v-VALUED, int-keyed polymorphic association map -- the
   parameterized generalization of [Vmap] on its VALUE side.  Keys stay
   [int] (Lean-native decidable equality); values are only STORED, so
   genericity reduces to sub-problem B of the poly study (parameterized
   ghost sort + parameterized Lean model, the [Pset] mechanism), NOT to
   the DecidableEq wall.  See docs/plans/2026-07-06-vox-polymorphic-
   containers-design.md F-C1 (this module is C-tier-1, "ships now, zero
   new trust").

   The representation is a genuine CONS assoc-list (like Vmap), so [add]
   really IS a prepend (first-binding-wins, [add] shadows) and its
   structural spec [_ = m_add k v m] is faithful.  ['v t] is
   [refines ('v mlist)]: a client binds it at the parameterized model
   [MList v] (e.g. [int t] at [MList Int]) and reasons in map vocabulary.
   find results are the exposed, PARAMETERIZED ADT ['v mopt] (= the
   auto-generated [Vox_Vpmap_mopt v]); a client pattern-matches
   [MMiss]/[MFound v] at ANY value type (verified at int AND string, see
   clients/smoke_vpmap.ml).  This is the "'v-result-ADT" boundary the
   monomorphic Vmap dodged -- it WORKS (no universe/param wall); the only
   nullary gap is [empty]'s spec (F-B2, see below).

   Model ops ([m_find]/[m_add]/[m_isempty]/[m_empty]) are exported OPAQUE
   (the Vmap/oset obligation pattern), NOT unfoldable defs: [m_add] is a
   non-recursive prepend and [m_find] matches only the head, so exposed
   defs would let [grind] discharge the add-laws by unfolding -- the
   shipped algebra would be dead.  This reasoning is UNCHANGED by
   parameterization (probed: the opaque axioms keep all four laws LIVE at
   the parameterized value sort, proven by per-law deletion in the smoke).
   The .ml pays these as obligations over the concrete assoc-list defs.

   KNOWN GAP -- [empty] ships UNSPECCED (no [{ _ = m_empty }] postcond):
   a nullary via-constructor cannot carry a refinement over a
   PARAMETERIZED model, even at a concrete element type (poly study F-B2 /
   ask-#2 -- the Lean datatype's type parameter is left an unsolved
   metavariable at the via injection).  Emptiness stays OBSERVABLE via
   [is_empty] (its argument pins the value sort).  See notes/vpmap.md. *)

type 'v mlist [@@vox.sort lean "MList"]
type 'v mopt = MMiss | MFound of 'v
type 'v t : value refines ('v mlist)

[%%vox.lean {lean|
public inductive MList (v : Type) where
  | MNil : MList v
  | MCons : Int -> v -> MList v -> MList v

-- Model vocabulary, exported OPAQUE (obligation pattern): the .ml pays
-- concrete defs; clients compute only through the laws below.  Implicit
-- {v : Type} carries the parameterized value sort.
public axiom m_empty {v : Type} : MList v
public axiom m_isempty {v : Type} : MList v -> Prop
public axiom m_find {v : Type} : Int -> MList v -> Vox_Vpmap_mopt v
public axiom m_add {v : Type} : Int -> v -> MList v -> MList v

-- remove's postcondition, a quantified spec def kept @[expose] (the F-2
-- exemption: the forall is the point, a client INSTANTIATES it at a key).
@[grind, expose] public def m_remove_spec {v : Type}
    (r : MList v) (k : Int) (m : MList v) : Prop :=
  ∀ k', m_find k' r = (if k' = k then .MMiss else m_find k' m)

-- The algebra clients reason with (all four LIVE under opaque model ops).
public axiom m_isempty_empty {v : Type} : m_isempty (m_empty : MList v)
grind_pattern m_isempty_empty => m_isempty (m_empty : MList v)

public axiom m_find_empty {v : Type} (k : Int) :
    m_find k (m_empty : MList v) = .MMiss
grind_pattern m_find_empty => m_find k (m_empty : MList v)

public axiom m_find_add_eq {v : Type} (k : Int) (w : v) (m : MList v) :
    m_find k (m_add k w m) = .MFound w
grind_pattern m_find_add_eq => m_find k (m_add k w m)

public axiom m_find_add_ne {v : Type} (k k' : Int) (w : v) (m : MList v)
    (h : k ≠ k') :
    m_find k (m_add k' w m) = m_find k m
grind_pattern m_find_add_ne => m_find k (m_add k' w m)
|lean}]

val empty : (u : unit) -> 'v t
val is_empty : (m : 'v t) -> bool{ _ = m_isempty m }
val find : (k : int) -> (m : 'v t) -> 'v mopt{ _ = m_find k m }
val add : (k : int) -> (w : 'v) -> (m : 'v t) -> 'v t{ _ = m_add k w m }
val remove : (k : int) -> (m : 'v t) -> 'v t{ m_remove_spec _ k m }

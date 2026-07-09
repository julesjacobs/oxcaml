(* Vmap_make: a verified finite MAP as an ORD FUNCTOR over an
   int-representable, ordered KEY type -- the [Map.Make(ORD)] shape.  This
   is the headline of the functor productization: an idiomatic ML map whose
   ordering is supplied by a comparator whose contract carries the
   total-order content, and whose instantiation obligation is REAL (a
   lawful [IntOrd] seals green; a sign-flipped comparator is DISPROVED --
   clients/smoke_vmap_make.ml + the cross-unit demo).

   ADDITIVE to the flat [Vmap]: SAME op names and STRUCTURAL spec shapes
   ([empty]/[find]/[add]/[mem]/[singleton] over [m_empty]/[m_find]/[m_add]/
   [m_haskey]), so a future unification is mechanical.  Two deliberate
   differences from [Vmap], both documented in notes/vmap_make.md:
   - keys are the ORDERED functor argument [O.t] (int-representable), not a
     bare [int];
   - [add] has OVERWRITE semantics (a re-add of a present key REPLACES its
     value), the idiomatic Map.add -- vs [Vmap]'s prepend-shadow assoc-list.
     This is faithful because the model is a characteristic function.

   THE MODEL (crisp boundary).  A map is a CHARACTERISTIC FUNCTION
   [MMap := Int -> MOpt] (MOpt = the exposed result ADT [mopt]).  Its
   funext-equality IS map equality, so [add] carries a STRUCTURAL
   postcondition [_ = m_add k v m] (no membership-agreement quantifier, no
   "many reps of one map" caveat).  find results are the exposed ADT [mopt]
   ([MNone]/[MSome v]); a client pattern-matches (verified in the smoke).
   The block ships only the client-facing map vocabulary; the tree and its
   abstraction [mmap] are .ml-only (they name [Vox_Vmap_make_tree], which
   only exists inside the functor -- blocks are unit-level).

   [remove] is DEFERRED: an ordered-BST delete (min-extraction / re-link)
   is materially more proof than the insert/lookup core and is not needed
   for the "Map with an ordered compare" headline; see notes/vmap_make.md
   for the crisp reason.  Zero trust: every op is proved through a
   [refine_] unpack, zero [assume_unchecked_]. *)

type mmap [@@vox.sort lean "MMap"]
type mopt = MNone | MSome of int

module type ORD = sig
  type t [@@vox.sort int]
  val compare : (x : t) -> (y : t)
    -> int{ (_ < 0 -> x < y) && (_ = 0 -> x = y) && (_ > 0 -> y < x) }
end

[%%vox.lean {lean|
-- The exported MAP model: a total function from key to result-option.
-- funext-equality IS map equality, so op specs are STRUCTURAL (_ = m_add ...).
-- MMap's sort is transparent (a char-function), but the model OPS are
-- exported OPAQUE (the Vmap/oset obligation pattern): m_add is a
-- non-recursive point-update and m_find is one application, so exposed defs
-- would let a client's grind discharge the add-laws by beta-unfolding and
-- the shipped algebra would be dead.  Opaque ops keep the three laws LIVE:
-- they are the only path a client computes over m_find/m_add.  The .ml pays
-- them as obligations over the concrete char-function defs.
public abbrev MMap := Int -> Vox_Vmap_make_mopt
public axiom m_find : Int -> MMap -> Vox_Vmap_make_mopt
public axiom m_empty : MMap
public axiom m_add : Int -> Int -> MMap -> MMap
-- key-presence predicate (parity with Vmap.m_haskey); the consumable spec
-- for the bool [mem] query.  Exposed (a bridge predicate, kills no law).
@[grind, expose] public def m_haskey (k : Int) (m : MMap) : Prop :=
  m_find k m ≠ .MNone

-- The algebra clients reason with (all three LIVE under opaque model ops).
public axiom m_find_empty (k : Int) : m_find k m_empty = .MNone
grind_pattern m_find_empty => m_find k m_empty
public axiom m_find_add_eq (k v : Int) (m : MMap) :
    m_find k (m_add k v m) = .MSome v
grind_pattern m_find_add_eq => m_find k (m_add k v m)
public axiom m_find_add_ne (k k' v : Int) (m : MMap) (h : k ≠ k') :
    m_find k (m_add k' v m) = m_find k m
grind_pattern m_find_add_ne => m_find k (m_add k' v m)
|lean}]

module type MAP = sig
  type key
  type t : value refines (mmap)
  val empty : (u : unit) -> t{ _ = m_empty }
  val find : (k : key) -> (m : t) -> mopt{ _ = m_find k m }
  val add : (k : key) -> (v : int) -> (m : t) -> t{ _ = m_add k v m }
  val mem : (k : key) -> (m : t) -> bool{ _ = m_haskey k m }
  (* [singleton k v] is the one-binding map (add over empty). *)
  val singleton : (k : key) -> (v : int) -> t{ _ = m_add k v m_empty }
end

module Make : functor (O : ORD) -> MAP with type key = O.t

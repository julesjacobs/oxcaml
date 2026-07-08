(* Vmap: a verified association map with int keys and int values, behind
   a via-ABSTRACTED interface.  The representation is a genuine CONS
   assoc-list (a real prepend list, like Vlist -- NOT an ordered tree),
   so [add] really IS a prepend and its structural spec [_ = m_add k v m]
   is faithful (soundness M-2): first binding wins, [add] shadows.

   [t] is [refines (mlist)], so a client binds [t] at the Lean model
   [MList] and reasons in map vocabulary.  find results are the exposed
   ADT [mopt] (= [MOpt]); a client pattern-matches [MMiss]/[MFound v].

   The model ops ([m_find]/[m_add]/[m_isempty]/[m_empty]) are exported as
   OPAQUE axioms (the oset obligation pattern), NOT unfoldable defs.  This
   is deliberate: [m_add] is a non-recursive prepend and [m_find] matches
   only the head, so exposed defs would let [grind] discharge the add-laws
   by unfolding -- the shipped algebra would be dead.  Opaque model ops
   keep the four laws LIVE: they are the only way a client computes over
   [m_find]/[m_add], so removing any breaks a client (see notes/vmap.md).
   The .ml pays these as obligations over the concrete assoc-list defs. *)

open Vlist

type mlist [@@vox.sort lean "MList"]
type mopt = MMiss | MFound of int
type t : value refines (mlist)

[%%vox.lean {lean|
public inductive MList where
  | MNil : MList
  | MCons : Int -> Int -> MList -> MList

-- Model vocabulary, exported OPAQUE (obligation pattern): the .ml pays
-- concrete defs; clients compute only through the laws below.
public axiom m_empty : MList
public axiom m_isempty : MList -> Prop
public axiom m_find : Int -> MList -> Vox_Vmap_mopt
public axiom m_add : Int -> Int -> MList -> MList

-- Relational / eliminator vocabulary (F-2/F-3): quantified spec defs. These
-- stay @[expose] (exempt from the de-expose rule): the ∀ is the whole point,
-- so a client consumes them by INSTANTIATING at a point, never by writing its
-- own quantifier. `remove`'s postcondition and `m_agree` are stated here.
@[grind, expose] public def m_remove_spec (r : MList) (k : Int) (m : MList) : Prop :=
  ∀ k', m_find k' r = (if k' = k then .MMiss else m_find k' m)

@[grind, expose] public def m_agree (a : MList) (b : MList) : Prop :=
  ∀ k, m_find k a = m_find k b

-- keys eliminator (Mech A): enumerate the map's KEYS into a Vlist, bridging
-- Vlist membership (imported ll_mem) to key-presence in the map.  m_haskey is
-- the point predicate; m_keys_spec is the ∀-agreement the client consumes.
@[grind, expose] public def m_haskey (k : Int) (m : MList) : Prop :=
  m_find k m ≠ .MMiss
@[grind, expose] public def m_keys_spec (l : LList) (m : MList) : Prop :=
  ∀ k, ll_mem k l = m_haskey k m

-- union is LEFT/a-biased (a's binding wins where a has the key, else b's), the
-- shadowing-consistent choice: it is exactly list append (add already prepends
-- shadow), so [find] of the union is [find a] falling back to [find b].  Stated
-- via [m_find] so it composes with the shipped find-laws.  (Right-biased would
-- reverse the append but break the "first binding wins" story that [add]/[find]
-- establish -- see notes/vmap.md union-bias.)
@[grind, expose] public def m_unionspec (r a b : MList) : Prop :=
  ∀ k, m_find k r = (match m_find k a with | .MMiss => m_find k b | x => x)

-- The algebra clients reason with (all four LIVE under opaque model ops).
public axiom m_isempty_empty : m_isempty m_empty
grind_pattern m_isempty_empty => m_isempty m_empty

public axiom m_find_empty (k : Int) : m_find k m_empty = .MMiss
grind_pattern m_find_empty => m_find k m_empty

public axiom m_find_add_eq (k v : Int) (m : MList) :
    m_find k (m_add k v m) = .MFound v
grind_pattern m_find_add_eq => m_find k (m_add k v m)

public axiom m_find_add_ne (k k' v : Int) (m : MList) (h : k ≠ k') :
    m_find k (m_add k' v m) = m_find k m
grind_pattern m_find_add_ne => m_find k (m_add k' v m)
|lean}]

val empty : (u : unit) -> t{ _ = m_empty }
val is_empty : (m : t) -> bool{ _ = m_isempty m }
val find : (k : int) -> (m : t) -> mopt{ _ = m_find k m }
val add : (k : int) -> (v : int) -> (m : t) -> t{ _ = m_add k v m }
val remove : (k : int) -> (m : t) -> t{ m_remove_spec _ k m }
val keys : (m : t) -> Vlist.t{ m_keys_spec _ m }

(* ===== map surface (WP-3) ===== *)
(* [mem] is the bool key-presence QUERY (m_haskey is the consumable spec). *)
val mem : (k : int) -> (m : t) -> bool{ _ = m_haskey k m }
(* [singleton k v] is the one-binding map (add over empty). *)
val singleton : (k : int) -> (v : int) -> t{ _ = m_add k v m_empty }
(* [union a b] is the a-biased merge (= list append). *)
val union : (a : t) -> (b : t) -> t{ m_unionspec _ a b }

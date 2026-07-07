(* Per-module SMOKE client (dead-law check, blueprint §6.7): forces each
   of Vmap's four shipped laws to fire.  Because the model ops
   ([m_find]/[m_add]/[m_isempty]/[m_empty]) are exported OPAQUE, none of
   these goals can be discharged by unfolding -- each needs exactly its
   law, so removing any law from vmap.mli breaks the corresponding goal
   (verified by deletion, §6.6).

   The find goals are stated as model EQUATIONS between two opaque
   [m_find] applications (never naming the [mopt] constructors, which the
   refinement grammar rejects -- see notes/vmap.md): the only way to prove
   the two sides equal is for the relevant law to rewrite each to the same
   normal form.  Verified against Vmap.cmi + VoxSig_Vmap.olean. *)

open Vmap

(* forces m_isempty_empty : m_isempty m_empty *)
let empty_is_empty : bool{ _ = true } =
  let e = Vmap.empty () in
  Vmap.is_empty e

(* forces m_find_empty : find on empty is key-independent (both -> MMiss) *)
let find_empty_eq (k1 : int) (k2 : int) : mopt{ _ = m_find k2 m_empty } =
  let e = Vmap.empty () in
  Vmap.find k1 e

(* forces m_find_add_eq : find of the just-added key is map-independent
   (both -> MFound v) *)
let find_added_eq (k : int) (v : int) (m1 : Vmap.t) (m2 : Vmap.t) :
    mopt{ _ = m_find k (m_add k v m2) } =
  let a = Vmap.add k v m1 in
  Vmap.find k a

(* forces m_find_add_ne : find sees through a shadowing add of a different
   key.  Ground distinct keys (1, 2): grind discharges 1 <> 2, then the
   law carries find through the add. *)
let find_added_other (v : int) (m : Vmap.t) : mopt{ _ = m_find 1 m } =
  let a = Vmap.add 2 v m in
  Vmap.find 1 a

(* forces m_remove_spec at k' = k: the removed key misses (compare to empty).
   The ∀ lives in m_remove_spec (F-3): the client writes no quantifier, it
   just instantiates the shipped def at the point k. *)
let removed_key_gone (k : int) (m : Vmap.t) : mopt{ _ = m_find k m_empty } =
  let r = Vmap.remove k m in
  Vmap.find k r

(* forces m_remove_spec at k' <> k (ground 1, 2): remove sees through *)
let remove_sees_through (m : Vmap.t) : mopt{ _ = m_find 1 m } =
  let r = Vmap.remove 2 m in
  Vmap.find 1 r

(* forces m_agree (F-3): consumed as a hypothesis on b's refinement and
   instantiated at the point k -- no client-side quantifier. *)
let agree_point (a : Vmap.t) (b : Vmap.t{ m_agree a _ }) (k : int) :
    mopt{ _ = m_find k a } =
  Vmap.find k b

(* forces m_keys_spec (Mech A eliminator): read a key back out of the
   enumerated key-list; membership in Vmap.keys agrees with key-presence.
   The ∀ lives in m_keys_spec; the client instantiates at the point k via
   Vlist's own ll_mem, composing two modules' vocab (Vmap keys + Vlist mem). *)
let key_enumerated (k : int) (m : Vmap.t) : bool{ _ = m_haskey k m } =
  let ks = Vmap.keys m in
  Vlist.mem k ks

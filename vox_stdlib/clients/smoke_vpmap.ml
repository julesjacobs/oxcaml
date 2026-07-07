(* Per-module SMOKE client for Vpmap (dead-law check, blueprint §6.7),
   exercised at BOTH int-valued AND string-valued maps to witness the
   parameterized value sort.  Because the model ops are exported OPAQUE,
   none of these goals is dischargeable by unfolding -- each needs exactly
   its law, so removing a law from Vpmap.mli breaks the corresponding goal
   (verified by deletion).  find goals are stated as model EQUATIONS
   between two opaque [m_find] applications (never naming the exposed
   [mopt] constructors, which the refinement grammar rejects), so the only
   way to prove the two sides equal is for the relevant law to normalize
   each to the same form.  Verified against Vpmap.cmi + VoxSig_Vpmap.olean.

   NOTE (empty gap): [m_isempty_empty] is NOT forced here -- with
   [empty] unspecced (F-B2), no op yields an [m_empty]-specced value, so
   that law is client-unreachable (see notes/vpmap.md).  [m_find_empty] IS
   reached, via the remove-vs-empty comparison whose RHS names [m_empty]
   with [v] pinned by the return ADT type.

   Post-#53 (finding C1): find/add have EQUATIONAL result contracts so their
   results inline into a dependent parameter (C1 let-binds removed); remove has
   a RELATIONAL contract (m_remove_spec) so its result is STILL let-bound (the
   relational-contract boundary -- see LANGUAGE_NEEDS). *)

open Vpmap

(* ===================== int-valued instantiation ===================== *)

(* forces m_find_add_eq : find of the just-added key is map-independent
   (both -> MFound w) *)
let find_added_eq_int (k : int) (w : int) (m1 : int t) (m2 : int t) :
    int mopt{ _ = m_find k (m_add k w m2) } =
  Vpmap.find k (Vpmap.add k w m1)

(* forces m_find_add_ne : find sees through a shadowing add of a different
   key (ground distinct keys 1, 2) *)
let find_added_other_int (w : int) (m : int t) : int mopt{ _ = m_find 1 m } =
  Vpmap.find 1 (Vpmap.add 2 w m)

(* forces m_remove_spec at k' = k AND m_find_empty : removed key misses,
   compared to empty (RHS m_find k m_empty, v = int pinned by return type). *)
let removed_key_gone_int (k : int) (m : int t) : int mopt{ _ = m_find k m_empty } =
  let r = Vpmap.remove k m in
  Vpmap.find k r

(* forces m_remove_spec at k' <> k (ground 1, 2): remove sees through *)
let remove_sees_through_int (m : int t) : int mopt{ _ = m_find 1 m } =
  let r = Vpmap.remove 2 m in
  Vpmap.find 1 r

(* ==================== string-valued instantiation ==================== *)

let find_added_eq_str (k : int) (w : string) (m1 : string t) (m2 : string t) :
    string mopt{ _ = m_find k (m_add k w m2) } =
  Vpmap.find k (Vpmap.add k w m1)

let find_added_other_str (w : string) (m : string t) :
    string mopt{ _ = m_find 1 m } =
  Vpmap.find 1 (Vpmap.add 2 w m)

let removed_key_gone_str (k : int) (m : string t) :
    string mopt{ _ = m_find k m_empty } =
  let r = Vpmap.remove k m in
  Vpmap.find k r

let remove_sees_through_str (m : string t) : string mopt{ _ = m_find 1 m } =
  let r = Vpmap.remove 2 m in
  Vpmap.find 1 r

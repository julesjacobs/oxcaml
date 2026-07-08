(* CROSS-UNIT smoke for Vmap_make: instantiates the ORD functor [Make] with
   a real int order [IntOrd] in THIS separate unit (the F-1 stamp-collision
   case -- the element-mentioning [compare] contract crosses the .cmi), then
   proves map facts THROUGH the sealed abstraction with no view of the tree.
   Verified against Vmap_make.cmi + VoxSig_Vmap_make.olean.

   IntOrd.compare discharges ORD's ordered contract HONESTLY (against Int's
   <); the sign-flipped counterpart is DISPROVED in smoke_vmap_make_bad.ml.

   Model ops are OPAQUE, so each goal is closed only by the relevant law:
   find goals are stated as model EQUATIONS between two applications (never
   naming the exposed [mopt] constructors, which the refinement grammar
   rejects); mem goals use bool{ _ = true/false }.  Remove any law from the
   .mli and exactly its goal breaks (dead-law sweep, blueprint §6.7). *)

open Vmap_make

module IntOrd = struct
  type t = int
  let compare : (x : int) -> (y : int)
      -> int{ (_ < 0 -> x < y) && (_ = 0 -> x = y) && (_ > 0 -> y < x) } =
    fun x y -> if x < y then -1 else if x = y then 0 else 1
end

module M = Make (IntOrd)

(* forces m_find_add_eq: find of the just-added key is map-independent (both
   normalize to MSome v).  m1, m2 distinct so the two sides are equal ONLY
   through the law, not structurally. *)
let find_added_eq (k : int) (v : int) (m1 : M.t) (m2 : M.t) :
    mopt{ _ = m_find k (m_add k v m2) } =
  let m' = M.add k v m1 in
  M.find k m'

(* forces m_find_add_ne (ground distinct keys 1, 2): find sees through a
   shadowing add of a different key. *)
let find_added_other (v : int) (m : M.t) : mopt{ _ = m_find 1 m } =
  let m' = M.add 2 v m in
  M.find 1 m'

(* forces empty's structural spec (empty () yields m_empty); the goal is
   reflexive once that spec fires.  m_find_empty itself is forced by
   mem_other_empty below (find k m_empty -> MNone -> not present). *)
let find_empty (k : int) : mopt{ _ = m_find k m_empty } =
  let e = M.empty () in
  M.find k e

(* forces mem + m_find_add_eq via m_haskey: the just-added key is present *)
let mem_after_add (k : int) (v : int) (m : M.t) : bool{ _ = true } =
  let m' = M.add k v m in
  M.mem k m'

(* forces m_find_add_ne + m_find_empty via m_haskey: a key never added and
   different from the added one is absent (ground 1, 2). *)
let mem_other_empty (v : int) : bool{ _ = false } =
  let e = M.empty () in
  let m' = M.add 2 v e in
  M.mem 1 m'

(* forces singleton's spec + m_find_add_eq: the singleton's key is present *)
let mem_singleton (k : int) (v : int) : bool{ _ = true } =
  let s = M.singleton k v in
  M.mem k s

(* CROSS-UNIT smoke for Vset_make: instantiates the ORD functor [Make] with
   a real int order [IntOrd] in THIS separate unit, then proves set facts
   THROUGH the sealed abstraction (char-function ISet), no view of the tree.
   Verified against Vset_make.cmi + VoxSig_Vset_make.olean.  The bad-order
   instance is DISPROVED in smoke_vset_make_bad.ml.

   Model ops are OPAQUE, so each membership goal is closed only by the
   relevant law (bool{ _ = true/false }).  Remove any law from the .mli and
   exactly its goal breaks (dead-law sweep, blueprint §6.7). *)

open Vset_make

module IntOrd = struct
  type t = int
  let compare : (x : int) -> (y : int)
      -> int{ (_ < 0 -> x < y) && (_ = 0 -> x = y) && (_ > 0 -> y < x) } =
    fun x y -> if x < y then -1 else if x = y then 0 else 1
end

module S = Make (IntOrd)

(* forces add's structural spec + mem_s_ins: the just-added element is a
   member. *)
let mem_after_add (x : int) (s : S.t) : bool{ _ = true } =
  let s' = S.add x s in
  S.mem x s'

(* forces mem_s_empty: nothing is a member of the empty set. *)
let mem_empty (x : int) : bool{ _ = false } =
  let e = S.empty () in
  S.mem x e

(* forces mem_s_ins_ne + mem_s_empty (ground 1, 2): an element never added
   and different from the added one is absent. *)
let mem_other_empty : bool{ _ = false } =
  let e = S.empty () in
  let s' = S.add 2 e in
  S.mem 1 s'

(* forces singleton's spec + mem_s_ins: exactly x is a member of {x}. *)
let mem_singleton (x : int) : bool{ _ = true } =
  let s = S.singleton x in
  S.mem x s

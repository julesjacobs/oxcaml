(* SMOKE for Vpmap_ord (the parameter-style poly-key map fallback), at int
   keys.  The comparator + its reflected key relation [e] are supplied
   INLINE at each [mem] call (their contract [(_ = 0) = eqHolds e x y] is
   checked against mem's expected type with [e] instantiated) -- the SAME
   comparator a client would hand Vmap_make.Make, threaded as a value.
   Verified against Vpmap_ord.cmi + VoxSig_Vpmap_ord.olean (+ Vhof).

   Both presence laws are forced: mp_haskeyr_cons by every goal;
   mp_haskeyr_nil by mem_absent_singleton (whose base is mp_nil, reached via
   singleton's spec).  NOTE (F-B2 gap): a mem on a BARE [empty ()] cannot be
   proven false -- empty is unspecced, so the model does not know its value
   is mp_nil; emptiness is reached only THROUGH singleton (see
   notes/vpmap_ord.md). *)

open Vpmap_ord

(* forces mp_haskeyr_cons at a reflexive key match: the just-added key is
   present (m arbitrary -- no nil needed). *)
let mem_after_add (k : int) (v : int) (m : int t) : bool{ _ = true } =
  let m' = add k v m in
  mem (fun a b -> a = b)
    (fun x y -> if x < y then -1 else if x = y then 0 else 1) k m'

(* forces singleton's structural spec + mp_haskeyr_cons: the singleton's key
   is present. *)
let mem_singleton (k : int) (v : int) : bool{ _ = true } =
  let s = singleton k v in
  mem (fun a b -> a = b)
    (fun x y -> if x < y then -1 else if x = y then 0 else 1) k s

(* forces mp_haskeyr_cons (non-match) + mp_haskeyr_nil (ground 1, 2): a key
   different from the singleton's key is absent. *)
let mem_absent_singleton (v : int) : bool{ _ = false } =
  let s = singleton 2 v in
  mem (fun a b -> a = b)
    (fun x y -> if x < y then -1 else if x = y then 0 else 1) 1 s

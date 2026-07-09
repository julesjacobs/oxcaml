(* Viarray: the immutable-array graduation of ia_lib.

   [int iarray] modelled by the BUILT-IN iarray theory: [Iarray.length a]
   and [a.(i)] reflect to the opaque Vox_ia_len / Vox_ia_get, whose sole
   compiler-owned fact is length nonnegativity.  [length]/[get]/[unsafe_get]
   need NO block and author NO algebra -- they are discharged directly by
   reflection.

   The WP-4 additions (for_all / any / mem) are the queryable surface the opaque
   theory DOES admit: a scalar-returning read-only index loop, specified by a
   quantified window predicate over [Vox_ia_get].  The window predicates are
   [@grind, expose] SPEC defs (a bounded forall / exists) -- sanctioned exposed
   vocabulary (blueprint 6.7 excludes forall/exists spec defs): grind discharges
   each loop's step/base obligation by UNFOLDING the window, so no separate step
   law is needed (an earlier draft shipped step/done laws; the removal test
   proved them dead, so they were dropped -- notes/viarray.md).  No stores are
   needed (the array is read-only), so no unique modes appear.

   BOUNDARY (L10 / N2, notes/viarray.md): the opaque theory has Vox_ia_get and
   Vox_ia_len but NO constructor, so any op that BUILDS an array (map / sub /
   append / of_list / fill / blit) cannot state its result's elements or length
   -- those stay unshippable here and are recorded as findings.  The theory is
   also [int iarray]-only (no ['a iarray], no mutable [int array]). *)

open Vhof

[%%vox.lean {lean|
-- for_all: every index in the window [i, n) satisfies p.
@[grind, expose] public def ia_all_from (p : IntPred) (a : VoxIA) (i n : Int) : Prop :=
  forall k, i <= k -> k < n -> pHolds p (Vox_ia_get a k)
-- any: some index in [i, n) satisfies p.
@[grind, expose] public def ia_ex_from (p : IntPred) (a : VoxIA) (i n : Int) : Prop :=
  exists k, i <= k /\ k < n /\ pHolds p (Vox_ia_get a k)
-- mem: some index in [i, n) holds value x.
@[grind, expose] public def ia_mem_from (x : Int) (a : VoxIA) (i n : Int) : Prop :=
  exists k, i <= k /\ k < n /\ Vox_ia_get a k = x
|lean}]

val length : (a : int iarray) -> int{ _ = Iarray.length a }

val get : (a : int iarray) -> (i : int{ 0 <= _ && _ < Iarray.length a })
          -> int{ _ = a.(i) }

val unsafe_get : (a : int iarray)
                 -> (i : int{ 0 <= _ && _ < Iarray.length a }) -> int

(* ===== WP-4: read-only scalar queries (loop over the opaque theory) ===== *)
val for_all :
  (p : ((int -> bool) [@vox.total])) ->
  (test : ((x : int) -> bool{ _ = pHolds p x })) ->
  (a : int iarray) -> bool{ _ = ia_all_from p a 0 (Iarray.length a) }

val any :
  (p : ((int -> bool) [@vox.total])) ->
  (test : ((x : int) -> bool{ _ = pHolds p x })) ->
  (a : int iarray) -> bool{ _ = ia_ex_from p a 0 (Iarray.length a) }

val mem :
  (x : int) -> (a : int iarray) -> bool{ _ = ia_mem_from x a 0 (Iarray.length a) }

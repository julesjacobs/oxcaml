(* Viarray: the immutable-array graduation of ia_lib.

   [int iarray] modelled by the BUILT-IN iarray theory: [Iarray.length a]
   and [a.(i)] reflect directly into predicates, so this module needs NO
   [%%vox.lean] block and authors NO algebra.  The theory's sole
   compiler-owned fact is length nonnegativity, so [length]'s result is
   provably >= 0 for free.

   The bounds contract rides the interface: every caller of [get] proves
   0 <= i < Iarray.length a and NOTHING is assumed.  [get]'s result
   refinement is discharged by the built-in theory's reflection;
   [unsafe_get] deletes the per-access runtime check because the contract
   already did the checking, statically. *)

val length : (a : int iarray) -> int{ _ = Iarray.length a }

val get : (a : int iarray) -> (i : int{ 0 <= _ && _ < Iarray.length a })
          -> int{ _ = a.(i) }

val unsafe_get : (a : int iarray)
                 -> (i : int{ 0 <= _ && _ < Iarray.length a }) -> int

(* Verified array access: the bounds contract rides the interface --
   every caller proves 0 <= i < Iarray.length a -- and NOTHING is
   assumed.  [get]'s result refinement is discharged by the built-in
   theory's reflection; [unsafe_get] deletes the per-access runtime
   check because the contract already did the checking, statically. *)

val get : (a : int iarray) -> (i : int{ 0 <= _ && _ < Iarray.length a })
          -> int{ _ = a.(i) }

val unsafe_get : (a : int iarray)
                 -> (i : int{ 0 <= _ && _ < Iarray.length a }) -> int

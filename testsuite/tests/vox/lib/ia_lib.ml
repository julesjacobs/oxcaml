(* See ia_lib.mli: both reads are PROVED against the built-in iarray
   theory; no assumption anywhere. *)

let get : (a : int iarray) -> (i : int{ 0 <= _ && _ < Iarray.length a })
          -> int{ _ = a.(i) } =
  fun a i -> Iarray.get a i

let unsafe_get : (a : int iarray)
                 -> (i : int{ 0 <= _ && _ < Iarray.length a }) -> int =
  fun a i -> Iarray.unsafe_get a i

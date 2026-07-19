(* A branch and its negation each become a fact.

   In the [then] branch the verifier knows [x >= 0]; in the [else]
   branch it knows the negation, so [0 - x] is nonnegative too.  Either
   way the result meets the annotation [int{ _ >= 0 }]. *)

let abs (x : int) =
  (if x >= 0 then x else 0 - x : int{ _ >= 0 })

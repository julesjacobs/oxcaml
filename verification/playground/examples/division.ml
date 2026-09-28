(* Int.Refined.( / ) requires a divisor that is provably nonzero. Here the
   divisor may be 0, and the counterexample says so. The note points at the
   refinement in the standard library that is violated. *)

let (percent @ total) (part : int) (whole : {w : int | w >= 0}) : int =
  Int.Refined.(100 * part / whole)

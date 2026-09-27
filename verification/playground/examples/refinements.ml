(* A refinement type {x : int | p} is an int for which p holds. Vox proves
   every such claim at compile time with Z3; nothing is checked at run time.

   [clamp] promises a result between [lo] and [hi], but only when the caller
   passes [hi >= lo]: the type of [hi] mentions [lo]. [bounded] relies on
   that promise. Try passing [clamp 10 0 x] in [bounded]. *)

let (clamp @ total) (lo : int) (hi : {h : int | lo <= h}) (x : int) :
    {r : int | lo <= r && r <= hi} =
  if x < lo then lo else if x > hi then hi else x

let bounded (x : int) : {r : int | 0 <= r && r <= 10} = clamp 0 10 x

(* Integer division needs a nonzero divisor: Int.Refined.( / ) takes
   {d : int | d <> 0}, so a positive count is enough. *)
let (average @ total) (sum : int) (count : {c : int | c > 0}) : int =
  Int.Refined.(sum / count)

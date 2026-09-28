(* TEST
 has-z3;
 flags = "-extension refinement_types";
 { expect; }
*)

module Total = struct
  let (midpoint @ total) (lo : {lo : int | lo >= 0})
      (hi : {hi : int | lo <= hi}) : {m : int | lo <= m && m <= hi} =
    lo + Int.Refined.((hi - lo) / 2)

  let (last_digit @ total) (x : {x : int | x >= 0}) :
      {d : int | 0 <= d && d < 10} =
    Int.Refined.(x mod 10)

  let (quotient @ total) (x : int) (y : {y : int | y <> 0}) =
    Int.Refined.div x y, Int.Refined.rem x y
end;;
[%%expect{|
module Total :
  sig
    val midpoint :
      (lo : {lo : int | lo >= 0}) ->
      (hi : {hi : int | lo <= hi}) -> {m : int | (lo <= m) && (m <= hi)}
    val last_digit : {x : int | x >= 0} -> {d : int | (0 <= d) && (d < 10)}
    val quotient : int -> {y : int | y <> 0} -> int * int
  end
|}]

(* The divisor must be proved nonzero. *)
module Rejected = struct
  let (divide @ total) (x : int) (y : int) = Int.Refined.(x / y)
end;;
[%%expect{|
Line 2, characters 62-63:
2 |   let (divide @ total) (x : int) (y : int) = Int.Refined.(x / y)
                                                                  ^
Error: Refinement could not be proved (counterexample: y = 0)
File "int.mli", line 65, characters 37-43:
  The refinement is stated here.
|}]

let runtime = Total.midpoint 3 10, Total.last_digit 1234,
  Total.quotient (-7) 2;;
[%%expect{|
val runtime : int * int * (int * int) = (6, 4, (-3, -1))
|}]

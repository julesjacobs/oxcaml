(* TEST
 has-z3;
 flags = "-extension refinement_types";
 expect;
*)

(* A parameter annotation may mention an earlier parameter, whatever the
   result annotation. *)

let h (a : int) (b : {v : int | v > a}) : int = b;;
[%%expect{|
val h : (a : int) -> {v : int | v > a} -> int = <fun>
|}]

let h2 (a : int) (t : {v : unit | a > 0}) : {r : int | r > 0} = a;;
[%%expect{|
val h2 : (a : int) -> {v : unit | a > 0} -> {r : int | r > 0} = <fun>
|}]

let h3 (a : int) (b : int) (c : {v : int | v > a + b}) = c;;
[%%expect{|
val h3 : (a : int) -> (b : int) -> {v : int | v > (a + b)} -> int = <fun>
|}]

let use () = h 1 2 + h2 1 () + h3 1 2 4;;
[%%expect{|
val use : unit -> int = <fun>
|}]

let bad () = h 3 2;;
[%%expect{|
Line 1, characters 17-18:
1 | let bad () = h 3 2;;
                     ^
Error: Refinement could not be proved (counterexample)
Line 1, characters 32-37:
1 | let h (a : int) (b : {v : int | v > a}) : int = b;;
                                    ^^^^^
  The refinement is stated here.
|}]

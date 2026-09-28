(* TEST
 has-z3;
 flags = "-extension refinement_types";
 { expect; }
*)

(* Talk, section 1 ("the language in 150 seconds") and claim 6 ("the logic
   matches the machine"), the native verdicts behind the playground
   examples; the playground's own files are checked in talk-playground/.
   1. [x + 1 > x] fails at max_int: [int] is exact 63-bit wrapping
      arithmetic in the logic (the investigation's smt-encoding E1).
   2. A failed proof is not always a real counterexample: variable-by-
      variable multiplication is uninterpreted, so the true [x * x >= 0]
      is rejected with a countermodel of the abstraction, [x = 0]
      (smt-encoding E2).
   3. Totality is not purity, but reading an ordinary mutable reference is
      partial, so a total function cannot do it. *)

let incr_pos (x : {v : int | v >= 0}) : {r : int | r > x} = x + 1;;
[%%expect{|
Line 1, characters 60-65:
1 | let incr_pos (x : {v : int | v >= 0}) : {r : int | r > x} = x + 1;;
                                                                ^^^^^
Error: Refinement could not be proved (counterexample: x = 4611686018427387903)
Line 1, characters 51-56:
1 | let incr_pos (x : {v : int | v >= 0}) : {r : int | r > x} = x + 1;;
                                                       ^^^^^
  The refinement is stated here.
|}]

let incr_below_max (x : {v : int | v >= 0 && v < max_int}) : {r : int | r > x} =
  x + 1;;
[%%expect{|
val incr_below_max :
  (x : {v : int | (v >= 0) && (v < max_int)}) -> {r : int | r > x} = <fun>
|}]

let square (x : {v : int | 0 <= v && v < 1000}) : {r : int | r >= 0} = x * x;;
[%%expect{|
Line 1, characters 71-76:
1 | let square (x : {v : int | 0 <= v && v < 1000}) : {r : int | r >= 0} = x * x;;
                                                                           ^^^^^
Error: Refinement could not be proved (countermodel for abstract multiplication: x = 0)
Line 1, characters 61-67:
1 | let square (x : {v : int | 0 <= v && v < 1000}) : {r : int | r >= 0} = x * x;;
                                                                 ^^^^^^
  The refinement is stated here.
|}]

let counter = ref 0;;
[%%expect{|
val counter : int ref = {contents = 0}
|}]

let (read_counter @ total) () = !counter;;
[%%expect{|
Line 1, characters 32-33:
1 | let (read_counter @ total) () = !counter;;
                                    ^
Error: The value "(!)" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 27-40
         which is expected to be "total".
|}]

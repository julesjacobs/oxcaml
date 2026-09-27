(* TEST
 has-z3;
 flags = "-extension refinement_types";
 { expect; }
*)

let bounds () : {u : unit | max_int > 0 && min_int < 0
    && max_int + 1 = min_int && Int.max_int = max_int
    && Int.min_int = min_int} = ();;
[%%expect{|
val bounds :
  unit ->
  {u : unit
    | (max_int > 0) &&
        ((min_int < 0) &&
           (((max_int + 1) = min_int) &&
              ((Int.max_int = max_int) && (Int.min_int = min_int))))} =
  <fun>
|}]

let (succ_pred @ total) (x : int) : {y : int | y = x + 1} =
  pred (succ (succ x));;
[%%expect{|
val succ_pred : (x : int) -> {y : int | y = (x + 1)} = <fun>
|}]

let (magnitude @ total) (x : int) :
    {y : int | (x >= 0 && y = x) || (x < 0 && y = - x)} = abs x;;
[%%expect{|
val magnitude :
  (x : int) -> {y : int | ((x >= 0) && (y = x)) || ((x < 0) && (y = (- x)))} =
  <fun>
|}]

let abs_min () : {u : unit | abs min_int = min_int && abs (-3) = 3} = ();;
[%%expect{|
val abs_min :
  unit -> {u : unit | ((abs min_int) = min_int) && ((abs (-3)) = 3)} = <fun>
|}]

let (scaled @ total) (x : {x : int | 0 <= x && x < 1024}) :
    {y : int | y = x * 8} = x lsl 3;;
[%%expect{|
val scaled :
  (x : {x : int | (0 <= x) && (x < 1024)}) -> {y : int | y = (x * 8)} = <fun>
|}]

let (halved @ total) (x : {x : int | x >= 0}) : {y : int | y = x lsr 1} =
  x asr 1;;
[%%expect{|
val halved : (x : {x : int | x >= 0}) -> {y : int | y = (x lsr 1)} = <fun>
|}]

let negative_shift () : {u : unit | (-8) asr 2 = -2 && (-1) asr 62 = -1
    && 1 lsl 62 = min_int} = ();;
[%%expect{|
val negative_shift :
  unit ->
  {u : unit
    | (((-8) asr 2) = (-2)) &&
        ((((-1) asr 62) = (-1)) && ((1 lsl 62) = min_int))} =
  <fun>
|}]

(* Outside [0, 63] the shifted value is unspecified. *)
let unspecified (x : int) : {u : unit | 1 lsl 64 = 0} = ();;
[%%expect{|
Line 1, characters 56-58:
1 | let unspecified (x : int) : {u : unit | 1 lsl 64 = 0} = ();;
                                                            ^^
Error: Refinement could not be proved (counterexample)
Line 1, characters 40-52:
1 | let unspecified (x : int) : {u : unit | 1 lsl 64 = 0} = ();;
                                            ^^^^^^^^^^^^
  The refinement is stated here.
|}]

let wrong_abs (x : int) : {u : unit | abs x >= 0} = ();;
[%%expect{|
Line 1, characters 52-54:
1 | let wrong_abs (x : int) : {u : unit | abs x >= 0} = ();;
                                                        ^^
Error: Refinement could not be proved (counterexample: x = -4611686018427387904)
Line 1, characters 38-48:
1 | let wrong_abs (x : int) : {u : unit | abs x >= 0} = ();;
                                          ^^^^^^^^^^
  The refinement is stated here.
|}]

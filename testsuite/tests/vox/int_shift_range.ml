(* TEST
 has-z3;
 flags = "-extension refinement_types";
 { expect; }
*)

(* OCaml leaves a shift by a count outside [0, 63] unspecified, and compiled
   code differs: native code folds [1 lsl 64] to 0, while arm64 computes
   [1 lsl n] with [n = 64] as 1. Every shift in verified code must therefore
   have a count in range. *)

let same (n : {n : int | n = 64}) : {b : bool | b} = (1 lsl n) = (1 lsl 64);;
[%%expect{|
Line 1, characters 72-74:
1 | let same (n : {n : int | n = 64}) : {b : bool | b} = (1 lsl n) = (1 lsl 64);;
                                                                            ^^
Error: Refinement could not be proved (counterexample: n = 64)
Line 1, characters 65-75:
1 | let same (n : {n : int | n = 64}) : {b : bool | b} = (1 lsl n) = (1 lsl 64);;
                                                                     ^^^^^^^^^^
  A shift count must be between 0 and 63: outside that range OCaml leaves the result unspecified.
|}]

let same_lsr (n : {n : int | n = 64}) : {b : bool | b} =
  ((-1) lsr n) = ((-1) lsr 64);;
[%%expect{|
Line 2, characters 27-29:
2 |   ((-1) lsr n) = ((-1) lsr 64);;
                               ^^
Error: Refinement could not be proved (counterexample: n = 64)
Line 2, characters 17-30:
2 |   ((-1) lsr n) = ((-1) lsr 64);;
                     ^^^^^^^^^^^^^
  A shift count must be between 0 and 63: outside that range OCaml leaves the result unspecified.
|}]

let same_asr (n : {n : int | n = 64}) : {b : bool | b} =
  (min_int asr n) = (min_int asr 64);;
[%%expect{|
Line 2, characters 33-35:
2 |   (min_int asr n) = (min_int asr 64);;
                                     ^^
Error: Refinement could not be proved (counterexample: n = 64)
Line 2, characters 20-36:
2 |   (min_int asr n) = (min_int asr 64);;
                        ^^^^^^^^^^^^^^^^
  A shift count must be between 0 and 63: outside that range OCaml leaves the result unspecified.
|}]

let negative (x : int) : {r : int | true} = x asr (-1);;
[%%expect{|
Line 1, characters 50-54:
1 | let negative (x : int) : {r : int | true} = x asr (-1);;
                                                      ^^^^
Error: Refinement could not be proved (counterexample)
Line 1, characters 44-54:
1 | let negative (x : int) : {r : int | true} = x asr (-1);;
                                                ^^^^^^^^^^
  A shift count must be between 0 and 63: outside that range OCaml leaves the result unspecified.
|}]

(* A count proved in range is accepted, at the bounds too. *)
let bit (i : {i : int | 0 <= i && i < 62}) : {m : int | m > 0} = 1 lsl i;;
[%%expect{|
val bit : {i : int | (0 <= i) && (i < 62)} -> {m : int | m > 0} = <fun>
|}]

let bounds (x : int) : {r : int | true} =
  (x lsl 0) lxor (x lsr 63) lxor (x asr 63);;
[%%expect{|
val bounds : int -> {r : int | true} = <fun>
|}]

let low_byte (x : int) (k : {k : int | 0 <= k && k < 8}) :
    {b : int | 0 <= b && b < 256} =
  (x lsr (8 * k)) land 255;;
[%%expect{|
val low_byte :
  int -> {k : int | (0 <= k) && (k < 8)} -> {b : int | (0 <= b) && (b < 256)} =
  <fun>
|}]

(* The standard shifts are not total, since their result for other counts
   can differ between evaluations. A total function or a predicate uses the
   shifts of [Int.Refined], whose count is refined. *)
let (scaled @ total) (x : int) = x lsl 3;;
[%%expect{|
Line 1, characters 35-38:
1 | let (scaled @ total) (x : int) = x lsl 3;;
                                       ^^^
Error: The value "\#lsl" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 21-40
         which is expected to be "total".
|}]

let (scaled @ total) (x : {x : int | 0 <= x && x < 1024}) :
    {y : int | y = x * 8} = Int.Refined.(x lsl 3);;
[%%expect{|
val scaled :
  (x : {x : int | (0 <= x) && (x < 1024)}) -> {y : int | y = (x * 8)} = <fun>
|}]

let (unchecked @ total) (x : int) (n : int) = Int.Refined.(x lsl n);;
[%%expect{|
Line 1, characters 65-66:
1 | let (unchecked @ total) (x : int) (n : int) = Int.Refined.(x lsl n);;
                                                                     ^
Error: Refinement could not be proved (counterexample: n = -1)
File "int.mli", line 73, characters 39-45:
  The refinement is stated here.
|}]

module Named = struct
  let (left @ total) (x : int) (n : {n : int | 0 <= n && n <= 63}) =
    Int.Refined.shift_left x n

  let (right @ total) (x : int) (n : {n : int | 0 <= n && n <= 63}) =
    Int.Refined.shift_right x n

  let (logical @ total) (x : int) (n : {n : int | 0 <= n && n <= 63}) =
    Int.Refined.shift_right_logical x n

  let values (n : {n : int | 0 <= n && n <= 63}) :
      {u : unit | n <> 4 || (Int.Refined.shift_left 3 n = 48
        && Int.Refined.shift_right (-40) n = -3
        && Int.Refined.shift_right_logical (-1) n = 576460752303423487
        && Int.Refined.( lsl ) 3 n = 48 && Int.Refined.( asr ) (-40) n = -3
        && Int.Refined.( lsr ) (-1) n = 576460752303423487)} = ()
end;;
[%%expect{|
module Named :
  sig
    val left : int -> {n : int | (0 <= n) && (n <= 63)} -> int
    val right : int -> {n : int | (0 <= n) && (n <= 63)} -> int
    val logical : int -> {n : int | (0 <= n) && (n <= 63)} -> int
    val values :
      (n : {n : int | (0 <= n) && (n <= 63)}) ->
      {u : unit
        | (n <> 4) ||
            (((Int.Refined.shift_left 3 n) = 48) &&
               (((Int.Refined.shift_right (-40) n) = (-3)) &&
                  (((Int.Refined.shift_right_logical (-1) n) =
                      576460752303423487)
                     &&
                     (((Int.Refined.(lsl) 3 n) = 48) &&
                        (((Int.Refined.(asr) (-40) n) = (-3)) &&
                           ((Int.Refined.(lsr) (-1) n) = 576460752303423487))))))}
  end
|}]

(* A total function's calls are equal for equal arguments; the counts it
   shifts by are in range, so compiled code agrees. *)
let (power @ total) (n : {n : int | 0 <= n && n <= 63}) = Int.Refined.(1 lsl n)

let calls (n : {n : int | n = 62}) : {b : bool | b} = power n = power 62;;
[%%expect{|
val power : {n : int | (0 <= n) && (n <= 63)} -> int = <fun>
val calls : {n : int | n = 62} -> {b : bool | b} = <fun>
|}]

(* A function that shifts by an unchecked count is partial, so equal calls
   need not give equal results. *)
let unchecked_power (n : int) = 1 lsl n

let unchecked_calls (n : {n : int | n = 64}) : {b : bool | b} =
  unchecked_power n = unchecked_power 64;;
[%%expect{|
val unchecked_power : int -> int = <fun>
Line 4, characters 2-40:
4 |   unchecked_power n = unchecked_power 64;;
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample: n = 64)
Line 3, characters 59-60:
3 | let unchecked_calls (n : {n : int | n = 64}) : {b : bool | b} =
                                                               ^
  The refinement is stated here.
|}]

(* A partially applied shift checks its count where it is applied. *)
let partial (n : int) : {r : int | true} =
  let shift = ( lsl ) 1 in
  shift n;;
[%%expect{|
Line 3, characters 8-9:
3 |   shift n;;
            ^
Error: Refinement could not be proved (counterexample: n = -1)
Line 3, characters 2-9:
3 |   shift n;;
      ^^^^^^^
  A shift count must be between 0 and 63: outside that range OCaml leaves the result unspecified.
|}]

(* Shifts by a variable count still use the abstraction of bitwise
   operations when the exact query is hard. *)
let (halve_by @ total) (x : {x : int | x >= 0})
    (k : {k : int | 0 <= k && k <= 63}) : {r : int | 0 <= r && r <= x} =
  Int.Refined.(x lsr k);;
[%%expect{|
val halve_by :
  (x : {x : int | x >= 0}) ->
  {k : int | (0 <= k) && (k <= 63)} -> {r : int | (0 <= r) && (r <= x)} =
  <fun>
|}]

let runtime = bit 5, low_byte 0x1234 1, Named.left 3 2, Named.right (-8) 1,
  Named.logical 16 4, power 10;;
[%%expect{|
val runtime : int * int * int * int * int * int = (32, 18, 12, -4, 1, 1024)
|}]

(* A total, stateless function is a function of its arguments even in a unit
   that is never verified, so a shift may be declared total only with its
   count refined to [0, 63]. *)
external shift : int -> int -> int @@ total = "%lslint";;
[%%expect{|
Line 1, characters 0-55:
1 | external shift : int -> int -> int @@ total = "%lslint";;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: A shift declared total must refine its count to [0, 63], as in Int.Refined: {n : int | 0 <= n && n <= 63}. OCaml leaves other counts unspecified, and their results differ between evaluations.
|}]

external shift : int -> {n : int | n <= 63} -> int @@ total = "%lslint";;
[%%expect{|
Line 1, characters 0-71:
1 | external shift : int -> {n : int | n <= 63} -> int @@ total = "%lslint";;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: A shift declared total must refine its count to [0, 63], as in Int.Refined: {n : int | 0 <= n && n <= 63}. OCaml leaves other counts unspecified, and their results differ between evaluations.
|}]

external shift : int -> {n : int | 0 <= n && n < 64} -> int @@ total
  = "%lslint";;
[%%expect{|
external shift : int -> {n : int | (0 <= n) && (n < 64)} -> int = "%lslint"
|}]

external shift : int -> int -> int = "%lslint";;
[%%expect{|
external shift : int -> int -> int = "%lslint"
|}]

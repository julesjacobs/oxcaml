(* TEST
 has-z3;
 flags = "-extension refinement_types";
 { expect; }
 { expect.opt; }
 { flags += " -principal"; expect; }
 { flags += " -principal"; expect.opt; }
*)

let (clamp @ total) (lo : int) (hi : {h : int | lo <= h}) (x : int) :
    {r : int | lo <= r && r <= hi} =
  if x < lo then lo else if x > hi then hi else x

let bounded (x : int) : {r : int | 0 <= r && r <= 10} =
  clamp 0 10 x
;;
[%%expect{|
val clamp :
  (lo : int) ->
  (hi : {h : int | lo <= h}) -> int -> {r : int | (lo <= r) && (r <= hi)} =
  <fun>
val bounded : int -> {r : int | (0 <= r) && (r <= 10)} = <fun>
|}]

let () = List.iter (fun x -> Format.printf "%d@." (bounded x)) [-3; 4; 12]
;;
[%%expect{|
0
4
10
|}]

let reversed x = clamp 10 0 x
;;
[%%expect{|
Line 1, characters 26-27:
1 | let reversed x = clamp 10 0 x
                              ^
Error: Refinement could not be proved (counterexample)
|}]

let wrong (x : int) : {r : int | 0 <= r && r <= 10} = x
;;
[%%expect{|
Line 1, characters 54-55:
1 | let wrong (x : int) : {r : int | 0 <= r && r <= 10} = x
                                                          ^
Error: Refinement could not be proved (counterexample)
|}]

let opaque_clamp lo hi x =
  if x < lo then lo else if x > hi then hi else x
;;
[%%expect{|
val opaque_clamp : 'a -> 'a -> 'a -> 'a = <fun>
|}]

let opaque_result (x : int) : {r : int | 0 <= r && r <= 10} =
  opaque_clamp 0 10 x
;;
[%%expect{|
Line 2, characters 2-21:
2 |   opaque_clamp 0 10 x
      ^^^^^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

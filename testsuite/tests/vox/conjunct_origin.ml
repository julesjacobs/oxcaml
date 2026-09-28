(* TEST
 has-z3;
 flags = "-extension refinement_types";
 { expect; }
*)

(* The origin note names the conjunct that could not be proved. *)
let f (x : int) : {r : int | r > 0 && r < 100 && r <> 50} = x + 1;;
[%%expect{|
Line 1, characters 60-65:
1 | let f (x : int) : {r : int | r > 0 && r < 100 && r <> 50} = x + 1;;
                                                                ^^^^^
Error: Refinement could not be proved (counterexample: x = -1)
Line 1, characters 29-34:
1 | let f (x : int) : {r : int | r > 0 && r < 100 && r <> 50} = x + 1;;
                                 ^^^^^
  The refinement is stated here.
|}]

let g (x : {x : int | x > 0}) : {r : int | r > 0 && r < 100 && r <> 50} =
  x + 1;;
[%%expect{|
Line 2, characters 2-7:
2 |   x + 1;;
      ^^^^^
Error: Refinement could not be proved (counterexample: x = 4611686018427387903)
Line 1, characters 43-48:
1 | let g (x : {x : int | x > 0}) : {r : int | r > 0 && r < 100 && r <> 50} =
                                               ^^^^^
  The refinement is stated here.
|}]

let h (x : {x : int | x > 0 && x < 99}) :
    {r : int | r > 0 && r < 100 && r <> 50} = x + 1;;
[%%expect{|
Line 2, characters 46-51:
2 |     {r : int | r > 0 && r < 100 && r <> 50} = x + 1;;
                                                  ^^^^^
Error: Refinement could not be proved (counterexample: x = 49)
Line 2, characters 35-42:
2 |     {r : int | r > 0 && r < 100 && r <> 50} = x + 1;;
                                       ^^^^^^^
  The refinement is stated here.
|}]

(* A goal inside a conjunction is proved under the earlier conjuncts. *)
let later (x : int) (y : {y : int | y > 0}) :
    {r : int | r >= 0 && (if r > 0 then y > 0 else true) && r <= max_int} =
  if x > 0 then x else 0;;
[%%expect{|
val later :
  int ->
  (y : {y : int | y > 0}) ->
  {r : int | (r >= 0) && ((if r > 0 then y > 0 else true) && (r <= max_int))} =
  <fun>
|}]

let ok (x : {x : int | x > 0 && x < 49}) :
    {r : int | r > 0 && r < 100 && r <> 50} = x + 1;;
[%%expect{|
val ok :
  {x : int | (x > 0) && (x < 49)} ->
  {r : int | (r > 0) && ((r < 100) && (r <> 50))} = <fun>
|}]

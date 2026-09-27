(* TEST
 has-z3;
 flags = "-extension refinement_types";
 { expect; }
*)

(* [assert false] raises, so the path after it is dead. *)
let positive (x : int) : {y : int | y > 0} =
  if x > 0 then x else assert false;;
[%%expect{|
val positive : int -> {y : int | y > 0} = <fun>
|}]

let matched (x : int option) : {n : int | n >= 0} =
  match x with Some n when n >= 0 -> n | _ -> assert false;;
[%%expect{|
val matched : int option -> {n : int | n >= 0} = <fun>
|}]

(* With assertions enabled, the condition holds after [assert e]. *)
let after_assert (x : int) : {y : int | y > 5} =
  assert (x > 5);
  x;;
[%%expect{|
val after_assert : int -> {y : int | y > 5} = <fun>
|}]

(* Obligations in the condition are still checked. *)
let checked_condition (a : int iarray) (i : int) =
  assert (Iarray.Refined.get a i > 0);;
[%%expect{|
Line 2, characters 31-32:
2 |   assert (Iarray.Refined.get a i > 0);;
                                   ^
Error: Refinement could not be proved (counterexample)
File "iarray.mli", line 59, characters 15-37:
  The refinement is stated here.
|}]

(* A weaker assertion gives only its own fact. *)
let weaker (x : int) : {y : int | y > 5} =
  assert (x > 5 || x = 0);
  x;;
[%%expect{|
Line 3, characters 2-3:
3 |   x;;
      ^
Error: Refinement could not be proved (counterexample: x = 0)
Line 1, characters 34-39:
1 | let weaker (x : int) : {y : int | y > 5} =
                                      ^^^^^
  The refinement is stated here.
|}]

(* [assert false] still raises, so it is partial. *)
module Total = struct
  let (f @ total) (x : int) = if x > 0 then x else assert false
end;;
[%%expect{|
Line 2, characters 51-63:
2 |   let (f @ total) (x : int) = if x > 0 then x else assert false
                                                       ^^^^^^^^^^^^
Error: The expression is "partial"
       but is expected to be "total"
         because it is used inside the function at line 2, characters 18-63
         which is expected to be "total".
|}]

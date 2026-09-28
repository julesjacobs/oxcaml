(* TEST
 has-z3;
 flags = "-extension refinement_types -noassert";
 { expect; }
*)

(* [assert false] raises even under [-noassert]. *)
let positive (x : int) : {y : int | y > 0} =
  if x > 0 then x else assert false;;
[%%expect{|
val positive : int -> {y : int | y > 0} = <fun>
|}]

(* Under [-noassert] the check [assert e] is removed, so [e] is not
   assumed afterwards. *)
let after_assert (x : int) : {y : int | y > 5} =
  assert (x > 5);
  x;;
[%%expect{|
Line 3, characters 2-3:
3 |   x;;
      ^
Error: Refinement could not be proved (counterexample: x = 0)
Line 1, characters 40-45:
1 | let after_assert (x : int) : {y : int | y > 5} =
                                            ^^^^^
  The refinement is stated here.
|}]

(* TEST
 has-z3;
 flags = "-extension refinement_types";
 expect;
*)

let impossible x : {n : int | false} = refine_ x;;
[%%expect{|
Line 1, characters 39-48:
1 | let impossible x : {n : int | false} = refine_ x;;
                                           ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
Line 1, characters 30-35:
1 | let impossible x : {n : int | false} = refine_ x;;
                                  ^^^^^
  The refinement is stated here.
|}]

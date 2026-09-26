(* TEST
 has-z3;
 flags = "-extension refinement_types";
 expect;
*)

let (impossible @ total) (_ : {u : unit | false}) : int =
  unreachable_ ();;
[%%expect{|
val impossible : {u : unit | false} -> int = <fun>
|}]

let (branch @ total) (x : {x : int | x > 0}) =
  if x > 0 then x else unreachable_ ();;
[%%expect{|
val branch : {x : int | x > 0} -> int = <fun>
|}]

let (bad @ total) () : int = unreachable_ ();;
[%%expect{|
Line 1, characters 29-44:
1 | let (bad @ total) () : int = unreachable_ ();;
                                 ^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let bad_partial () : int = unreachable_ ();;
[%%expect{|
Line 1, characters 27-42:
1 | let bad_partial () : int = unreachable_ ();;
                               ^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let (bad_branch @ total) (x : int) =
  if x > 0 then x else unreachable_ ();;
[%%expect{|
Line 2, characters 23-38:
2 |   if x > 0 then x else unreachable_ ();;
                           ^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let (ghost_impossible @ total) (_ : {u : unit | false}) =
  ghost_ (unreachable_ () : int);;
[%%expect{|
val ghost_impossible : {u : unit | false} -> int @ ghost = <fun>
|}]

let (product @ total) (_ : {u : unit | false}) : #(int * bool) =
  unreachable_ ();;
[%%expect{|
val product : {u : unit | false} -> #(int * bool) = <fun>
|}]

module Shadow = struct
  type unit = Shadow
  let (impossible @ total) (_ : {x : int | false}) : int = unreachable_ ()
end;;
[%%expect{|
module Shadow :
  sig type unit = Shadow val impossible : {x : int | false} -> int end
|}]

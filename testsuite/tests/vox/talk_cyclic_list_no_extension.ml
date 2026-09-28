(* TEST
 { expect; }
*)

(* Talk, section 9 ("the ask") and the hard question "what breaks in plain
   OxCaml?": the totality axis is active even without
   [-extension refinement_types]. [list] is [@@inductive], so a cyclic list
   is rejected, the price of sound datatype acyclicity. This file is
   compiled without the extension. *)

let rec ones = 1 :: ones;;
[%%expect{|
Line 1, characters 15-24:
1 | let rec ones = 1 :: ones;;
                   ^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of "let rec"
|}]

(* A cyclic value of a type that is not declared inductive is still
   accepted. *)
type stream = Cons of int * stream

let rec ones' = Cons (1, ones');;
[%%expect{|
type stream = Cons of int * stream
val ones' : stream = Cons (1, <cycle>)
|}]

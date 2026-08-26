(* TEST
 has-z3;
 flags = "-extension refinement_types";
 expect;
*)

external ( >= ) : int -> int -> bool @@ total = "%greaterequal"
external ( = ) : int -> int -> bool @@ total = "%equal";;
[%%expect{|
external ( >= ) : int -> int -> bool = "%greaterequal"
external ( = ) : int -> int -> bool = "%equal"
|}]

type nonnegative = {n : int | n >= 0};;
[%%expect{|
type nonnegative = {n : int | n >= 0}
|}]

let wrapped : nonnegative = let input = 42 in refine_ input
let payload = let refine_ n = wrapped in n;;
[%%expect{|
val wrapped : nonnegative = 42
val payload : int = 42
|}]

let implicit_payload = wrapped + 1;;
[%%expect{|
Line 1, characters 23-30:
1 | let implicit_payload = wrapped + 1;;
                           ^^^^^^^
Error: The value "wrapped" has type "nonnegative" = "{n : int | n >= 0}"
       but an expression was expected of type "int"
|}]

let unknown x = refine_ x;;
[%%expect{|
Line 1, characters 16-25:
1 | let unknown x = refine_ x;;
                    ^^^^^^^^^
Error: "refine_" requires a known refinement type from its context
|}]

let escapes =
  let bound = 42 in
  let result : {n : int | n = bound} = refine_ bound in
  result;;
[%%expect{|
Lines 2-4, characters 2-8:
2 | ..let bound = 42 in
3 |   let result : {n : int | n = bound} = refine_ bound in
4 |   result..
Error: the refinement type of this expression escapes the scope of binding "bound"
|}]

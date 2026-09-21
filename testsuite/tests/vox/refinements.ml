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
val implicit_payload : int = 43
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
val escapes : int = 42
|}]

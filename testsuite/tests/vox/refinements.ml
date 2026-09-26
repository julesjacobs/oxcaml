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

(* Implicit elimination only removes the predicate; the base type must still
   match. *)
let mismatched_payload = wrapped ^ "!";;
[%%expect{|
Line 1, characters 25-32:
1 | let mismatched_payload = wrapped ^ "!";;
                             ^^^^^^^
Error: The value "wrapped" has type "int" but an expression was expected of type
         "string"
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

(* A refinement cannot be dropped from a function type, so a function whose
   result predicate mentions a local binding still escapes its scope. *)
let escapes_function =
  let bound = 42 in
  let add_bound : (x : int) -> {n : int | n = x + bound} =
    fun x -> x + bound in
  add_bound;;
[%%expect{|
Line 5, characters 2-11:
5 |   add_bound;;
      ^^^^^^^^^
Error: the refinement type of this expression escapes the scope of binding "bound"
|}]

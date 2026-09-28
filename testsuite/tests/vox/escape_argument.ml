(* TEST
 has-z3;
 flags = "-extension refinement_types";
 expect;
*)

(* When a refinement mentions an argument that is not a variable, the escape
   error names the parameter and points at the argument. *)

external ( >= ) : int -> int -> bool @@ total = "%greaterequal"
external ( = ) : int -> int -> bool @@ total = "%equal"
let identity : (x : int) -> {y : int | y = x} = fun x -> x
let follow : (x : int) -> {y : int | y >= x} -> int = fun x y -> y;;
[%%expect{|
external ( >= ) : int -> int -> bool = "%greaterequal"
external ( = ) : int -> int -> bool = "%equal"
val identity : (x : int) -> {y : int | y = x} = <fun>
val follow : (x : int) -> {y : int | y >= x} -> int = <fun>
|}]

let escaped = follow (identity 1);;
[%%expect{|
Line 1, characters 14-33:
1 | let escaped = follow (identity 1);;
                  ^^^^^^^^^^^^^^^^^^^
Error: the refinement type of this expression mentions the argument
       for parameter "x", which is not a variable
Line 1, characters 21-33:
1 | let escaped = follow (identity 1);;
                         ^^^^^^^^^^^^
  This is the argument.
Hint: bind the argument to a variable with a let outside this expression.
|}]

let bound = identity 1
let escaped_bound = follow bound;;
[%%expect{|
val bound : int = 1
val escaped_bound : {y : int | y >= bound} -> int = <fun>
|}]

let escaped_local =
  let n = 0 in
  follow n;;
[%%expect{|
Line 3, characters 2-10:
3 |   follow n;;
      ^^^^^^^^
Error: the refinement type of this expression escapes the scope of binding "n"
Hint: bind "n" outside this expression.
|}]

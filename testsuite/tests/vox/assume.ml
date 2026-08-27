(* TEST
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

let check x : nonnegative = assume_ x
let input = 42
let checked = check input
let result = let refine_ n = checked in n;;
[%%expect{|
val check : int @ total -> nonnegative = <fun>
val input : int = 42
val checked : nonnegative = 42
val result : int = 42
|}]

let rejected =
  let input = -1 in
  match check input with
  | _ -> false
  | exception Assert_failure _ -> true;;
[%%expect{|
val rejected : bool = true
|}]

module Mandatory_check = struct
  let (check @ total) x : nonnegative = assume_ x
end;;
[%%expect{|
Line 2, characters 40-49:
2 |   let (check @ total) x : nonnegative = assume_ x
                                            ^^^^^^^^^
Error: The expression is "partial"
       but is expected to be "total"
         because it is used inside the function at line 2, characters 22-49
         which is expected to be "total".
|}]

let check_sum : (x : int) -> (y : int) -> {z : int | z = x + y} =
  fun x y -> let sum = x + y in assume_ sum
let wraps =
  let x = max_int in
  let y = 1 in
  let refine_ sum = check_sum x y in
  sum = min_int;;
[%%expect{|
val check_sum : (x : int) -> (y : int) -> {z : int | z = (x + y)} = <fun>
val wraps : bool = true
|}]

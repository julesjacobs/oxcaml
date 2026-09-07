(* TEST
 flags = "-extension refinement_types";
 expect;
*)

external ( = ) : int -> int -> bool @@ total = "%equal"
external ( < ) : int -> int -> bool @@ total = "%lessthan";;
[%%expect{|
external ( = ) : int -> int -> bool = "%equal"
external ( < ) : int -> int -> bool = "%lessthan"
|}]

let add : (x : int) -> (y : int) -> {z : int | z = x + y} =
  fun x y -> let sum = x + y in refine_ sum;;
[%%expect{|
val add : (x : int) -> (y : int) -> {z : int | z = (x + y)} = <fun>
|}]

let x = 20
let y = 22
let result = let refine_ sum = add x y in sum;;
[%%expect{|
val x : int = 20
val y : int = 22
val result : int = 42
|}]

let nonvariable = add (x + 1) y;;
[%%expect{|
Line 1, characters 22-29:
1 | let nonvariable = add (x + 1) y;;
                          ^^^^^^^
Error: A dependent function argument must be a plain local variable
|}]

module type Recursion = sig
  val fix :
    ((n : int) ->
      (({m : int | m < n} -> 'a) @ total -> 'a) @ total) @ total ->
    (int -> 'a) @ total
    @@ total
end;;
[%%expect{|
module type Recursion =
  sig
    val fix :
      ((n : int) -> (({m : int | m < n} -> 'a) @ total -> 'a) @ total) @ total ->
      (int -> 'a) @ total @@ total
  end
|}]

module Countdown (R : Recursion) : sig
  val countdown : int -> int @@ total
end = struct
  let (countdown @ total) =
    R.fix (fun n recur ->
      if n > 0 then
        let next = n - 1 in
        recur (refine_ next)
      else 0)
end;;
[%%expect{|
module Countdown :
  functor (R : Recursion) -> sig val countdown : int -> int @@ total end
|}]

module Missing_inner_total (R : Recursion) (Step : sig
  val step : (n : int) ->
    ({m : int | m < n} -> int) @ total -> int @@ total
end) = struct
  let (countdown @ total) = R.fix Step.step
end;;
[%%expect{|
Line 5, characters 34-43:
5 |   let (countdown @ total) = R.fix Step.step
                                      ^^^^^^^^^
Error: The value "Step.step" has type
         "(n : int) -> ({m : int | m < n} -> int) @ total -> int"
       but an expression was expected of type
         "(n : int) -> (({m : int | m < n} -> 'a) -> 'a) @ total"
|}]

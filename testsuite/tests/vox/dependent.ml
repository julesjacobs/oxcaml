(* TEST
 has-z3;
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
val nonvariable : int = 43
|}]

(* The computed argument is bound once, so the result is still exact. *)
let nonvariable_exact : {n : int | n = x + 1 + y} = add (x + 1) y;;
[%%expect{|
val nonvariable_exact : {n : int | n = ((x + 1) + y)} = 43
|}]

let nonvariable_wrong : {n : int | n = x + y} = add (x + 1) y;;
[%%expect{|
Line 1, characters 48-61:
1 | let nonvariable_wrong : {n : int | n = x + y} = add (x + 1) y;;
                                                    ^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
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

let borrowed_sum : (left : int) -> (right : int) ->
    {n : int | n = left + right} = fun left right ->
  let refine_ sum = add (borrow_ left) (borrow_ right) in
  refine_ sum;;
[%%expect{|
val borrowed_sum :
  (left : int) -> (right : int) -> {n : int | n = (left + right)} = <fun>
|}]

let borrowed_nonvariable = add (borrow_ (x + 1)) y;;
[%%expect{|
val borrowed_nonvariable : int = 43
|}]

let borrowed_mutable () =
  let mutable current = 1 in
  let refine_ result = add (borrow_ current) y in
  result;;
[%%expect{|
Line 2, characters 14-21:
2 |   let mutable current = 1 in
                  ^^^^^^^
Warning 186 [unmutated-mutable]: mutable variable "current" was never mutated.

val borrowed_mutable : unit -> int = <fun>
|}]

(* The borrowed argument is a snapshot; its fact does not follow later
   assignments to the mutable variable. *)
let stale_borrow () =
  let mutable current = 1 in
  let result = add (borrow_ current) y in
  current <- 5;
  let now = current in
  let (_ : {n : int | n = now + y}) = result in
  ();;
[%%expect{|
Line 6, characters 38-44:
6 |   let (_ : {n : int | n = now + y}) = result in
                                          ^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

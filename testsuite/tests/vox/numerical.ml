(* TEST
 flags = "-extension refinement_types";
 has-z3;
 { expect; }
 { expect.opt; }
*)

module Countdown = struct
  let rec (countdown @ total) n =
    if n > 0 then countdown (n - 1) else 0
  [@@decreases n]
end;;
[%%expect{|
module Countdown : sig val countdown : int -> int end
|}]

module Fib = struct
  let[@def] rec fib n =
    if n <= 0 then 0
    else if n = 1 then 1
    else fib (n - 1) + fib (n - 2)
  [@@decreases n]
end;;
[%%expect{|
module Fib :
  sig
    val fib : int -> int
    val fib_def :
      (n : int) ->
      {u : unit
        | (fib n) ===
            (if n <= 0
             then 0
             else if n = 1 then 1 else (fib (n - 1)) + (fib (n - 2)))}
  end
|}]

let bad n =
  let rec loop n = loop n [@@decreases n] in loop n;;
[%%expect{|
Line 2, characters 19-25:
2 |   let rec loop n = loop n [@@decreases n] in loop n;;
                       ^^^^^^
Error: Refinement could not be proved (counterexample)
Line 2, characters 39-40:
2 |   let rec loop n = loop n [@@decreases n] in loop n;;
                                           ^
  Required by this decreases attribute
|}]

let wraps () =
  let rec loop n = loop (n - 1) [@@decreases n] in loop 0;;
[%%expect{|
Line 2, characters 19-31:
2 |   let rec loop n = loop (n - 1) [@@decreases n] in loop 0;;
                       ^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
Line 2, characters 45-46:
2 |   let rec loop n = loop (n - 1) [@@decreases n] in loop 0;;
                                                 ^
  Required by this decreases attribute
|}]

let unfolded () : {r : int | r = 1} =
  let n = 1 in
  let r = Fib.fib n in
  let refine_ proof = Fib.fib_def n in
  refine_ r;;
[%%expect{|
val unfolded : unit -> {r : int | r = 1} = <fun>
|}]

module Measures = struct
  let (captured @ total) limit =
    let rec loop i =
      if 0 <= i && i < limit then loop (i + 1) else i
    [@@decreases limit - i] in
    loop 0

  let rec (refined_measure @ total) : {n : int | 0 <= n} -> int = fun n ->
    let refine_ value = n in
    if value > 0 then
      let next = value - 1 in
      let checked : {n : int | 0 <= n} = refine_ next in
      refined_measure checked
    else 0
  [@@decreases let refine_ value = n in value]

  let rec (postcondition @ total) : int -> {r : int | r = 0} = fun n ->
    if n > 0 then
      let next = n - 1 in
      let refine_ r = postcondition next in
      postcondition r
    else let r = 0 in refine_ r
  [@@decreases n]

  let rec (negative @ total) n =
    if n > -3 then negative (n - 1) else n
  [@@decreases n]

  let rec (distance @ total) i limit =
    if 0 <= i && i < limit then distance (i + 1) limit else i
  [@@decreases limit - i]

  let rec (shadowing @ total) n =
    if n > 0 then
      let next = n - 1 in
      let n = false in
      if n then 0 else shadowing next
    else 0
  [@@decreases let entry = n in if entry > 0 then entry else 0]

  let rec (opaque @ total) n xs =
    if n > 0 then opaque (n - 1) xs else xs
  [@@decreases n]

  let rec effectful n =
    if n > 0 then (print_int n; effectful (n - 1)) else ()
  [@@decreases n]
end;;
[%%expect{|
module Measures :
  sig
    val captured : int -> int
    val refined_measure : {n : int | 0 <= n} -> int
    val postcondition : int -> {r : int | r = 0}
    val negative : int -> int
    val distance : int -> int -> int
    val shadowing : int -> int
    val opaque : int -> 'a -> 'a
    val effectful : int -> unit
  end
|}, Principal{|
module Measures :
  sig
    val captured : int @ total -> int
    val refined_measure : {n : int | 0 <= n} -> int
    val postcondition : int -> {r : int | r = 0}
    val negative : int -> int
    val distance : int -> int -> int
    val shadowing : int -> int
    val opaque : int -> 'a -> 'a
    val effectful : int -> unit
  end
|}]

module User_measure = struct
  let (identity @ total) n = n
  let rec loop n = if n > 0 then loop (n - 1) else 0
  [@@decreases identity n]
end;;
[%%expect{|
Line 4, characters 15-25:
4 |   [@@decreases identity n]
                   ^^^^^^^^^^
Error: Unsupported decreases expression: expected scalar primitive operations
Line 4, characters 15-25:
4 |   [@@decreases identity n]
                   ^^^^^^^^^^
  Required by this decreases attribute
|}]

module Branch_measure = struct
  let rec (loop @ total) (n : int) b (g @ total) =
    let m = g n in
    if m < n then loop m b g else 0
  [@@decreases if b then n else n]
end;;
[%%expect{|
module Branch_measure :
  sig val loop : int -> bool -> (int -> int) @ total -> int end
|}]

let enclosing_fact () =
  let step = 1 in
  let rec loop n = if n > 0 then loop (n - step) else 0
  [@@decreases n] in
  loop 0;;
[%%expect{|
Line 3, characters 33-48:
3 |   let rec loop n = if n > 0 then loop (n - step) else 0
                                     ^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
Line 4, characters 15-16:
4 |   [@@decreases n] in
                   ^
  Required by this decreases attribute
|}]

let rec body_local n = let next = n - 1 in body_local next
[@@decreases next];;
[%%expect{|
Line 2, characters 13-17:
2 | [@@decreases next];;
                 ^^^^
Error: Unbound value "next"
|}]

let rec non_integer n = if n > 0 then non_integer (n - 1) else 0
[@@decreases true];;
[%%expect{|
Line 2, characters 13-17:
2 | [@@decreases true];;
                 ^^^^
Error: The constructor "true" has type "bool"
       but an expression was expected of type "int"
|}]

let rec partial_measure n = partial_measure (n - 1)
[@@decreases (print_int n; n)];;
[%%expect{|
Line 2, characters 14-23:
2 | [@@decreases (print_int n; n)];;
                  ^^^^^^^^^
Error: The value "print_int" is "partial"
       but is expected to be "total"
         because it is used in an expression (at line 2, characters 13-29).
|}]

let rec user_measure n = if n > 0 then user_measure (n - 1) else 0
[@@decreases abs n];;
[%%expect{|
Line 2, characters 13-16:
2 | [@@decreases abs n];;
                 ^^^
Error: The value "abs" is "partial"
       but is expected to be "total"
         because it is used in an expression (at line 2, characters 13-18).
|}]

let rec self_measure n = if n > 0 then self_measure (n - 1) else 0
[@@decreases self_measure n];;
[%%expect{|
Line 2, characters 13-25:
2 | [@@decreases self_measure n];;
                 ^^^^^^^^^^^^
Error: The value "self_measure" is "partial"
       but is expected to be "total"
         because it is used in an expression (at line 2, characters 13-27).
|}]

let rec alias n = let f = alias in if n > 0 then f (n - 1) else 0
[@@decreases n];;
[%%expect{|
Line 1, characters 26-31:
1 | let rec alias n = let f = alias in if n > 0 then f (n - 1) else 0
                              ^^^^^
Error: the recursive function must be called directly
|}]

let rec delayed n =
  let f () = delayed (n - 1) in if n > 0 then f () else 0
[@@decreases n];;
[%%expect{|
Line 2, characters 13-20:
2 |   let f () = delayed (n - 1) in if n > 0 then f () else 0
                 ^^^^^^^
Error: the recursive function occurs in a delayed body
|}]

let rec functor_body n =
  let module F (X : sig end) = struct
    let x = if n > 0 then functor_body (n - 1) else 0
  end in
  0
[@@decreases n];;
[%%expect{|
Line 3, characters 26-38:
3 |     let x = if n > 0 then functor_body (n - 1) else 0
                              ^^^^^^^^^^^^
Error: the recursive function occurs in a delayed body
|}]

let rec class_body n =
  let module M = struct
    class c = object
      val x = if n > 0 then class_body (n - 1) else 0
    end
  end in
  0
[@@decreases n];;
[%%expect{|
Line 4, characters 28-38:
4 |       val x = if n > 0 then class_body (n - 1) else 0
                                ^^^^^^^^^^
Error: the recursive function occurs in a delayed body
|}]

let rec eager_object n =
  let obj = object val x = if n > 0 then eager_object (n - 1) else 0 end in
  ignore obj; 0
[@@decreases n];;
[%%expect{|
val eager_object : int -> int = <fun>
|}]

let rec partial_application n x = ignore (partial_application (n - 1)); x
[@@decreases n];;
[%%expect{|
Line 1, characters 41-70:
1 | let rec partial_application n x = ignore (partial_application (n - 1)); x
                                             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 5 [ignored-partial-application]: this function application is partial,
  maybe some arguments are missing.

Line 1, characters 41-70:
1 | let rec partial_application n x = ignore (partial_application (n - 1)); x
                                             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: recursive calls must supply every value parameter
|}]

let rec circular : (n : int) -> {r : int | r < n} = fun n ->
  let refine_ r = circular n in refine_ r
[@@decreases n];;
[%%expect{|
Line 2, characters 18-28:
2 |   let refine_ r = circular n in refine_ r
                      ^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
Line 3, characters 13-14:
3 | [@@decreases n];;
                 ^
  Required by this decreases attribute
|}]

let rec nested n = if n > 0 then nested (nested n) else 0
[@@decreases n];;
[%%expect{|
Line 1, characters 40-50:
1 | let rec nested n = if n > 0 then nested (nested n) else 0
                                            ^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
Line 2, characters 13-14:
2 | [@@decreases n];;
                 ^
  Required by this decreases attribute
|}]

let nonrecursive n = n [@@decreases n];;
[%%expect{|
Line 1, characters 36-37:
1 | let nonrecursive n = n [@@decreases n];;
                                        ^
Error: The decreases attribute requires a single recursive function binding
|}]

let rec mutual n = other n [@@decreases n]
and other n = mutual n;;
[%%expect{|
Line 1, characters 40-41:
1 | let rec mutual n = other n [@@decreases n]
                                            ^
Error: The decreases attribute requires a single recursive function binding
|}]

let rec value = 0 [@@decreases 0];;
[%%expect{|
Line 1, characters 31-32:
1 | let rec value = 0 [@@decreases 0];;
                                   ^
Error: The decreases attribute requires a function binding
|}]

let rec duplicate n = duplicate (n - 1) [@@decreases n] [@@decreases n];;
[%%expect{|
Line 1, characters 56-71:
1 | let rec duplicate n = duplicate (n - 1) [@@decreases n] [@@decreases n];;
                                                            ^^^^^^^^^^^^^^^
Error: Duplicate decreases attribute
|}]

let rec missing n = missing (n - 1) [@@decreases];;
[%%expect{|
Line 1, characters 36-49:
1 | let rec missing n = missing (n - 1) [@@decreases];;
                                        ^^^^^^^^^^^^^
Error: The decreases attribute requires one expression
|}]

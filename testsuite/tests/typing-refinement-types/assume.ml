(* TEST
 flags = "-extension refinement_types";
 expect;
*)

external equal : int -> int -> bool @@ total = "%equal";;
[%%expect{|
external equal : int -> int -> bool = "%equal"
|}]

type zero = { v : int | equal v 0 };;
[%%expect{|
type zero = {v : int | equal v 0}
|}]

let check (x : int) : zero = assume_ x;;
[%%expect{|
val check : int -> zero = <fun>
|}]

let unknown x = assume_ x;;
[%%expect{|
Line 1, characters 16-25:
1 | let unknown x = assume_ x;;
                    ^^^^^^^^^
Error: "assume_" requires a known refinement type from its context
|}]

let nonvariable : zero = assume_ (0 + 0);;
[%%expect{|
Line 1, characters 33-40:
1 | let nonvariable : zero = assume_ (0 + 0);;
                                     ^^^^^^^
Error: "assume_" requires a plain local variable
|}]

module M = struct let x = 0 end
let qualified : zero = assume_ M.x;;
[%%expect{|
module M : sig val x : int end
Line 2, characters 31-34:
2 | let qualified : zero = assume_ M.x;;
                                   ^^^
Error: "assume_" requires a plain local variable
|}]

let (bad @ total) (x : int) : zero = assume_ x;;
[%%expect{|
Line 1, characters 37-46:
1 | let (bad @ total) (x : int) : zero = assume_ x;;
                                         ^^^^^^^^^
Error: The expression is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 18-46
         which is expected to be "total".
|}]

let (bad_true @ total) (x : int) : { v : int | true } = assume_ x;;
[%%expect{|
Line 1, characters 56-65:
1 | let (bad_true @ total) (x : int) : { v : int | true } = assume_ x;;
                                                            ^^^^^^^^^
Error: The expression is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 23-65
         which is expected to be "total".
|}]

let partial x = 1 / x
let check_function : { f : int -> int | true } = assume_ partial;;
[%%expect{|
val partial : int -> int = <fun>
Line 2, characters 57-64:
2 | let check_function : { f : int -> int | true } = assume_ partial;;
                                                             ^^^^^^^
Error: This value is "partial" but is expected to be "total".
|}]

let dependent :
    (x : int) -> (y : int) -> { z : unit | x + y = y + x } =
  fun x y -> let u = () in assume_ u;;
[%%expect{|
val dependent : (x : int) -> (y : int) -> {z : unit | (x + y) = (y + x)} =
  <fun>
|}]

let zero = 0
let checked : zero = assume_ zero
let result = let refine_ result = checked in result;;
[%%expect{|
val zero : int = 0
val checked : zero = 0
val result : int = 0
|}]

let () =
  let x = 2 in
  let y = 3 in
  let refine_ proof = dependent x y in
  proof;;
[%%expect{|
|}]

let (checked_total @ total) = check zero;;
[%%expect{|
val checked_total : zero = 0
|}]

let (check_total @ total) = check;;
[%%expect{|
Line 1, characters 28-33:
1 | let (check_total @ total) = check;;
                                ^^^^^
Error: This value is "partial" but is expected to be "total".
|}]

module F (X : sig end) = struct
  let predicate (x : int) = equal x 0
  type checked = { v : int | predicate v }
end
module Input = struct end;;
[%%expect{|
module F :
  functor (X : sig end) ->
    sig
      val predicate : int -> bool
      type checked = {v : int | predicate v}
    end
module Input : sig end
|}]

let unavailable (x : int) : F(Input).checked = assume_ x;;
[%%expect{|
Line 1, characters 47-56:
1 | let unavailable (x : int) : F(Input).checked = assume_ x;;
                                                   ^^^^^^^^^
Error: Cannot recover runtime evidence for refinement reference "F(Input).predicate"
|}]

module Applied = F(Input)
let available (x : int) : Applied.checked = assume_ x;;
[%%expect{|
module Applied :
  sig val predicate : int -> bool type checked = {v : int | predicate v} end
val available : int -> Applied.checked = <fun>
|}]

module Primitive (X : sig end) = struct
  external equal : int -> int -> bool @@ total = "%equal"
  type checked = { v : int | equal v 0 }
end
let primitive : Primitive(Input).checked = assume_ zero;;
[%%expect{|
module Primitive :
  functor (X : sig end) ->
    sig
      external equal : int -> int -> bool = "%equal"
      type checked = {v : int | equal v 0}
    end
val primitive : Primitive(Input).checked = 0
|}]

exception Nonportable of (unit -> unit) @@ stateless
type checked_exception =
  { v : exn | match v with Nonportable _ -> true | _ -> false }
let (check_exception @ portable) (x : exn) : checked_exception = assume_ x;;
[%%expect{|
exception Nonportable of (unit -> unit) @@ stateless
type checked_exception =
    {v : exn | match v with | Nonportable _ -> true | _ -> false}
val check_exception : exn -> checked_exception = <fun>
|}]

let unique_result (x : int ref) : { v : int ref | true } @ unique = assume_ x;;
[%%expect{|
Line 1, characters 68-77:
1 | let unique_result (x : int ref) : { v : int ref | true } @ unique = assume_ x;;
                                                                        ^^^^^^^^^
Error: This value is "aliased" but is expected to be "unique".
|}]

type scalar_comparison = { v : int | v = 0 }
let check_scalar x : scalar_comparison = assume_ x;;
[%%expect{|
type scalar_comparison = {v : int | v = 0}
val check_scalar : int @ total -> scalar_comparison = <fun>
|}, Principal{|
Line 1, characters 37-38:
1 | type scalar_comparison = { v : int | v = 0 }
                                         ^
Error: This value is "immutable" but is expected to be "read_write".
|}]

let warning_state x : zero = assume_ x
let warning_restored x = match x with _ -> true | _ -> false;;
[%%expect{|
val warning_state : int @ total -> zero = <fun>
Line 2, characters 50-51:
2 | let warning_restored x = match x with _ -> true | _ -> false;;
                                                      ^
Warning 11 [redundant-case]: this match case is unused.

val warning_restored : 'a -> bool = <fun>
|}]

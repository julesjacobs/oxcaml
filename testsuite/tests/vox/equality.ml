(* TEST
 flags = "-extension refinement_types";
 has-z3;
 { expect; }
 { expect.opt; }
*)

type one = {x : int | x === 1};;
[%%expect{|
type one = {x : int | x === 1}
|}]

let reflexive (x : int) : {r : int | r === x} =
  let r = x in
  refine_ r;;
[%%expect{|
val reflexive : (x : int) -> {r : int | r === x} = <fun>
|}]

module Opaque : sig
  type t
  val zero : t
  val one : t
  val observe : t @ total immutable -> int @@ total
  val choose : bool -> t @@ total
end = struct
  type t = int
  let zero = 0
  let one = 1
  let observe x = x
  let choose b = if b then zero else one
end;;
[%%expect{|
module Opaque :
  sig
    type t
    val zero : t
    val one : t
    val observe : t @ total immutable -> int @@ total
    val choose : bool -> t @@ total
  end
|}]

let opaque_reflexive () : {r : Opaque.t | r === Opaque.zero} =
  let r = Opaque.zero in
  refine_ r;;
[%%expect{|
val opaque_reflexive : unit -> {r : Opaque.t | r === Opaque.zero} = <fun>
|}]

let opaque_result_congruence b :
    {u : unit | Opaque.choose b === Opaque.choose b} =
  let u = () in
  refine_ u;;
[%%expect{|
val opaque_result_congruence :
  (b : bool) -> {u : unit | (Opaque.choose b) === (Opaque.choose b)} = <fun>
|}]

type token = Token

let constructor_reflexive : {r : token | r === Token} =
  let r = Token in
  refine_ r;;
[%%expect{|
type token = Token
val constructor_reflexive : {r : token | r === Token} = Token
|}]

let opaque_argument_congruence (x : Opaque.t) (y : {y : Opaque.t | y === x})
    : {u : unit |
        let refine_ y = y in
        Opaque.observe y === Opaque.observe x} =
  let u = () in
  refine_ u;;
[%%expect{|
val opaque_argument_congruence :
  (x : Opaque.t) ->
  (y' : {y : Opaque.t | y === x}) ->
  {u : unit
    | let refine_ y = y' in (Opaque.observe y) === (Opaque.observe x)} =
  <fun>
|}]

let fresh_allocations : {u : unit | ref 0 === ref 0} =
  let u = () in
  refine_ u;;
[%%expect{|
Line 1, characters 36-39:
1 | let fresh_allocations : {u : unit | ref 0 === ref 0} =
                                        ^^^
Error: The value "ref" is "partial"
       but is expected to be "total"
         because it is used in an expression (at line 1, characters 36-51).
|}]

let scalar_failure =
  let x = 0 in
  match (assume_ x : {r : int | r === 1}) with
  | _ -> false
  | exception Assert_failure _ -> true;;
[%%expect{|
val scalar_failure : bool = true
|}]

module Functions = struct
  let (id @ total) x = x
  let (other @ total) x = x
end;;
[%%expect{|
module Functions : sig val id : 'a -> 'a val other : 'a -> 'a end
|}]

let function_identity =
  let f = Functions.id in
  match (assume_ f : {g : (int -> int) | g === f}) with
  | _ -> true
  | exception _ -> false;;
[%%expect{|
val function_identity : bool = true
|}]

let function_unknown =
  let f = Functions.id in
  let g = Functions.other in
  match (assume_ f : {h : (int -> int) | h === g}) with
  | _ -> false
  | exception Invalid_argument _ -> true;;
[%%expect{|
val function_unknown : bool = true
|}]

let ordinary_expression x y = x === y;;
[%%expect{|
Line 1, characters 30-37:
1 | let ordinary_expression x y = x === y;;
                                  ^^^^^^^
Error: "===" is available only in refinement predicates
|}]

let float_reflexive (x : float) : {b : bool | b} =
  let b = Stdlib.(=) x x in
  refine_ b;;
[%%expect{|
Line 3, characters 2-11:
3 |   refine_ b;;
      ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let float_irreflexive (x : float) : {b : bool | b === false} =
  let b = Stdlib.(<>) x x in
  refine_ b;;
[%%expect{|
Line 3, characters 2-11:
3 |   refine_ b;;
      ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

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

module Definitions = struct
  let[@def] next x = x + 1
end;;
[%%expect{|
module Definitions :
  sig
    val next : int -> int
    val next_def : (x : int) -> {u : unit | (next x) === (x + 1)}
  end
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

type 'a nothing = Nothing

let int_nothing : {r : int nothing | r === Nothing} =
  let (r : int nothing) = Nothing in
  refine_ r

let bool_nothing : {r : bool nothing | r === Nothing} =
  let (r : bool nothing) = Nothing in
  refine_ r;;
[%%expect{|
type 'a nothing = Nothing
val int_nothing : {r : int nothing | r === Nothing} = Nothing
val bool_nothing : {r : bool nothing | r === Nothing} = Nothing
|}]

let polymorphic_nothing = Nothing;;
[%%expect{|
val polymorphic_nothing : 'a nothing = Nothing
|}]

let int_polymorphic : {r : int nothing | r === polymorphic_nothing} =
  let r = polymorphic_nothing in
  refine_ r;;
[%%expect{|
val int_polymorphic : {r : int nothing | r === polymorphic_nothing} = Nothing
|}]

let bool_polymorphic : {r : bool nothing | r === polymorphic_nothing} =
  let r = polymorphic_nothing in
  refine_ r;;
[%%expect{|
val bool_polymorphic : {r : bool nothing | r === polymorphic_nothing} =
  Nothing
|}]

type side = Left | Right

let preserve_side (x : side) : {r : side | r === x} =
  match x with
  | Left ->
    let r = Left in
    refine_ r
  | Right ->
    let r = Right in
    refine_ r;;
[%%expect{|
type side = Left | Right
val preserve_side : (x : side) -> {r : side | r === x} = <fun>
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

let unknown =
  let x = Opaque.zero in
  let y = Opaque.one in
  match (assume_ x : {r : Opaque.t | r === y}) with
  | _ -> false
  | exception Invalid_argument _ -> true;;
[%%expect{|
val unknown : bool = true
|}]

let scalar_failure =
  let x = 0 in
  match (assume_ x : {r : int | r === 1}) with
  | _ -> false
  | exception Assert_failure _ -> true;;
[%%expect{|
val scalar_failure : bool = true
|}]

type any_int = {x : int | x === x};;
[%%expect{|
type any_int = {x : int | x === x}
|}]

let refined_scalar_failure =
  let raw_x = 0 in
  let raw_y = 1 in
  let x = (assume_ raw_x : any_int) in
  let y = (assume_ raw_y : any_int) in
  match (assume_ x : {r : any_int | r === y}) with
  | _ -> false
  | exception Assert_failure _ -> true
  | exception Invalid_argument _ -> false;;
[%%expect{|
val refined_scalar_failure : bool = true
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

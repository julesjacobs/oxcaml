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

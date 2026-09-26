(* TEST
   flags = "-w -a";
   expect;
*)

type color = Red | Blue

let (is_red @ total) = function Red -> true | Blue -> false
[%%expect{|
type color = Red | Blue
val is_red : color -> bool = <fun>
|}]

type ordinary_nat = Z | S of ordinary_nat

let (ordinary_predecessor @ total) = function Z -> Z | S n -> n
[%%expect{|
type ordinary_nat = Z | S of ordinary_nat
Line 3, characters 55-58:
3 | let (ordinary_predecessor @ total) = function Z -> Z | S n -> n
                                                           ^^^
Error: The expression is "partial"
       but is expected to be "total"
         because it is used inside the function at line 3, characters 37-63
         which is expected to be "total".
|}]

let ordinary_predecessor = function Z -> Z | S n -> n
[%%expect{|
val ordinary_predecessor : ordinary_nat -> ordinary_nat = <fun>
|}]

type nat = Z | S of nat [@@inductive]

let (predecessor @ total) = function Z -> Z | S n -> n
[%%expect{|
type nat = Z | S of nat [@@inductive]
val predecessor : nat -> nat = <fun>
|}]

let rec cyclic_nat = S cyclic_nat
[%%expect{|
Line 1, characters 21-33:
1 | let rec cyclic_nat = S cyclic_nat
                         ^^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of "let rec"
|}]

let rec left_cyclic_nat = S right_cyclic_nat
and right_cyclic_nat = S left_cyclic_nat
[%%expect{|
Line 1, characters 26-44:
1 | let rec left_cyclic_nat = S right_cyclic_nat
                              ^^^^^^^^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of "let rec"
|}]

type knot = Roll of (knot -> int)

let (roll @ total) f = Roll f
let (ignore_knot @ total) (_ : knot) = 0
[%%expect{|
type knot = Roll of (knot -> int)
val roll : (knot -> int) -> knot = <fun>
val ignore_knot : knot -> int = <fun>
|}]

let (unroll @ total) = function Roll f -> f
[%%expect{|
Line 1, characters 32-38:
1 | let (unroll @ total) = function Roll f -> f
                                    ^^^^^^
Error: The expression is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 23-43
         which is expected to be "total".
|}]

type invalid_knot = Invalid_roll of (invalid_knot -> int) [@@inductive]
[%%expect{|
Line 1, characters 0-71:
1 | type invalid_knot = Invalid_roll of (invalid_knot -> int) [@@inductive]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Invalid inductive declaration: recursive occurrences must be direct fields or tuple components.
|}]

type invalid_payload = Invalid_payload [@@inductive "payload"]
[%%expect{|
Line 1, characters 42-51:
1 | type invalid_payload = Invalid_payload [@@inductive "payload"]
                                              ^^^^^^^^^
Error: Attribute "inductive" does not accept a payload
|}]

type duplicate_attribute = Duplicate_attribute
  [@@inductive] [@@inductive]
[%%expect{|
Line 2, characters 19-28:
2 |   [@@inductive] [@@inductive]
                       ^^^^^^^^^
Error: Too many "inductive" attributes
|}]

type recursive_record = { run : recursive_record -> int }

let (project @ total) x = x.run
[%%expect{|
type recursive_record = { run : recursive_record -> int; }
Line 3, characters 26-31:
3 | let (project @ total) x = x.run
                              ^^^^^
Error: The expression is "partial"
       but is expected to be "total"
         because it is used inside the function at line 3, characters 22-31
         which is expected to be "total".
|}]

let (destructure @ total) { run } = run
[%%expect{|
Line 1, characters 26-33:
1 | let (destructure @ total) { run } = run
                              ^^^^^^^
Error: The expression is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 26-39
         which is expected to be "total".
|}]

type nat_wrapper = { payload : ordinary_nat }

let (payload @ total) wrapper = wrapper.payload
[%%expect{|
type nat_wrapper = { payload : ordinary_nat; }
val payload : nat_wrapper -> ordinary_nat = <fun>
|}]

let (generic_variant @ total) = function `Value x -> x
[%%expect{|
Line 1, characters 41-49:
1 | let (generic_variant @ total) = function `Value x -> x
                                             ^^^^^^^^
Error: The expression is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 32-54
         which is expected to be "total".
|}]

let (option_value @ total) = function None -> 0 | Some x -> x
[%%expect{|
val option_value : int option -> int = <fun>
|}]

type 'a box = Box of 'a

let (unbox @ total) = function Box x -> x
[%%expect{|
type 'a box = Box of 'a
val unbox : 'a box -> 'a = <fun>
|}]

type 'a continuation = Continue of ('a -> int) [@@inductive]

let (continue @ total) = function Continue f -> f
[%%expect{|
type 'a continuation = Continue of ('a -> int) [@@inductive]
val continue : 'a continuation -> 'a -> int = <fun>
|}]

type 'a wrapper = Wrapper of 'a
type separate_instances = Separate_instances of int wrapper * unit wrapper

let (first_instance @ total) = function Separate_instances (x, _) -> x
[%%expect{|
type 'a wrapper = Wrapper of 'a
type separate_instances = Separate_instances of int wrapper * unit wrapper
val first_instance : separate_instances -> int wrapper = <fun>
|}]

type 'a growing = Grow of ('a * 'a) growing

let (ungrow @ total) = function Grow x -> x
[%%expect{|
type 'a growing = Grow of ('a * 'a) growing
Line 3, characters 32-38:
3 | let (ungrow @ total) = function Grow x -> x
                                    ^^^^^^
Error: The expression is "partial"
       but is expected to be "total"
         because it is used inside the function at line 3, characters 23-43
         which is expected to be "total".
|}]

type 'a phantom = Phantom
type phantom_recursive = Wrap_phantom of phantom_recursive phantom

let (unwrap_phantom @ total) = function Wrap_phantom Phantom -> ()
[%%expect{|
type 'a phantom = Phantom
type phantom_recursive = Wrap_phantom of phantom_recursive phantom
val unwrap_phantom : phantom_recursive -> unit = <fun>
|}]

let (list_head @ total) = function [] -> 0 | x :: _ -> x
[%%expect{|
val list_head : int list -> int = <fun>
|}]

type ordinary_nat_alias = ordinary_nat

let (alias_predecessor @ total) : ordinary_nat_alias -> ordinary_nat_alias =
  function Z -> Z | S n -> n
[%%expect{|
type ordinary_nat_alias = ordinary_nat
Line 4, characters 20-23:
4 |   function Z -> Z | S n -> n
                        ^^^
Error: The expression is "partial"
       but is expected to be "total"
         because it is used inside the function at line 4, characters 2-28
         which is expected to be "total".
|}]

type nat_alias = nat

let (alias_predecessor @ total) : nat_alias -> nat_alias =
  function Z -> Z | S n -> n
[%%expect{|
type nat_alias = nat
val alias_predecessor : nat_alias -> nat_alias = <fun>
|}]

module rec Left : sig
  type t = Left of Right.t
end = Left
and Right : sig
  type t = Right of Left.t
end = Right

let (unwrap_left @ total) = function Left.Left right -> right
[%%expect{|
module rec Left : sig type t = Left of Right.t end
and Right : sig type t = Right of Left.t end
Line 8, characters 37-52:
8 | let (unwrap_left @ total) = function Left.Left right -> right
                                         ^^^^^^^^^^^^^^^
Error: The expression is "partial"
       but is expected to be "total"
         because it is used inside the function at line 8, characters 28-61
         which is expected to be "total".
|}]

module Cannot_forge : sig
  type t = Stop | More of t [@@inductive]
end = struct
  type t = Stop | More of t
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = Stop | More of t
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = Stop | More of t end
       is not included in
         sig type t = Stop | More of t [@@inductive] end
       Type declarations do not match:
         type t = Stop | More of t
       is not included in
         type t = Stop | More of t
       [@@inductive]
       Their inductive guarantees differ;
       the guarantee can only be hidden
       behind an abstract type.
|}]

module type Abstract_dependency = sig
  type payload
  type t = Abstract_roll of (payload -> int) [@@inductive]
end

module Abstract_eliminator (Argument : Abstract_dependency) = struct
  let (unroll @ total) = function Argument.Abstract_roll f -> f
end
[%%expect{|
module type Abstract_dependency =
  sig
    type payload
    type t = Abstract_roll of (payload -> int)
    [@@inductive]
  end
module Abstract_eliminator :
  functor (Argument : Abstract_dependency) ->
    sig val unroll : Argument.t -> Argument.payload -> int end
|}]

module rec Closed :
  (Abstract_dependency with type payload = Closed.t) = struct
  type payload = Closed.t
  type t = Abstract_roll of (payload -> int) [@@inductive]
end
[%%expect{|
Line 2, characters 3-51:
2 |   (Abstract_dependency with type payload = Closed.t) = struct
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "Closed.t" has an [@@inductive] guarantee, which is not allowed in a recursive module signature.
|}]

module rec Uses_nat : sig
  val identity : nat -> nat @@ total
end = struct
  let (identity @ total) x = x
end
[%%expect{|
module rec Uses_nat : sig val identity : nat -> nat @@ total end
|}]

module rec Nested : sig
  module Inner : sig
    type t = Stop | More of t [@@inductive]
  end
end = struct
  module Inner = struct
    type t = Stop | More of t [@@inductive]
  end
end
[%%expect{|
Lines 1-5, characters 20-3:
1 | ....................sig
2 |   module Inner : sig
3 |     type t = Stop | More of t [@@inductive]
4 |   end
5 | end.........
Error: Type "t" has an [@@inductive] guarantee, which is not allowed in a recursive module signature.
|}]

module rec Internal : sig
  type payload = Internal.t
  type t = Internal_roll of (payload -> int) [@@inductive]
end = struct
  type payload = Internal.t
  type t = Internal_roll of (payload -> int) [@@inductive]
end
[%%expect{|
Lines 1-4, characters 22-3:
1 | ......................sig
2 |   type payload = Internal.t
3 |   type t = Internal_roll of (payload -> int) [@@inductive]
4 | end.........
Error: Type "Internal.t" has an [@@inductive] guarantee, which is not allowed in a recursive module signature.
|}]

module rec Transforming_left : sig
  type 'a t = Transforming_left of ('a * 'a) Transforming_right.t
end = Transforming_left
and Transforming_right : sig
  type 'a t = Transforming_right of 'a Transforming_left.t
end = Transforming_right

type transforming_root = Transforming_root of int Transforming_left.t

let (unwrap_transforming_root @ total) = function Transforming_root x -> x
[%%expect{|
module rec Transforming_left :
  sig type 'a t = Transforming_left of ('a * 'a) Transforming_right.t end
and Transforming_right :
  sig type 'a t = Transforming_right of 'a Transforming_left.t end
type transforming_root = Transforming_root of int Transforming_left.t
Line 10, characters 50-69:
10 | let (unwrap_transforming_root @ total) = function Transforming_root x -> x
                                                       ^^^^^^^^^^^^^^^^^^^
Error: The expression is "partial"
       but is expected to be "total"
         because it is used inside the function at line 10, characters 41-74
         which is expected to be "total".
|}]

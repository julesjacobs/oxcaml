(* TEST
 flags = "-extension refinement_types";
 has-z3;
 expect;
*)

external ( = ) : int -> int -> bool @@ total = "%equal";;
[%%expect{|
external ( = ) : int -> int -> bool = "%equal"
|}]

module V = struct
  module List = struct
    type 'a t = Nil | Cons of 'a * 'a t [@@inductive]
    let rec (length @ total) (xs @ immutable) =
      match xs with
      | Nil -> 0
      | Cons (_, rest) -> 1 + length rest
  end
end;;
[%%expect{|
module V :
  sig
    module List :
      sig
        type 'a t = Nil | Cons of 'a * 'a t
        [@@inductive]
        val length : 'a t @ immutable -> int
      end
  end
|}]

let two = V.List.Cons (20, V.List.Cons (22, V.List.Nil))
let length = V.List.length two;;
[%%expect{|
val two : int V.List.t = V.List.Cons (20, V.List.Cons (22, V.List.Nil))
val length : int = 2
|}]

type two_elements = {xs : int V.List.t | V.List.length xs = 2}
let checked : two_elements = assume_ two
let result = let refine_ xs = checked in V.List.length xs;;
[%%expect{|
type two_elements = {xs : int V.List.t | (V.List.length xs) = 2}
val checked : two_elements = V.List.Cons (20, V.List.Cons (22, V.List.Nil))
val result : int = 2
|}]

let rec cycle = V.List.Cons (0, cycle);;
[%%expect{|
Line 1, characters 16-38:
1 | let rec cycle = V.List.Cons (0, cycle);;
                    ^^^^^^^^^^^^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of "let rec"
|}]

module No_descent = struct
  let rec (length @ total) xs =
    match xs with
    | V.List.Nil -> 0
    | V.List.Cons (_, _) -> length xs
end;;
[%%expect{|
Line 5, characters 28-37:
5 |     | V.List.Cons (_, _) -> length xs
                                ^^^^^^^^^
Error: This recursive function cannot be total: the recursive argument is not a known proper descendant.
|}]

module Forged : sig
  type t = Leaf | Node of t [@@inductive]
end = struct
  type t = Leaf | Node of t
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = Leaf | Node of t
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = Leaf | Node of t end
       is not included in
         sig type t = Leaf | Node of t [@@inductive] end
       Type declarations do not match:
         type t = Leaf | Node of t
       is not included in
         type t = Leaf | Node of t
       [@@inductive]
       Their inductive guarantees differ;
       the guarantee can only be hidden
       behind an abstract type.
|}]

module Composition = struct
  let rec (length @ total) (xs @ immutable) =
    match xs with
    | V.List.Nil -> 0
    | V.List.Cons (_, rest) ->
        let[@def] step n = n + 1 in
        step (length rest)
end;;
[%%expect{|
module Composition : sig val length : 'a V.List.t @ immutable -> int end
|}]

let unproved () : {n : int | n = 0} =
  let xs = V.List.Nil in
  let n = V.List.length xs in
  refine_ n;;
[%%expect{|
Line 4, characters 2-11:
4 |   refine_ n;;
      ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

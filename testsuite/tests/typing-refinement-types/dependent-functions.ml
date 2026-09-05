(* TEST
 has-z3;
 flags = "-extension refinement_types";
 expect;
*)

external add : int -> int -> int @@ total = "%addint"
external eq : int -> int -> bool @@ total = "%equal"
external gt : int -> int -> bool @@ total = "%greaterthan";;
[%%expect{|
external add : int -> int -> int = "%addint"
external eq : int -> int -> bool = "%equal"
external gt : int -> int -> bool = "%greaterthan"
|}]

type bounded = (n : int) -> {r : int | n >= 0 && r = n};;
[%%expect{|
type bounded = (n : int) -> {r : int | (n >= 0) && (r = n)}
|}]

type two_indices =
  (n : int) -> (m : int) -> {r : int | n <= m && r = n};;
[%%expect{|
type two_indices = (n : int) -> (m : int) -> {r : int | (n <= m) && (r = n)}
|}]

type 'a constrained_dependent =
  (p : ('a * int)) -> {r : int | match p with a, b -> r = a + b};;
[%%expect{|
type 'a constrained_dependent =
    (p : 'a * int) -> {r : int | match p with | (a, b) -> r = (a + b)}
  constraint 'a = int
|}]

type addition =
  (x : int) -> (y : int) -> {z : int | eq z (add x y)}

type addition_alpha =
  (left : int) -> (right : int) ->
  {result : int | eq result (add left right)}

let alpha_equivalent : addition list = ([] : addition_alpha list);;
[%%expect{|
type addition = (x : int) -> (y : int) -> {z : int | eq z (add x y)}
type addition_alpha =
    (left : int) ->
    (right : int) -> {result : int | eq result (add left right)}
val alpha_equivalent : addition list = []
|}]

let add_refined : addition =
  fun x y -> let raw = add x y in refine_ raw;;
[%%expect{|
val add_refined : addition = <fun>
|}]

let apply x y : {z : int | eq z (add x y)} =
  add_refined x y;;
[%%expect{|
val apply : (x : int) -> (y : int) -> {z : int | eq z (add x y)} = <fun>
|}]

let bad_argument x y = add_refined (add x 1) y;;
[%%expect{|
Line 1, characters 35-44:
1 | let bad_argument x y = add_refined (add x 1) y;;
                                       ^^^^^^^^^
Error: A dependent function argument must be a plain local variable
|}]

module M = struct let x = 1 end
let bad_qualified y = add_refined M.x y;;
[%%expect{|
module M : sig val x : int end
Line 2, characters 34-37:
2 | let bad_qualified y = add_refined M.x y;;
                                      ^^^
Error: A dependent function argument must be a plain local variable
|}]

type ordinary = (unused : int) -> int
type ordinary_curried = (unused : int) -> int -> int
let ordinary_argument : ordinary = fun x -> x
let ordinary_curried_equivalent : ordinary_curried list =
  ([] : (int -> int -> int) list)
let ordinary_application = ordinary_argument (add 1 2);;
[%%expect{|
type ordinary = int -> int
type ordinary_curried = int -> int -> int
val ordinary_argument : ordinary = <fun>
val ordinary_curried_equivalent : ordinary_curried list = []
val ordinary_application : int = 3
|}]

let explicitly_dependent x : {y : int | eq y (add x 1)} =
  let raw = add x 1 in refine_ raw;;
[%%expect{|
val explicitly_dependent : (x : int) -> {y : int | eq y (add x 1)} = <fun>
|}]

let rec directly_annotated_recursive :
  (x : int) -> {y : int | eq y x} =
  fun x -> refine_ x;;
[%%expect{|
val directly_annotated_recursive : (x : int) -> {y : int | eq y x} = <fun>
|}]

type labelled_after_dependent =
  (x : int) -> label:int -> {z : int | eq z (add x 1)}

let labelled_after_dependent : labelled_after_dependent =
  fun x ~label:_ -> let raw = add x 1 in refine_ raw

let label_only = labelled_after_dependent ~label:0

let apply_label_first x : {z : int | eq z (add x 1)} =
  labelled_after_dependent ~label:0 x;;
[%%expect{|
type labelled_after_dependent =
    (x : int) -> label:int -> {z : int | eq z (add x 1)}
val labelled_after_dependent : labelled_after_dependent = <fun>
val label_only : (x : int) -> {z : int | eq z (add x 1)} = <fun>
val apply_label_first : (x : int) -> {z : int | eq z (add x 1)} = <fun>
|}]

type box = { mutable contents : int }
external same_box : box @ immutable -> box @ immutable -> bool @@ total = "%equal"

let keep_box :
    (box : box) -> {result : box | same_box result box} =
  fun box -> assume_ box

let mutate_returned_box box =
  let refine_ returned = keep_box box in
  returned.contents <- 1;;
[%%expect{|
type box = { mutable contents : int; }
external same_box : box @ immutable -> box @ immutable -> bool = "%equal"
val keep_box : (box : box) -> {result : box | same_box result box} = <fun>
val mutate_returned_box : box -> unit = <fun>
|}]

let mutate_before_return box : {result : box | same_box result box} =
  box.contents <- 1;
  assume_ box;;
[%%expect{|
val mutate_before_return :
  (box : box) -> {result : box | same_box result box} = <fun>
|}]

let keep_immutable (box @ immutable) :
    {result : box | same_box result box} @ immutable =
  assume_ box;;
[%%expect{|
val keep_immutable :
  (box : box) -> {result : box | same_box result box} @ immutable = <fun>
|}]

let bad_pattern :
  (pair : (int * int)) ->
  {z : int | eq z (match pair with left, _ -> left)} =
  fun (left, _) -> refine_ left;;
[%%expect{|
Line 4, characters 6-15:
4 |   fun (left, _) -> refine_ left;;
          ^^^^^^^^^
Error: A function checked against a dependent arrow must have a simple variable parameter
|}]

type positive = {x : int | gt x 0}
type above_argument =
  (r : positive) ->
  {y : int | let refine_ x = r in gt y x};;
[%%expect{|
type positive = {x : int | gt x 0}
type above_argument =
    (r : positive) -> {y : int | let refine_ x = r in gt y x}
|}]

type applies_function =
  (f : (int -> int)) -> {z : int | eq z (f 0)}

let apply_function : applies_function =
  fun f -> let raw = f 0 in assume_ raw

let partial x = x
let bad_partial = apply_function partial;;
[%%expect{|
type applies_function = (f : (int -> int)) -> {z : int | eq z (f 0)}
val apply_function : applies_function = <fun>
val partial : 'a -> 'a = <fun>
Line 8, characters 33-40:
8 | let bad_partial = apply_function partial;;
                                     ^^^^^^^
Error: This value is "partial" but is expected to be "total".
|}]

external increment : int -> int @@ total = "%identity"
let good_total = apply_function increment;;
[%%expect{|
external increment : int -> int = "%identity"
val good_total : {z : int | eq z (increment 0)} = 0
|}]

let mutable_argument () =
  let mutable x = 0 in
  ignore (explicitly_dependent x);;
[%%expect{|
Line 3, characters 31-32:
3 |   ignore (explicitly_dependent x);;
                                   ^
Error: A dependent function argument must have a stable binding; bind the current value with [let] first
|}]

let mutable_snapshot () =
  let mutable x = 0 in
  x <- 1;
  let snapshot = x in
  ignore (explicitly_dependent snapshot);;
[%%expect{|
val mutable_snapshot : unit -> unit = <fun>
|}]
let recursive_mode () =
  let rec (once @ once) : (x : int) -> {y : int | eq y x} -> int =
    fun x y -> let refine_ y = y in add x y
  in
  let x = 3 in
  let y : {y : int | eq y x} = refine_ x in
  once x y;;
[%%expect{|
val recursive_mode : unit -> int = <fun>
|}]

module Explicit_return_mode = struct
  let rec f : ((x : int) ->
      (unit -> {y : int | eq y x}) @ once) =
    fun x () -> refine_ x
  let call x =
    let result = f x in
    let _ @ many = result in ()
end;;
[%%expect{|
Line 7, characters 19-25:
7 |     let _ @ many = result in ()
                       ^^^^^^
Error: This value is "once" but is expected to be "many".
|}]

type cell = Cell of int

let (contain_contended @ total) :
    (x : int) -> {r : cell | true} @ immutable contended =
  fun x ->
  let cell = Cell x in
  refine_ cell;;
[%%expect{|
type cell = Cell of int
val contain_contended : int -> {r : cell | true} @ immutable = <fun>
|}]

(* TEST
 has-z3;
 flags = "-extension refinement_types";
 expect;
*)

type chain = Stop | Next of chain [@@inductive];;
[%%expect{|
type chain = Stop | Next of chain [@@inductive]
|}]

let rec (proof @ total) : (xs : chain) @ immutable -> (q : int) ->
    {u : unit | true} @ ghost = fun xs q -> ghost_ (
  match xs with
  | Stop -> let u = () in refine_ u
  | Next rest -> proof rest q);;
[%%expect{|
val proof : chain @ immutable -> int -> {u : unit | true} @ ghost = <fun>
|}]

module Result_annotation = struct
  let rec (size @ total) (xs : chain) : {n : int | n >= 0} =
    match xs with
    | Stop -> let n = 0 in refine_ n
    | Next rest -> let refine_ n = size rest in refine_ n
end;;
[%%expect{|
module Result_annotation : sig val size : chain -> {n : int | n >= 0} end
|}]

let rec (bad @ total) (xs : chain) = bad xs;;
[%%expect{|
Line 1, characters 37-43:
1 | let rec (bad @ total) (xs : chain) = bad xs;;
                                         ^^^^^^
Error: This recursive function cannot be total: the recursive argument is not a known proper descendant.
|}]

let rec (countdown @ total) n = if n > 0 then countdown (n - 1) else 0
[@@decreases n];;
[%%expect{|
val countdown : int -> int = <fun>
|}]

module Dependent_result = struct
  let rec (same @ total) (xs : chain) (n : int) : {m : int | m = n} =
    match xs with
    | Stop -> refine_ n
    | Next rest -> let refine_ m = same rest n in refine_ m
end;;
[%%expect{|
module Dependent_result :
  sig val same : chain -> (n : int) -> {m : int | m = n} end
|}]

module Wrong_result = struct
  let rec (bad @ total) (xs : chain) (n : int) : {m : int | m > n} =
    match xs with
    | Stop -> refine_ n
    | Next rest -> let refine_ m = bad rest n in refine_ m
end;;
[%%expect{|
Line 4, characters 14-23:
4 |     | Stop -> refine_ n
                  ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Shadowed_result = struct
  let n = 0
  let rec (same @ total) (xs : chain) (n : int) : {m : int | m = n} =
    match xs with
    | Stop -> refine_ n
    | Next rest -> let refine_ m = same rest n in refine_ m
end;;
[%%expect{|
module Shadowed_result :
  sig val n : int val same : chain -> (n : int) -> {m : int | m = n} end
|}]

let rec (self_measure @ total) (n : int @ immutable) =
  if n > 0 then self_measure (n - 1) else 0
[@@decreases self_measure n];;
[%%expect{|
Line 3, characters 13-25:
3 | [@@decreases self_measure n];;
                 ^^^^^^^^^^^^
Error: The recursive function occurs in its own measure
Line 3, characters 13-27:
3 | [@@decreases self_measure n];;
                 ^^^^^^^^^^^^^^
  Required by this decreases attribute
|}]

let rec (refined_chain @ total) (xs : {xs : chain | true}) =
  match xs with Stop -> () | Next rest -> refined_chain rest;;
[%%expect{|
val refined_chain : {xs : chain | true} -> unit = <fun>
|}]

let rec (refined_cycle @ total) (xs : {xs : chain | true}) =
  refined_cycle xs;;
[%%expect{|
Line 2, characters 2-18:
2 |   refined_cycle xs;;
      ^^^^^^^^^^^^^^^^
Error: This recursive function cannot be total: the recursive argument is not a known proper descendant.
|}]

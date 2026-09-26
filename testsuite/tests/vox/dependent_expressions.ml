(* TEST
 has-z3;
 flags = "-extension refinement_types";
 expect;
*)

module Values = struct
  let identity : (x : int) -> {r : int | r = x} = fun x -> refine_ x
  type box = { value : int }
  let literal () : {r : int | r = 42} =
    let refine_ r = identity 42 in refine_ r
  let field : (b : box) -> {r : int | r = b.value} = fun b ->
    let refine_ r = identity b.value in refine_ r
end;;
[%%expect{|
module Values :
  sig
    val identity : (x : int) -> {r : int | r = x}
    type box = { value : int; }
    val literal : unit -> {r : int | r = 42}
    val field : (b : box) -> {r : int | r = b.value}
  end
|}]

module Mutable_field = struct
  type box = { mutable value : int }
  let get b = Values.identity b.value
end;;
[%%expect{|
module Mutable_field :
  sig type box = { mutable value : int; } val get : box -> int end
|}]

module Effectful = struct
  let get () = Values.identity (print_endline "bad"; 42)
end;;
[%%expect{|
module Effectful : sig val get : unit -> int end
|}]

(* The effectful argument is evaluated exactly once. *)
let effectful_once =
  let count = ref 0 in
  let r = Values.identity (incr count; 42) in
  r, !count;;
[%%expect{|
val effectful_once : int * int = (42, 1)
|}]

(* The mutable field is read once; the result is not tied to later values of
   the field. *)
module Mutable_field_snapshot = struct
  type box = { mutable value : int }
  let stale (b : box) =
    let r = Values.identity b.value in
    b.value <- r + 1;
    let now = b.value in
    let (_ : {n : int | n = now}) = r in
    ()
end;;
[%%expect{|
Line 7, characters 36-37:
7 |     let (_ : {n : int | n = now}) = r in
                                        ^
Error: Refinement could not be proved (counterexample)
|}]

(* An effectful argument is not identified with a second evaluation. *)
module Effectful_not_repeated = struct
  let twice (f : unit -> int) =
    let r = Values.identity (f ()) in
    let again = f () in
    let (_ : {n : int | n = again}) = r in
    ()
end;;
[%%expect{|
Line 5, characters 38-39:
5 |     let (_ : {n : int | n = again}) = r in
                                          ^
Error: Refinement could not be proved (counterexample)
|}]

module More = struct
  type box = #{ value : int }
  let field : (b : box) -> {r : int | r = b.#value} = fun b ->
    let refine_ r = Values.identity b.#value in refine_ r
  let choose : (x : int) -> (y : int) -> {r : int | r = y} =
    fun _ y -> refine_ y
  let nested () : {r : int | r = 2} =
    let f = choose 1 in let refine_ r = f 2 in refine_ r
  let original : (x : int) -> {r : int | r = x} = Values.identity
end;;
[%%expect{|
module More :
  sig
    type box = #{ value : int; }
    val field : (b : box) -> {r : int | r = b.#value}
    val choose : int -> (y : int) -> {r : int | r = y}
    val nested : unit -> {r : int | r = 2}
    val original : (x : int) -> {r : int | r = x}
  end
|}]

module Wrong_literal = struct
  let wrong () : {r : int | r = 43} =
    let refine_ r = Values.identity 42 in refine_ r
end;;
[%%expect{|
Line 3, characters 42-51:
3 |     let refine_ r = Values.identity 42 in refine_ r
                                              ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

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
Line 3, characters 30-37:
3 |   let get b = Values.identity b.value
                                  ^^^^^^^
Error: A dependent argument must be a stable variable, literal, or immutable field projection
|}]

module Effectful = struct
  let get () = Values.identity (print_endline "bad"; 42)
end;;
[%%expect{|
Line 2, characters 31-56:
2 |   let get () = Values.identity (print_endline "bad"; 42)
                                   ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: A dependent argument must be a stable variable, literal, or immutable field projection
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

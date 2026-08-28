(* TEST
 flags = "-extension refinement_types";
 has-z3;
 { expect; }
 { expect.opt; }
*)

external ( = ) : int -> int -> bool @@ total = "%equal"
external ( > ) : int -> int -> bool @@ total = "%greaterthan";;
[%%expect{|
external ( = ) : int -> int -> bool = "%equal"
external ( > ) : int -> int -> bool = "%greaterthan"
|}]

module Definitions = struct
  let[@def] next x = x + 2
  let[@def] choose b x y = if b then x + 1 else y - 1
end
open Definitions;;
[%%expect{|
module Definitions :
  sig
    val next : int -> int
    val next_def : (x : int) -> {u : unit | (next x) === (x + 2)}
    val choose : bool -> int -> int -> int
    val choose_def :
      (b : bool) ->
      (x : int) ->
      (y : int) ->
      {u : unit | (choose b x y) === (if b then x + 1 else y - 1)}
  end
|}]

let unfolded () : {n : int | n = 5} =
  let x = 3 in
  let y = next x in
  let refine_ proof = next_def x in
  refine_ y;;
[%%expect{|
val unfolded : unit -> {n : int | n = 5} = <fun>
|}]

let opaque () : {n : int | n = 5} =
  let x = 3 in
  let y = next x in
  refine_ y;;
[%%expect{|
Line 4, characters 2-11:
4 |   refine_ y;;
      ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let not_eliminated () : {n : int | n = 5} =
  let x = 3 in
  let y = next x in
  let proof = next_def x in
  refine_ y;;
[%%expect{|
Line 4, characters 6-11:
4 |   let proof = next_def x in
          ^^^^^
Warning 26 [unused-var]: unused variable "proof".

Line 5, characters 2-11:
5 |   refine_ y;;
      ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let alias () : {n : int | n = 5} =
  let g = next in
  let x = 3 in
  let y = g x in
  let refine_ proof = next_def x in
  refine_ y;;
[%%expect{|
val alias : unit -> {n : int | n = 5} = <fun>
|}]

let local_capture (x : int) : {n : int | n = x + 3} =
  let offset = 3 in
  let[@def] add (y : int) = y + offset in
  let result : int = add x in
  let refine_ proof = add_def x in
  refine_ result;;
[%%expect{|
val local_capture : (x : int) -> {n : int | n = (x + 3)} = <fun>
|}]

let shadowing () : {n : int | n = 1} =
  let[@def] f x = x + 1 in
  let x = 0 in
  let f x = x + 2 in
  let y = f x in
  let refine_ proof = f_def x in
  refine_ y;;
[%%expect{|
Line 7, characters 2-11:
7 |   refine_ y;;
      ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let multiple () : {n : int | n = 4} =
  let b = true in
  let x = 3 in
  let y = 9 in
  let result = choose b x y in
  let refine_ proof = choose_def b x y in
  refine_ result;;
[%%expect{|
val multiple : unit -> {n : int | n = 4} = <fun>
|}]

let wrapping x : {n : int | n > x} =
  let result = next x in
  let refine_ proof = next_def x in
  refine_ result;;
[%%expect{|
Line 4, characters 2-16:
4 |   refine_ result;;
      ^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let shadowed_equality () : {n : int | n = 5} =
  let ( = ) _ _ = false in
  let[@def] f (x : int) = x + 2 in
  let x = 3 in
  let y : int = f x in
  let refine_ proof = f_def x in
  refine_ y;;
[%%expect{|
Line 2, characters 6-11:
2 |   let ( = ) _ _ = false in
          ^^^^^
Warning 26 [unused-var]: unused variable "=".

val shadowed_equality : unit -> {n : int | n = 5} = <fun>
|}]

let partial_calls (f : int -> int) (x : int) : {n : int | n = 0} =
  let a = f x in
  let (_ : {n : int | n = 0}) = assume_ a in
  let b = f x in
  refine_ b;;
[%%expect{|
Line 5, characters 2-11:
5 |   refine_ b;;
      ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let total_calls (f @ total) (x : int) : {n : int | n = 0} =
  let a = f x in
  let (_ : {n : int | n = 0}) = assume_ a in
  let b = f x in
  refine_ b;;
[%%expect{|
val total_calls : (int -> int @ total) @ total -> int -> {n : int | n = 0} =
  <fun>
|}]

external stateful_total_read : int ref -> int @@ stateful total = "%field0"

let stateful_total_calls_are_fresh r : {n : int | n = 0} =
  let a = stateful_total_read r in
  r := a + 1;
  let b = stateful_total_read r in
  let n = a - b in
  refine_ n;;
[%%expect{|
external stateful_total_read : int ref -> int = "%field0"
Line 8, characters 2-11:
8 |   refine_ n;;
      ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let closure_instances make : {n : int | n = 1} =
  let one = 1 in
  let two = 2 in
  let (a @ total) = make one in
  let (b @ total) = make two in
  let x = 0 in
  let y = a x in
  let (_ : {n : int | n = 1}) = assume_ y in
  let result = b x in
  refine_ result;;
[%%expect{|
Line 10, characters 2-16:
10 |   refine_ result;;
       ^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Module_alias = struct
  module M = struct let (g @ total) = Definitions.next end
  let verified () : {n : int | n = 5} =
    let x = 3 in
    let y = M.g x in
    let refine_ proof = next_def x in
    refine_ y
end;;
[%%expect{|
module Module_alias :
  sig
    module M : sig val g : int -> int end
    val verified : unit -> {n : int | n = 5}
  end
|}]

let local_module_alias () : {n : int | n = 5} =
  let module M = struct let (g @ total) = Definitions.next end in
  let x = 3 in
  let y : int = M.g x in
  let refine_ proof = next_def x in
  refine_ y;;
[%%expect{|
val local_module_alias : unit -> {n : int | n = 5} = <fun>
|}]

type wrapped = Wrap of int
let[@def] wrap x = Wrap x;;
[%%expect{|
type wrapped = Wrap of int
val wrap : int -> wrapped = <fun>
val wrap_def : (x : int) -> {u : unit | (wrap x) === (Wrap x)} = <fun>
|}]

let translated_datatype_definition_lemma (x : int) :
    {n : int | n = x} =
  let refine_ proof = wrap_def x in
  let n = 0 in
  refine_ n;;
[%%expect{|
Line 5, characters 2-11:
5 |   refine_ n;;
      ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Datatype_definition = struct
  let[@def] (wrapped_zero @ total stateless) (_wrapped : wrapped) = 0
  let wrapped_argument = Wrap 1

  let use () : {n : int | n = 0} =
    let (wrapped @ total stateless) = wrapped_argument in
    let refine_ proof = wrapped_zero_def wrapped in
    let n = wrapped_zero wrapped in
    refine_ n
end;;
[%%expect{|
module Datatype_definition :
  sig
    val wrapped_zero : wrapped -> int
    val wrapped_zero_def :
      (_wrapped : wrapped) -> {u : unit | (wrapped_zero _wrapped) === 0}
    val wrapped_argument : wrapped
    val use : unit -> {n : int | n = 0}
  end
|}]

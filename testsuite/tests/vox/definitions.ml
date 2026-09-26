(* TEST
 flags = "-extension refinement_types";
 has-z3;
 { expect; }
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
  let _ = next_def x in
  y;;
[%%expect{|
val unfolded : unit -> {n : int | n = 5} = <fun>
|}]

let opaque () : {n : int | n = 5} =
  let x = 3 in
  let y = next x in
  y;;
[%%expect{|
Line 4, characters 2-3:
4 |   y;;
      ^
Error: Refinement could not be proved (counterexample)
|}]

let not_eliminated () : {n : int | n = 5} =
  let x = 3 in
  let y = next x in
  let _ = next_def x in
  y;;
[%%expect{|
val not_eliminated : unit -> {n : int | n = 5} = <fun>
|}]

let alias () : {n : int | n = 5} =
  let g = next in
  let x = 3 in
  let y = g x in
  let _ = next_def x in
  y;;
[%%expect{|
val alias : unit -> {n : int | n = 5} = <fun>
|}]

let local_capture (x : int) : {n : int | n = x + 3} =
  let offset = 3 in
  let[@def] add (y : int) = y + offset in
  let result : int = add x in
  let _ = add_def x in
  result;;
[%%expect{|
val local_capture : (x : int) -> {n : int | n = (x + 3)} = <fun>
|}]

let shadowing () : {n : int | n = 1} =
  let[@def] f x = x + 1 in
  let x = 0 in
  let f x = x + 2 in
  let y = f x in
  let _ = f_def x in
  y;;
[%%expect{|
Line 7, characters 2-3:
7 |   y;;
      ^
Error: Refinement could not be proved (counterexample)
|}]

let multiple () : {n : int | n = 4} =
  let b = true in
  let x = 3 in
  let y = 9 in
  let result = choose b x y in
  let _ = choose_def b x y in
  result;;
[%%expect{|
val multiple : unit -> {n : int | n = 4} = <fun>
|}]

let wrapping x : {n : int | n > x} =
  let result = next x in
  let _ = next_def x in
  result;;
[%%expect{|
Line 4, characters 2-8:
4 |   result;;
      ^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let shadowed_equality () : {n : int | n = 5} =
  let ( = ) _ _ = false in
  let[@def] f (x : int) = x + 2 in
  let x = 3 in
  let y : int = f x in
  let _ = f_def x in
  y;;
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
  b;;
[%%expect{|
Line 5, characters 2-3:
5 |   b;;
      ^
Error: Refinement could not be proved (counterexample)
|}]

let total_calls (f @ total) (x : int) : {n : int | n = 0} =
  let a = f x in
  let (_ : {n : int | n = 0}) = assume_ a in
  let b = f x in
  b;;
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
  n;;
[%%expect{|
external stateful_total_read : int ref -> int = "%field0"
Line 8, characters 2-3:
8 |   n;;
      ^
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
  result;;
[%%expect{|
Line 10, characters 2-8:
10 |   result;;
       ^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Module_alias = struct
  module M = struct let (g @ total) = Definitions.next end
  let verified () : {n : int | n = 5} =
    let x = 3 in
    let y = M.g x in
    let _ = next_def x in
    y
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
  let _ = next_def x in
  y;;
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
  let _ = wrap_def x in
  let n = 0 in
  n;;
[%%expect{|
Line 5, characters 2-3:
5 |   n;;
      ^
Error: Refinement could not be proved (counterexample)
|}]

module Datatype_definition = struct
  let[@def] (wrapped_zero @ total stateless) (_wrapped : wrapped) = 0
  let wrapped_argument = Wrap 1

  let use () : {n : int | n = 0} =
    let (wrapped @ total stateless) = wrapped_argument in
    let _ = wrapped_zero_def wrapped in
    let n = wrapped_zero wrapped in
    n
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

module Polymorphic = struct
  let[@def] identity (x : 'a @ immutable) = x

  let (identity_law @ total) :
      (x : ('a : immutable_data)) -> {u : unit | identity x === x} =
    fun x ->
    let _ = identity_def x in
    ()

  let integer (x : int) : {y : int | y === x} =
    let result = identity x in
    let _ = identity_def x in
    result

  let boolean (x : bool) : {y : bool | y === x} =
    let result = identity x in
    let _ = identity_def x in
    result
end
;;
[%%expect{|
module Polymorphic :
  sig
    val identity : 'a @ immutable -> 'a @ immutable
    val identity_def :
      (x : 'a) @ immutable -> {u : unit | (identity x) === x}
    val identity_law :
      ('a : immutable_data). (x : 'a) -> {u : unit | (identity x) === x}
    val integer : (x : int) -> {y : int | y === x}
    val boolean : (x : bool) -> {y : bool | y === x}
  end
|}]

module Let_patterns = struct
  let[@def] sum (pair : int * int) =
    let x, pair = pair in x + pair

  let[@def] nested (pair : int * (int * int)) =
    let x, (y, z) = pair in x + y + z

  type fields = { left : int; right : int }

  let[@def] fields_sum (fields : fields) =
    let { left; right } = fields in left + right
end;;
[%%expect{|
module Let_patterns :
  sig
    val sum : int * int -> int
    val sum_def :
      (pair' : (int * int)) ->
      {u : unit | (sum pair') === (match pair' with | (x, pair) -> x + pair)}
    val nested : int * (int * int) -> int
    val nested_def :
      (pair : (int * (int * int))) ->
      {u : unit
        | (nested pair) === (match pair with | (x, (y, z)) -> (x + y) + z)}
    type fields = { left : int; right : int; }
    val fields_sum : fields -> int
    val fields_sum_def :
      (fields : fields) ->
      {u : unit
        | (fields_sum fields) ===
            (match fields with | { left; right } -> left + right)}
  end
|}]

let tuple_let_unfold () : {n : int | n = 7} =
  let pair = 3, 4 in
  let result = Let_patterns.sum pair in
  let _ = Let_patterns.sum_def pair in
  result;;
[%%expect{|
val tuple_let_unfold : unit -> {n : int | n = 7} = <fun>
|}]

let tuple_let_wrong () : {n : int | n = 8} =
  let pair = 3, 4 in
  let result = Let_patterns.sum pair in
  let _ = Let_patterns.sum_def pair in
  result;;
[%%expect{|
Line 5, characters 2-8:
5 |   result;;
      ^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let[@def] partial_let (values : int list) =
  let head :: _ = values in head;;
[%%expect{|
Line 2, characters 2-32:
2 |   let head :: _ = values in head;;
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "[]"

Line 2, characters 2-32:
2 |   let head :: _ = values in head;;
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The expression is "partial"
       but is expected to be "total"
         because it is used inside the function at lines 1-2, characters 22-32
         which is expected to be "total".
|}]

let tuple_let_predicate (pair : int * int) :
    {n : int | let x, y = pair in n = x + y} =
  let x, y = pair in
  let result = x + y in
  result;;
[%%expect{|
val tuple_let_predicate :
  (pair : (int * int)) -> {n : int | match pair with | (x, y) -> n = (x + y)} =
  <fun>
|}]

let nested_let_unfold () : {n : int | n = 12} =
  let pair = 3, (4, 5) in
  let result = Let_patterns.nested pair in
  let _ = Let_patterns.nested_def pair in
  result;;
[%%expect{|
val nested_let_unfold : unit -> {n : int | n = 12} = <fun>
|}]

let record_let_unfold () : {n : int | n = 7} =
  let fields = { Let_patterns.left = 3; right = 4 } in
  let result = Let_patterns.fields_sum fields in
  let _ = Let_patterns.fields_sum_def fields in
  result;;
[%%expect{|
val record_let_unfold : unit -> {n : int | n = 7} = <fun>
|}]

module Function_alias = struct
  type callback = int -> int
  type curried = int -> callback

  let[@def] (identity @ total) (x : int) =
    let f : callback @ total = fun y -> y in
    f x

  let[@def] (first @ total) (x : int) (y : int) =
    let f : curried @ total = fun a b -> a in
    f x y
end;;
[%%expect{|
module Function_alias :
  sig
    type callback = int -> int
    type curried = int -> callback
    val identity : int -> int
    val identity_def :
      (x : int) ->
      {u : unit
        | (identity x) ===
            (let (f : callback) = (fun y -> y : callback) in f x)}
    val first : int -> int -> int
    val first_def :
      (x : int) ->
      (y : int) ->
      {u : unit
        | (first x y) ===
            (let (f : curried) = (fun a -> fun b -> a : curried) in f x y)}
  end
|}]

module Local_definition = struct
let[@def] (local_empty @ total) (xs : int list @ local immutable) =
  match xs with [] -> true | _ -> false
let (local_lemma @ total) (xs : int list @ local immutable) :
    {u : unit | local_empty xs === (match xs with [] -> true | _ -> false)} =
  local_empty_def xs;
  ()
end;;
[%%expect{|
module Local_definition :
  sig
    val local_empty : int list @ local immutable -> bool
    val local_empty_def :
      (xs : int list) @ local forkable unyielding immutable ->
      {u : unit
        | (local_empty xs) === (match xs with | [] -> true | _ -> false)}
    val local_lemma :
      (xs : int list) @ local immutable ->
      {u : unit
        | (local_empty xs) === (match xs with | [] -> true | _ -> false)}
  end
|}]

let local_function_lemma () : {n : int | n = 1} =
  Function_alias.identity_def 1;
  Function_alias.identity 1;;
[%%expect{|
val local_function_lemma : unit -> {n : int | n = 1} = <fun>
|}]

module Captured_function = struct
  let[@def] (add @ total) (x : int) (y : int) =
    let f = fun z -> x + z in
    f y
end;;
[%%expect{|
module Captured_function :
  sig
    val add : int -> int -> int
    val add_def :
      (x : int) ->
      (y : int) -> {u : unit | (add x y) === (let f z = x + z in f y)}
  end
|}]

let captured_function_lemma () : {n : int | n = 7} =
  Captured_function.add_def 3 4;
  Captured_function.add 3 4;;
[%%expect{|
val captured_function_lemma : unit -> {n : int | n = 7} = <fun>
|}]

let curried_function_lemma () : {n : int | n = 3} =
  Function_alias.first_def 3 4;
  Function_alias.first 3 4;;
[%%expect{|
val curried_function_lemma : unit -> {n : int | n = 3} = <fun>
|}]

let false_captured_function () : {n : int | n = 8} =
  Captured_function.add_def 3 4;
  Captured_function.add 3 4;;
[%%expect{|
Line 3, characters 2-27:
3 |   Captured_function.add 3 4;;
      ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let false_curried_function () : {n : int | n = 4} =
  Function_alias.first_def 3 4;
  Function_alias.first 3 4;;
[%%expect{|
Line 3, characters 2-26:
3 |   Function_alias.first 3 4;;
      ^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Partial_function = struct
  let[@def] (call @ total) (x : int) =
    let f y = if y = 0 then failwith "zero" else y in
    f x
end;;
[%%expect{|
Line 3, characters 28-36:
3 |     let f y = if y = 0 then failwith "zero" else y in
                                ^^^^^^^^
Error: The value "failwith" is "partial"
       but is expected to be "total"
         because it is used inside the function at lines 2-4, characters 27-7
         which is expected to be "total".
|}]

module Mutable_function = struct
  let[@def] (read @ total) (x : int ref) =
    let f () = !x in
    f ()
end;;
[%%expect{|
Line 3, characters 15-16:
3 |     let f () = !x in
                   ^
Error: The value "(!)" is "partial"
       but is expected to be "total"
         because it is used inside the function at lines 2-4, characters 27-8
         which is expected to be "total".
|}]

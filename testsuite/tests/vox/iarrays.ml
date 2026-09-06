(* TEST
 has-z3;
 {
   flags = "-extension refinement_types";
   { expect; }
   { expect.opt; }
 }{
   flags = "-extension refinement_types -principal";
   { expect; }
   { expect.opt; }
 }
*)

let literal () : {n : int | n = 30} =
  let values = [: 10; 20; 30; 40 :] in
  let index = 2 in
  let one : {i : int | 0 <= i && i < Iarray.length values} = refine_ index in
  let n = Iarray.Refined.get values one in
  refine_ n
;;

let aliased_get_literal () : {n : int | n = 20} =
  let get = Iarray.get in
  let n = get [: 10; 20; 30 :] 1 in
  refine_ n
;;

let normal_return_bounds
    (values : int iarray)
    index : {i : int | 0 <= i && i < Iarray.length values} =
  let _ = Iarray.get values index in
  refine_ index
;;

let nested () : {n : int | n = 7} =
  let rows = [: [: 3; 5 :]; [: 7; 11 :] :] in
  let one = 1 in
  let row_index : {i : int | 0 <= i && i < Iarray.length rows} = refine_ one in
  let row = Iarray.Refined.get rows row_index in
  let zero = 0 in
  let column_index : {i : int | 0 <= i && i < Iarray.length row} = refine_ zero in
  let n = Iarray.Refined.get row column_index in
  refine_ n
;;

external raw_length : int iarray -> int = "%array_length"
external raw_get : int iarray -> int -> int = "%array_safe_get"

let raw_primitives () : {n : int | n = 20} =
  let values = [: 10; 20; 30 :] in
  let length = raw_length values in
  let (_ : {n : int | n = 3}) = refine_ length in
  let n = raw_get values 1 in
  refine_ n
;;
[%%expect{|
val literal : unit -> {n : int | n = 30} = <fun>
val aliased_get_literal : unit -> {n : int | n = 20} = <fun>
val normal_return_bounds :
  (values : int iarray) ->
  int @ total -> {i : int | (0 <= i) && (i < (Iarray.length values))} = <fun>
val nested : unit -> {n : int | n = 7} = <fun>
external raw_length : int iarray -> int = "%array_length"
external raw_get : int iarray -> int -> int = "%array_safe_get"
val raw_primitives : unit -> {n : int | n = 20} = <fun>
|}]

let caught_read (values : int iarray) index :
    {i : int | 0 <= i && i < Iarray.length values} =
  (try ignore (Iarray.get values index) with Invalid_argument _ -> ());
  refine_ index
;;
[%%expect{|
Line 4, characters 2-15:
4 |   refine_ index
      ^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let nonnegative_length (values : 'a iarray) : {n : int | 0 <= n} =
  let n = Iarray.length values in
  refine_ n
;;

let polymorphic_literal x : {n : int | n = 2} =
  let values = [: x; x :] in
  let n = Iarray.length values in
  refine_ n
;;
[%%expect{|
val nonnegative_length :
  ('a : value_maybe_null). 'a iarray -> {n : int | 0 <= n} = <fun>
val polymorphic_literal : ('a : value_maybe_null). 'a -> {n : int | n = 2} =
  <fun>
|}]

let unsupported_elements () : {n : int | n = 2} =
  let values = [: (fun x -> x + 1); (fun x -> x - 1) :] in
  let n = Iarray.length values in
  refine_ n
;;

type element = First | Second

let datatype_literal () : {x : element | x === Second} =
  let x = Iarray.get [: First; Second :] 1 in
  refine_ x
;;
[%%expect{|
val unsupported_elements : unit -> {n : int | n = 2} = <fun>
type element = First | Second
val datatype_literal : unit -> {x : element | x === Second} = <fun>
|}]

let equality_goal (values : int iarray) :
    {result : int iarray | result === values} =
  let result = values in
  refine_ result
;;
[%%expect{|
Line 2, characters 27-44:
2 |     {result : int iarray | result === values} =
                               ^^^^^^^^^^^^^^^^^
Error: Unsupported refinement predicate in VC generation
Line 4, characters 2-16:
4 |   refine_ result
      ^^^^^^^^^^^^^^
  Required by this refinement introduction
|}]

type recursive_container = Empty | More of int iarray * recursive_container
[@@inductive]

let recursive_equality_goal (value : recursive_container) :
    {result : recursive_container | result === value} =
  let result = value in
  refine_ result
;;
[%%expect{|
type recursive_container = Empty | More of int iarray * recursive_container
[@@inductive]
Line 5, characters 36-52:
5 |     {result : recursive_container | result === value} =
                                        ^^^^^^^^^^^^^^^^
Error: Unsupported refinement predicate in VC generation
Line 7, characters 2-16:
7 |   refine_ result
      ^^^^^^^^^^^^^^
  Required by this refinement introduction
|}]

let invalid_refined_get () =
  let values = [: 1 :] in
  let index = 1 in
  let bounded : {i : int | 0 <= i && i < Iarray.length values} =
    refine_ index
  in
  Iarray.Refined.get values bounded
;;
[%%expect{|
Line 5, characters 4-17:
5 |     refine_ index
        ^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let refined_get_unboxed () =
  let values = [: #1.; #2. :] in
  let index = 0 in
  let bounded : {i : int | 0 <= i && i < Iarray.length values} =
    refine_ index
  in
  let _ = Iarray.Refined.get values bounded in
  ()
;;
[%%expect{|
val refined_get_unboxed : unit -> unit = <fun>
|}]

module Refined_get_preserves_access : sig end = struct
  type value = { mutable payload : int }

  let update :
      (values : value iarray) ->
      {index : int | 0 <= index && index < Iarray.length values} ->
      unit =
    fun values index ->
      (Iarray.Refined.get values index).payload <- 1
end;;
[%%expect{|
module Refined_get_preserves_access : sig end
|}]

module Refined_get_rejects_partial_container : sig end = struct
  let partial () = failwith "partial"

  let rejected () =
    let values = [: partial :] in
    let zero = 0 in
    let index : {i : int | 0 <= i && i < Iarray.length values} = refine_ zero in
    Iarray.Refined.get values index
end;;
[%%expect{|
Line 7, characters 55-61:
7 |     let index : {i : int | 0 <= i && i < Iarray.length values} = refine_ zero in
                                                           ^^^^^^
Error: The value "values" is "partial"
         because it is an array that contains the expression at line 5, characters 20-27
         which is "partial".
       However, the value "values" highlighted is expected to be "total"
         because it is used in an expression (at line 7, characters 27-61).
|}]

let wrong_length () : {n : int | n = 2} =
  let n = Iarray.length [: 1; 2; 3 :] in
  refine_ n
;;
[%%expect{|
Line 3, characters 2-11:
3 |   refine_ n
      ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let wrong_element () : {n : int | n = 4} =
  let n = Iarray.get [: 1; 2; 3 :] 1 in
  refine_ n
;;
[%%expect{|
Line 3, characters 2-11:
3 |   refine_ n
      ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let mutable_array_stays_opaque () : {n : int | n = 3} =
  let n = Array.length [| 1; 2; 3 |] in
  refine_ n
;;
[%%expect{|
Line 3, characters 2-11:
3 |   refine_ n
      ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Stdlib__Iarray = struct
  let (length @ total) (_ : int iarray) = 0
end

let shadow_module_is_not_stdlib () : {n : int | n = 3} =
  let n = Stdlib__Iarray.length [: 1; 2; 3 :] in
  refine_ n
;;
[%%expect{|
module Stdlib__Iarray : sig val length : int iarray -> int end
Line 7, characters 2-11:
7 |   refine_ n
      ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

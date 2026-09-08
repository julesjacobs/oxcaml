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
val equality_goal :
  (values : int iarray) -> {result : int iarray | result === values} = <fun>
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
val recursive_equality_goal :
  (value : recursive_container) ->
  {result : recursive_container | result === value} = <fun>
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

let slice_contents (source : int iarray) (position : int) (size : int)
    (index : int) =
  let result = Iarray.sub source ~pos:position ~len:size in
  let value = Iarray.get result index in
  let expected = Iarray.get source (position + index) in
  let certificate : {n : int | n = expected} = refine_ value in
  let refine_ certificate = certificate in
  ()
;;

let append_contents (left : int iarray) (right : int iarray) (index : int) =
  let result = Iarray.append left right in
  let value = Iarray.get result index in
  let expected = if index < Iarray.length left then Iarray.get left index
    else Iarray.get right (index - Iarray.length left) in
  let certificate : {n : int | n = expected} = refine_ value in
  let refine_ certificate = certificate in
  ()
;;

let copy_lengths (source : int iarray) (position : int) (size : int) :
    {length : int | length = size + 1 && 0 <= position && 0 <= size
      && position <= Iarray.length source
      && size <= Iarray.length source - position} =
  let sub = Iarray.sub in
  let append = Iarray.append in
  let slice = sub source ~pos:position ~len:size in
  let result = append slice [: 42 :] in
  let length = Iarray.length result in
  refine_ length
;;

let copy_composition () : {value : int | value = 20} =
  let source = [: 10; 20; 30 :] in
  let result = Iarray.append [: 0 :] (Iarray.sub source ~pos:1 ~len:2) in
  let value = Iarray.get result 1 in
  refine_ value
;;

let conditional_copy (choose : bool) :
    {value : int | value = (if choose then 20 else 40)} =
  let result = if choose then Iarray.sub [: 10; 20 :] ~pos:1 ~len:1
    else Iarray.append [: :] [: 40 :] in
  let value = Iarray.get result 0 in
  refine_ value
;;
[%%expect{|
val slice_contents : int iarray -> int -> int -> int -> unit = <fun>
val append_contents : int iarray -> int iarray -> int -> unit = <fun>
val copy_lengths :
  (source : int iarray) ->
  (position : int) ->
  (size : int) ->
  {length : int
    | (length = (size + 1)) &&
        ((0 <= position) &&
           ((0 <= size) &&
              ((position <= (Iarray.length source)) &&
                 (size <= ((Iarray.length source) - position)))))} =
  <fun>
val copy_composition : unit -> {value : int | value = 20} = <fun>
val conditional_copy :
  (choose : bool) -> {value : int | value = (if choose then 20 else 40)} =
  <fun>
|}]

let caught_slice (source : int iarray) (position : int) (size : int) :
    {result : int | 0 <= result} =
  (try ignore (Iarray.sub source ~pos:position ~len:size)
   with Invalid_argument _ -> ());
  refine_ position
;;
[%%expect{|
Line 5, characters 2-18:
5 |   refine_ position
      ^^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let wrong_copy () : {value : int | value = 10} =
  let result = Iarray.sub [: 10; 20 :] ~pos:1 ~len:1 in
  let value = Iarray.get result 0 in
  refine_ value
;;
[%%expect{|
Line 4, characters 2-15:
4 |   refine_ value
      ^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]


let shared_copies () : {length : int | length = 0} =
  let array : int iarray = [: :] in
  let array = Iarray.append array array in
  let array = Iarray.append array array in
  let array = Iarray.append array array in
  let array = Iarray.append array array in
  let array = Iarray.append array array in
  let array = Iarray.append array array in
  let array = Iarray.append array array in
  let array = Iarray.append array array in
  let array = Iarray.append array array in
  let array = Iarray.append array array in
  let array = Iarray.append array array in
  let array = Iarray.append array array in
  let array = Iarray.append array array in
  let array = Iarray.append array array in
  let array = Iarray.append array array in
  let array = Iarray.append array array in
  let array = Iarray.append array array in
  let array = Iarray.append array array in
  let array = Iarray.append array array in
  let array = Iarray.append array array in
  let array = Iarray.append array array in
  let array = Iarray.append array array in
  let array = Iarray.append array array in
  let array = Iarray.append array array in
  let array = Iarray.append array array in
  let length = Iarray.length array in
  refine_ length
;;
[%%expect{|
val shared_copies : unit -> {length : int | length = 0} = <fun>
|}]

let shared_contents (index : int) : {value : int | value = 7} =
  let array = [: 7 :] in
  let array = Iarray.append array array in
  let array = Iarray.append array array in
  let array = Iarray.append array array in
  let array = Iarray.append array array in
  let array = Iarray.append array array in
  let array = Iarray.append array array in
  let array = Iarray.append array array in
  let array = Iarray.append array array in
  let array = Iarray.append array array in
  let array = Iarray.append array array in
  let array = Iarray.append array array in
  let array = Iarray.append array array in
  let array = Iarray.append array array in
  let array = Iarray.append array array in
  let array = Iarray.append array array in
  let value = Iarray.get array index in
  refine_ value
;;
[%%expect{|
val shared_contents : int -> {value : int | value = 7} = <fun>
|}]


let copied_alias () : {value : int | value = 7} =
  let slice = Iarray.sub [: 7 :] ~pos:0 ~len:1 in
  let row = Iarray.get [: slice :] 0 in
  let length = Iarray.length row in
  let (_ : {n : int | n = 1}) = refine_ length in
  let value = Iarray.get row 0 in
  refine_ value
;;
[%%expect{|
val copied_alias : unit -> {value : int | value = 7} = <fun>
|}]

module Record_bounds = struct
  type t = { data : int iarray }

  let get : (container : t) ->
      {i : int | 0 <= i && i < Iarray.length container.data} -> int =
    fun container index ->
    let data = container.data in
    let refine_ index = index in
    let bounded : {i : int | 0 <= i && i < Iarray.length data} =
      refine_ index in
    Iarray.Refined.get data bounded
end;;
[%%expect{|
module Record_bounds :
  sig
    type t = { data : int iarray; }
    val get :
      (container : t) ->
      {i : int | (0 <= i) && (i < (Iarray.length container.data))} -> int
  end
|}]

module Polymorphic_record_bounds = struct
  type 'a t = { data : 'a iarray }

  let get : ('a : value mod separable).
      (container : 'a t) ->
      {i : int | 0 <= i && i < Iarray.length container.data} -> 'a =
    fun container index ->
    let data = container.data in
    let refine_ index = index in
    let bounded : {i : int | 0 <= i && i < Iarray.length data} =
      refine_ index in
    Iarray.Refined.get data bounded
end;;
[%%expect{|
module Polymorphic_record_bounds :
  sig
    type 'a t = { data : 'a iarray; }
    val get :
      (container : 'a t) ->
      {i : int | (0 <= i) && (i < (Iarray.length container.data))} -> 'a
  end
|}]

module Separate_record_bounds = struct
  type 'a t = { data : 'a iarray }

  let get : ('a : value mod separable).
      (container : 'a t) -> (other : 'a t) @ total ->
      {i : int | 0 <= i && i < Iarray.length container.data} -> 'a =
    fun container other index ->
    let data = other.data in
    let refine_ index = index in
    let bounded : {i : int | 0 <= i && i < Iarray.length data} =
      refine_ index in
    Iarray.Refined.get data bounded
end;;
[%%expect{|
Line 11, characters 6-19:
11 |       refine_ index in
           ^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]


let () =
  let container = Polymorphic_record_bounds.{ data = [: 42 :] } in
  let zero = 0 in
  let index : {i : int | 0 <= i && i < Iarray.length container.data} =
    refine_ zero in
  assert (Polymorphic_record_bounds.get container index = 42);
  let container = Polymorphic_record_bounds.{ data = [: true :] } in
  let index : {i : int | 0 <= i && i < Iarray.length container.data} =
    refine_ zero in
  assert (Polymorphic_record_bounds.get container index)
;;
[%%expect{|
|}]

external equal_copy : (values : int iarray) ->
  {a : int iarray | a === values} @ total = "%obj_dup"
external equal_rows : (values : int iarray iarray) ->
  {a : int iarray iarray | a === values} @ total = "%obj_dup";;
[%%expect{|
external equal_copy :
  (values : int iarray) -> {a : int iarray | a === values} @ total
  = "%obj_dup"
external equal_rows :
  (values : int iarray iarray) ->
  {a : int iarray iarray | a === values} @ total = "%obj_dup"
|}]

external read_int : int iarray -> int -> int @ total = "%array_safe_get"
external read_row : int iarray iarray -> int -> int iarray @ total =
  "%array_safe_get";;
[%%expect{|
external read_int : int iarray -> int -> int @ total = "%array_safe_get"
external read_row : int iarray iarray -> int -> int iarray @ total
  = "%array_safe_get"
|}]

let equal_array_read () : {n : int | n = 20} =
  let source = [: 10; 20 :] in
  let refine_ values = equal_copy source in
  let n = read_int values 1 in
  refine_ n;;
[%%expect{|
val equal_array_read : unit -> {n : int | n = 20} = <fun>
|}]

let equal_nested_read () : {n : int | n = 20} =
  let source = [: [: 10; 20 :] :] in
  let refine_ values = equal_rows source in
  let row = read_row values 0 in
  let n = read_int row 1 in
  refine_ n;;
[%%expect{|
val equal_nested_read : unit -> {n : int | n = 20} = <fun>
|}]

let unequal_arrays () =
  let first = [: 1 :] in
  let second = [: 2 :] in
  let (_ : {a : int iarray | a === second}) = refine_ first in ();;
[%%expect{|
Line 4, characters 46-59:
4 |   let (_ : {a : int iarray | a === second}) = refine_ first in ();;
                                                  ^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

(* TEST
 flags = "-extension refinement_types -extension comprehensions";
 expect;
*)

external add : int -> int -> int @@ total = "%addint"
external eq : int -> int -> bool @@ total = "%equal"
external gt : int -> int -> bool @@ total = "%greaterthan"
external same_exception : exn -> exn -> bool @@ total = "%eq";;
[%%expect{|
external add : int -> int -> int = "%addint"
external eq : int -> int -> bool = "%equal"
external gt : int -> int -> bool = "%greaterthan"
external same_exception : exn -> exn -> bool = "%eq"
|}]

let above n : { value : int | gt value n } =
  let raw = add n 1 in
  assume_ raw

type addition =
  (left : int) ->
  (right : int) ->
  { result : int | eq result (add left right) }

let add_refined : addition =
  fun left right ->
    let raw = add left right in
    assume_ raw;;
[%%expect{|
val above : (n : int) -> {value : int | gt value n} = <fun>
type addition =
    (left : int) ->
    (right : int) -> {result : int | eq result (add left right)}
val add_refined : addition = <fun>
|}]

type 'a box = { value : 'a }

type optional_result =
  (n : int) -> { value : int | gt value n } option

type tuple_result =
  (n : int) -> { value : int | gt value n } * int

type record_result =
  (n : int) -> { value : int | gt value n } box

type list_result =
  (n : int) -> { value : int | gt value n } list

type array_result =
  (n : int) -> { value : int | gt value n } array

type thunk_result =
  (n : int) -> unit -> { value : int | gt value n }

type consumer_result =
  (n : int) -> ({ value : int | gt value n } -> int)

let optional_result : optional_result = fun n -> Some (above n)
let tuple_result : tuple_result = fun n -> above n, 0
let record_result : record_result = fun n -> { value = above n }
let list_result : list_result = fun n -> [above n]
let array_result : array_result = fun n -> [|above n|]
let thunk_result : thunk_result = fun n () -> above n
let consumer_result : consumer_result =
  fun n value ->
    ignore n;
    let refine_ raw = value in
    raw;;
[%%expect{|
type 'a box = { value : 'a; }
type optional_result = (n : int) -> {value : int | gt value n} option
type tuple_result = (n : int) -> {value : int | gt value n} * int
type record_result = (n : int) -> {value : int | gt value n} box
type list_result = (n : int) -> {value : int | gt value n} list
type array_result = (n : int) -> {value : int | gt value n} array
type thunk_result = (n : int) -> unit -> {value : int | gt value n}
type consumer_result = (n : int) -> {value : int | gt value n} -> int
val optional_result : optional_result = <fun>
val tuple_result : tuple_result = <fun>
val record_result : record_result = <fun>
val list_result : list_result = <fun>
val array_result : array_result = <fun>
val thunk_result : thunk_result = <fun>
val consumer_result : consumer_result = <fun>
|}]

let consume_parameter n =
  let value = above n in
  let refine_ result = value in
  result

let use_parameter_locally n =
  ignore (above n);
  0

let preserve_outer_dependency :
  (outer : int) -> { value : int | gt value outer } =
  fun outer ->
    let inner = 0 in
    ignore inner;
    above outer

let consume_partial_application x =
  let add_x = add_refined x in
  let value = add_x x in
  let refine_ result = value in
  result

let consume_local_slot () =
  let n = 0 in
  let slot = ref None in
  slot := Some (above n);
  match !slot with
  | None -> 0
  | Some value ->
      let refine_ result = value in
      result;;
[%%expect{|
val consume_parameter : int -> int = <fun>
val use_parameter_locally : int -> int = <fun>
val preserve_outer_dependency :
  (outer : int) -> {value : int | gt value outer} = <fun>
val consume_partial_application : int -> int = <fun>
val consume_local_slot : unit -> int = <fun>
|}]

let consume_local_module () =
  let module M = struct
    let (holds @ total) x = gt x 0
  end in
  let raw = 1 in
  let value : { x : int | M.holds x } = assume_ raw in
  let refine_ result = value in
  result

let consume_local_exception () =
  let exception E in
  let raw = E in
  let value : { x : exn | same_exception x E } = assume_ raw in
  let refine_ result = value in
  match result with E -> true | _ -> false;;
[%%expect{|
val consume_local_module : unit -> int = <fun>
val consume_local_exception : unit -> bool = <fun>
|}]

let escape_parameter n = above n;;
[%%expect{|
Line 1, characters 25-32:
1 | let escape_parameter n = above n;;
                             ^^^^^^^
Error: the refinement type of this expression escapes the scope of binding "n"
|}]

type packed = Pack : 'a -> packed

let escape_gadt_existential (Pack value) =
  (assume_ value : { result : _ | true });;
[%%expect{|
type packed = Pack : 'a -> packed
Line 4, characters 2-41:
4 |   (assume_ value : { result : _ | true });;
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "{result : $a | true}"
       but an expression was expected of type "'a"
       The type constructor "$a" would escape its scope
       Hint: "$a" is an existential type bound by the constructor "Pack".
|}]

let escape_let =
  let n = 0 in
  above n;;
[%%expect{|
Line 3, characters 2-9:
3 |   above n;;
      ^^^^^^^
Error: the refinement type of this expression escapes the scope of binding "n"
|}]

let escape_alias_chain =
  let n = 0 in
  let first = above n in
  let second = first in
  let third = second in
  third;;
[%%expect{|
Line 6, characters 2-7:
6 |   third;;
      ^^^^^
Error: the refinement type of this expression escapes the scope of binding "n"
|}]

let escape_match =
  match 0 with
  | n -> above n;;
[%%expect{|
Line 3, characters 9-16:
3 |   | n -> above n;;
             ^^^^^^^
Error: the refinement type of this expression escapes the scope of binding "n"
|}]

exception With_int of int

let escape_try =
  try raise (With_int 0) with
  | With_int n -> above n;;
[%%expect{|
exception With_int of int
Line 5, characters 18-25:
5 |   | With_int n -> above n;;
                      ^^^^^^^
Error: the refinement type of this expression escapes the scope of binding "n"
|}]

let escape_branch =
  let n = 0 in
  if true then above n else above n;;
[%%expect{|
Line 3, characters 15-22:
3 |   if true then above n else above n;;
                   ^^^^^^^
Error: the refinement type of this expression escapes the scope of binding "n"
|}]

let escape_optional =
  let n = 0 in
  Some (above n);;
[%%expect{|
Line 3, characters 7-16:
3 |   Some (above n);;
           ^^^^^^^^^
Error: the refinement type of this expression escapes the scope of binding "n"
|}]

let escape_tuple =
  let n = 0 in
  above n, 0;;
[%%expect{|
Line 3, characters 2-9:
3 |   above n, 0;;
      ^^^^^^^
Error: the refinement type of this expression escapes the scope of binding "n"
|}]

let escape_record =
  let n = 0 in
  let wrapped : { item : int | gt item n } box = { value = above n } in
  wrapped;;
[%%expect{|
Line 4, characters 2-9:
4 |   wrapped;;
      ^^^^^^^
Error: the refinement type of this expression escapes the scope of binding "n"
|}]

let escape_list =
  let n = 0 in
  [above n];;
[%%expect{|
Line 3, characters 3-10:
3 |   [above n];;
       ^^^^^^^
Error: the refinement type of this expression escapes the scope of binding "n"
|}]

let escape_array =
  let n = 0 in
  [|above n|];;
[%%expect{|
Line 3, characters 4-11:
3 |   [|above n|];;
        ^^^^^^^
Error: the refinement type of this expression escapes the scope of binding "n"
|}]

let escape_polymorphic_variant =
  let n = 0 in
  `Value (above n);;
[%%expect{|
Line 3, characters 2-18:
3 |   `Value (above n);;
      ^^^^^^^^^^^^^^^^
Error: the refinement type of this expression escapes the scope of binding "n"
|}]

let escape_lazy =
  let n = 0 in
  lazy (above n);;
[%%expect{|
Line 3, characters 7-16:
3 |   lazy (above n);;
           ^^^^^^^^^
Error: the refinement type of this expression escapes the scope of binding "n"
|}]

let escape_arrow_codomain =
  let n = 0 in
  fun () -> above n;;
[%%expect{|
Line 3, characters 12-19:
3 |   fun () -> above n;;
                ^^^^^^^
Error: the refinement type of this expression escapes the scope of binding "n"
|}]

let escape_arrow_domain =
  let n = 0 in
  fun (value : { value : int | gt value n }) ->
    let refine_ raw = value in
    raw;;
[%%expect{|
Lines 3-5, characters 2-7:
3 | ..fun (value : { value : int | gt value n }) ->
4 |     let refine_ raw = value in
5 |     raw..
Error: the refinement type of this expression escapes the scope of binding "n"
|}]

let escape_through_local_slot =
  let slot = ref None in
  let n = 0 in
  slot := Some (above n);
  slot;;
[%%expect{|
Line 4, characters 15-24:
4 |   slot := Some (above n);
                   ^^^^^^^^^
Error: the refinement type of this expression escapes the scope of binding "n"
|}]

let outer_slot = ref None;;
[%%expect{|
val outer_slot : '_weak1 option ref = {contents = None}
|}]

let escape_through_outer_slot () =
  let n = 0 in
  outer_slot := Some (above n);;
[%%expect{|
Line 3, characters 21-30:
3 |   outer_slot := Some (above n);;
                         ^^^^^^^^^
Error: the refinement type of this expression escapes the scope of binding "n"
|}]

let escape_through_mutable_slot () =
  let mutable slot = None in
  let n = 0 in
  slot <- Some (above n);;
[%%expect{|
Line 4, characters 15-24:
4 |   slot <- Some (above n);;
                   ^^^^^^^^^
Error: the refinement type of this expression escapes the scope of binding "n"
|}]

let escape_function_case =
  let slot = ref None in
  let store = function
    | n -> slot := Some (above n)
  in
  store 0;
  slot;;
[%%expect{|
Line 4, characters 24-33:
4 |     | n -> slot := Some (above n)
                            ^^^^^^^^^
Error: the refinement type of this expression escapes the scope of binding "n"
|}]

let escape_for_binder =
  let slot = ref None in
  for n = 0 to 0 do
    slot := Some (above n)
  done;
  slot;;
[%%expect{|
Line 4, characters 17-26:
4 |     slot := Some (above n)
                     ^^^^^^^^^
Error: the refinement type of this expression escapes the scope of binding "n"
|}]

let escape_comprehension_binder =
  let slot = ref None in
  ignore [slot := Some (above n) for n = 0 to 0];
  slot;;
[%%expect{|
Line 3, characters 23-32:
3 |   ignore [slot := Some (above n) for n = 0 to 0];
                           ^^^^^^^^^
Error: the refinement type of this expression escapes the scope of binding "n"
|}]

let ( let* ) value continuation = continuation value

let escape_let_operator_binder =
  let slot = ref None in
  let* n = 0 in
  slot := Some (above n);
  slot;;
[%%expect{|
val ( let* ) : 'a -> ('a -> 'b) -> 'b = <fun>
Line 6, characters 15-24:
6 |   slot := Some (above n);
                   ^^^^^^^^^
Error: the refinement type of this expression escapes the scope of binding "n"
|}]

let escape_local_function =
  let (holds @ total) x = gt x 0 in
  let raw = 1 in
  let value : { x : int | holds x } = assume_ raw in
  value;;
[%%expect{|
Line 5, characters 2-7:
5 |   value;;
      ^^^^^
Error: the refinement type of this expression escapes the scope of binding "holds"
|}]

let escape_local_module =
  let module M = struct
    let (holds @ total) x = gt x 0
  end in
  let raw = 1 in
  let value : { x : int | M.holds x } = assume_ raw in
  value;;
[%%expect{|
Line 7, characters 2-7:
7 |   value;;
      ^^^^^
Error: the refinement type of this expression escapes the scope of binding "M"
|}]

let escape_local_open =
  let open struct
    let (holds @ total) x = gt x 0
  end in
  let raw = 1 in
  let value : { x : int | holds x } = assume_ raw in
  value;;
[%%expect{|
Line 7, characters 2-7:
7 |   value;;
      ^^^^^
Error: the refinement type of this expression escapes the scope of binding "holds"
|}]

let escape_local_exception =
  let exception E in
  let raw = E in
  let value : { x : exn | same_exception x E } = assume_ raw in
  value;;
[%%expect{|
Line 5, characters 2-7:
5 |   value;;
      ^^^^^
Error: the refinement type of this expression escapes the scope of binding "E"
|}]

let inner_first_argument =
  let x = 1 in
  let result =
    let y = 2 in
    add_refined y x
  in
  result;;
[%%expect{|
Line 5, characters 4-19:
5 |     add_refined y x
        ^^^^^^^^^^^^^^^
Error: the refinement type of this expression escapes the scope of binding "y"
|}]

let inner_second_argument =
  let x = 1 in
  let result =
    let y = 2 in
    add_refined x y
  in
  result;;
[%%expect{|
Line 5, characters 4-19:
5 |     add_refined x y
        ^^^^^^^^^^^^^^^
Error: the refinement type of this expression escapes the scope of binding "y"
|}]

let materialized_partial_application =
  let x = 1 in
  let result =
    let y = 2 in
    let add_y = add_refined y in
    add_y x
  in
  result;;
[%%expect{|
Line 6, characters 4-11:
6 |     add_y x
        ^^^^^^^
Error: the refinement type of this expression escapes the scope of binding "y"
|}]

let escaped_partial_application =
  let x = 1 in
  add_refined x;;
[%%expect{|
Line 3, characters 2-15:
3 |   add_refined x;;
      ^^^^^^^^^^^^^
Error: the refinement type of this expression escapes the scope of binding "x"
|}]

let escape_definition_argument () =
  let[@def] increment x = x + 1 in
  let x = 3 in
  increment_def x;;
[%%expect{|
Line 4, characters 2-17:
4 |   increment_def x;;
      ^^^^^^^^^^^^^^^
Error: the refinement type of this expression escapes the scope of binding "x"
|}]

let escape_definition_function slot =
  let[@def] increment x = x + 1 in
  slot := increment_def;;
[%%expect{|
Line 3, characters 10-23:
3 |   slot := increment_def;;
              ^^^^^^^^^^^^^
Error: the refinement type of this expression escapes the scope of binding "increment"
|}]

class virtual escape_class_let =
  let n = 0 in
  object
    method virtual value : { x : int | gt x n }
  end;;
[%%expect{|
Lines 2-5, characters 2-5:
2 | ..let n = 0 in
3 |   object
4 |     method virtual value : { x : int | gt x n }
5 |   end..
Error: the refinement type of this class expression escapes the scope of binding "n"
|}]

class escape_method_parameter =
  object
    val mutable slot = None
    method set n = slot <- Some (above n)
  end;;
[%%expect{|
Line 4, characters 32-41:
4 |     method set n = slot <- Some (above n)
                                    ^^^^^^^^^
Error: the refinement type of this expression escapes the scope of binding "n"
|}]

let use_local_object n =
  let obj =
    object
      val mutable slot = None
      method set = slot <- Some (above n)
    end
  in
  obj#set;
  0;;
[%%expect{|
val use_local_object : int -> int = <fun>
|}]

let target_only_coercion x :> { value : int | gt value x } =
  above x;;
[%%expect{|
val target_only_coercion : (x : int) -> {value : int | gt value x} = <fun>
|}]

let source_and_target_coercion x
    : { source : int | gt source x }
    :> { target : int | gt target x } =
  above x;;
[%%expect{|
val source_and_target_coercion : (x : int) -> {target : int | gt target x} =
  <fun>
|}]

let constrained_function_cases :
    (x : int) -> unit -> { value : int | gt value x } =
  fun x : (unit -> { value : int | gt value x }) -> function
  | () -> above x;;
[%%expect{|
val constrained_function_cases :
  (x : int) -> unit -> {value : int | gt value x} = <fun>
|}]

let nested_newtype x (type a) (_ : a) : { value : int | gt value x } =
  above x;;
[%%expect{|
val nested_newtype : (x : int) -> 'a -> {value : int | gt value x} = <fun>
|}]

let alpha_link x =
  let first : { first : int | gt first x } = above x in
  let second : { second : int | gt second x } = first in
  let refine_ result = second in
  result;;
[%%expect{|
val alpha_link : int @ total -> int = <fun>
|}]

let escape_target_only_coercion =
  let x = 0 in
  (above x :> { value : int | gt value x });;
[%%expect{|
Line 3, characters 2-43:
3 |   (above x :> { value : int | gt value x });;
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: the refinement type of this expression escapes the scope of binding "x"
|}]

let escape_source_and_target_coercion =
  let x = 0 in
  (above x
    : { source : int | gt source x }
    :> { target : int | gt target x });;
[%%expect{|
Lines 3-5, characters 2-38:
3 | ..(above x
4 |     : { source : int | gt source x }
5 |     :> { target : int | gt target x })..
Error: the refinement type of this expression escapes the scope of binding "x"
|}]

let escape_alpha_link =
  let x = 0 in
  let first : { first : int | gt first x } = above x in
  let second : { second : int | gt second x } = first in
  second;;
[%%expect{|
Line 5, characters 2-8:
5 |   second;;
      ^^^^^^
Error: the refinement type of this expression escapes the scope of binding "x"
|}]

let after_failed_escape = 1;;
[%%expect{|
val after_failed_escape : int = 1
|}]

type _ tag = Int : int tag

let gadt_preserves_outer_dependency :
    (x : int) -> int tag -> { value : int | gt value x } =
  fun x tag ->
    match tag with
    | Int -> above x;;
[%%expect{|
type _ tag = Int : int tag
val gadt_preserves_outer_dependency :
  (x : int) -> int tag -> {value : int | gt value x} = <fun>
|}]

let escape_constrained_function_cases slot : int -> unit =
  fun x : (unit -> unit) -> function
  | () -> slot := Some (above x);;
[%%expect{|
Line 3, characters 23-32:
3 |   | () -> slot := Some (above x);;
                           ^^^^^^^^^
Error: the refinement type of this expression escapes the scope of binding "x"
|}]

let escape_nested_newtype =
  let x = 0 in
  fun (type a) (_ : a) -> above x;;
[%%expect{|
Line 3, characters 2-33:
3 |   fun (type a) (_ : a) -> above x;;
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: the refinement type of this expression escapes the scope of binding "x"
|}]

let after_nested_failures = 2;;
[%%expect{|
val after_nested_failures : int = 2
|}]

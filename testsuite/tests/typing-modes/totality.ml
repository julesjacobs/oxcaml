(* TEST
   flags = "-w -220 -extension comprehensions";
   expect;
*)

let (id @ total) x = x
[%%expect{|
val id : 'a -> 'a = <fun>
|}]

let outside = ref 0
[%%expect{|
val outside : int ref = {contents = 0}
|}]

let value_after_effect = (outside := 1; 1 : @ total)
[%%expect{|
val value_after_effect : int = 1
|}]

let bad_closure = ((fun () -> assert false) : @ total)
[%%expect{|
Line 1, characters 30-42:
1 | let bad_closure = ((fun () -> assert false) : @ total)
                                  ^^^^^^^^^^^^
Error: The expression is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 19-43
         which is expected to be "total".
|}]

let (apply @ total) f = f ()
[%%expect{|
val apply : (unit -> 'a) -> 'a = <fun>
|}]

let partial_argument () = apply (fun () -> assert false)
[%%expect{|
val partial_argument : unit -> 'a = <fun>
|}]

let partial () = assert false
[%%expect{|
val partial : unit -> 'a = <fun>
|}]

let (captured @ total) () = partial ()
[%%expect{|
Line 1, characters 28-35:
1 | let (captured @ total) () = partial ()
                                ^^^^^^^
Error: The value "partial" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 23-38
         which is expected to be "total".
|}]

let (recursive @ total) =
  let rec loop n = if n = 0 then 0 else loop (n - 1) in
  loop
[%%expect{|
Line 3, characters 2-6:
3 |   loop
      ^^^^
Error: This value is "partial" but is expected to be "total".
|}]

let (looping @ total) () = while true do () done
[%%expect{|
Line 1, characters 27-48:
1 | let (looping @ total) () = while true do () done
                               ^^^^^^^^^^^^^^^^^^^^^
Error: The loop is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 22-48
         which is expected to be "total".
|}]

let (partial_match @ total) = function Some x -> x
[%%expect{|
Line 1, characters 30-50:
1 | let (partial_match @ total) = function Some x -> x
                                  ^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "None"

Line 1, characters 30-50:
1 | let (partial_match @ total) = function Some x -> x
                                  ^^^^^^^^^^^^^^^^^^^^
Error: The expression is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 30-50
         which is expected to be "total".
|}]

type box = { mutable value : int }
[%%expect{|
type box = { mutable value : int; }
|}]

type immutable_box = { fixed : int }
[%%expect{|
type immutable_box = { fixed : int; }
|}]

let (allocate_immutable @ total) fixed = { fixed }
[%%expect{|
val allocate_immutable : int -> immutable_box = <fun>
|}]

let (read @ total) box = box.value
[%%expect{|
Line 1, characters 25-28:
1 | let (read @ total) box = box.value
                             ^^^
Error: The expression is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 19-34
         which is expected to be "total".
|}]

let (write @ total) box = box.value <- 1
[%%expect{|
Line 1, characters 26-40:
1 | let (write @ total) box = box.value <- 1
                              ^^^^^^^^^^^^^^
Error: The expression is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 20-40
         which is expected to be "total".
|}]

let (allocate @ total) value = { value }
[%%expect{|
Line 1, characters 31-40:
1 | let (allocate @ total) value = { value }
                                   ^^^^^^^^^
Error: The expression is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 23-40
         which is expected to be "total".
|}]

let (return_mutable @ total) (box : box) = box
[%%expect{|
val return_mutable : box -> box = <fun>
|}]

let captured_box = { value = 0 }
[%%expect{|
val captured_box : box = {value = 0}
|}]

let (capture_mutable @ total) () = captured_box
[%%expect{|
val capture_mutable : unit -> box @ immutable = <fun>
|}]

let (addition @ total) x = x + 1
[%%expect{|
val addition : int -> int = <fun>
|}]

let (negation @ total) x = not x
[%%expect{|
val negation : bool -> bool = <fun>
|}]

let (division @ total) x = x / 1
[%%expect{|
Line 1, characters 29-30:
1 | let (division @ total) x = x / 1
                                 ^
Error: The value "(/)" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 23-32
         which is expected to be "total".
|}]

let (allocate_ref @ total) x = ref x
[%%expect{|
Line 1, characters 31-34:
1 | let (allocate_ref @ total) x = ref x
                                   ^^^
Error: The value "ref" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 27-36
         which is expected to be "total".
|}]

let (allocate_array @ total) x = [|x|]
[%%expect{|
Line 1, characters 33-38:
1 | let (allocate_array @ total) x = [|x|]
                                     ^^^^^
Error: The expression is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 29-38
         which is expected to be "total".
|}]

let (allocate_iarray @ total) x : _ iarray = [|x|]
[%%expect{|
val allocate_iarray : ('a : value_maybe_null). 'a -> 'a iarray = <fun>
|}]

let (allocate_lazy @ total) x = lazy x
[%%expect{|
Line 1, characters 32-38:
1 | let (allocate_lazy @ total) x = lazy x
                                    ^^^^^^
Error: The expression is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 28-38
         which is expected to be "total".
|}]

let (allocate_exception @ total) () =
  let exception Fresh in
  Fresh
[%%expect{|
Lines 2-3, characters 2-7:
2 | ..let exception Fresh in
3 |   Fresh
Error: The expression is "partial"
       but is expected to be "total"
         because it is used inside the function at lines 1-3, characters 33-7
         which is expected to be "total".
|}]

let (allocate_module_exception @ total) () =
  let module M = struct exception Fresh end in
  M.Fresh
[%%expect{|
Line 2, characters 17-43:
2 |   let module M = struct exception Fresh end in
                     ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The expression is "partial"
       but is expected to be "total"
         because it is used inside the function at lines 1-3, characters 40-9
         which is expected to be "total".
|}]

module type Exception_module = sig exception Fresh end
[%%expect{|
module type Exception_module = sig exception Fresh end
|}]

let (allocate_packed_exception @ total) () =
  (module struct exception Fresh end : Exception_module)
[%%expect{|
Line 2, characters 10-36:
2 |   (module struct exception Fresh end : Exception_module)
              ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The expression is "partial"
       but is expected to be "total"
         because it is used inside the function at lines 1-2, characters 40-56
         which is expected to be "total".
|}]

let (open_exception_module @ total) () =
  let open struct exception Fresh end in
  Fresh
[%%expect{|
Line 2, characters 11-37:
2 |   let open struct exception Fresh end in
               ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The expression is "partial"
       but is expected to be "total"
         because it is used inside the function at lines 1-3, characters 36-7
         which is expected to be "total".
|}]

module Existing = struct let value = 1 end
[%%expect{|
module Existing : sig val value : int end
|}]

let (use_module_alias @ total) () =
  let module M = Existing in
  M.value
[%%expect{|
val use_module_alias : unit -> int = <fun>
|}]

let (read_ref @ total) x = !x
[%%expect{|
Line 1, characters 27-28:
1 | let (read_ref @ total) x = !x
                               ^
Error: The value "(!)" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 23-29
         which is expected to be "total".
|}]

module type Declared_only = sig
  val total : int -> int @@ total
end
[%%expect{|
module type Declared_only = sig val total : int -> int @@ total end
|}]

module Declared : sig
  val total : int -> int @@ total
  val partial : int -> int
end = struct
  let (total @ total) x = x + 1
  let partial x = x / 1
end
[%%expect{|
module Declared :
  sig val total : int -> int @@ total val partial : int -> int end
|}]

let (use_declared_total @ total) x = Declared.total x
[%%expect{|
val use_declared_total : int -> int = <fun>
|}]

let (use_declared_partial @ total) x = Declared.partial x
[%%expect{|
Line 1, characters 39-55:
1 | let (use_declared_partial @ total) x = Declared.partial x
                                           ^^^^^^^^^^^^^^^^
Error: The value "Declared.partial" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 35-57
         which is expected to be "total".
|}]

external trusted : int -> int @@ total = "%identity"
[%%expect{|
external trusted : int -> int = "%identity"
|}]

let (use_trusted @ total) x = trusted x
[%%expect{|
val use_trusted : int -> int = <fun>
|}]

let existing_object = object method value = 1 end
[%%expect{|
val existing_object : < value : int > = <obj>
|}]

let (return_existing_object @ total) () = existing_object
[%%expect{|
Line 1, characters 42-57:
1 | let (return_existing_object @ total) () = existing_object
                                              ^^^^^^^^^^^^^^^
Error: The value "existing_object" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 37-57
         which is expected to be "total".
|}]

let (force_lazy @ total) = function lazy x -> x
[%%expect{|
Line 1, characters 36-42:
1 | let (force_lazy @ total) = function lazy x -> x
                                        ^^^^^^
Error: The expression is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 27-47
         which is expected to be "total".
|}]

let (catch_argument @ total) f =
  match f () with x -> x | exception _ -> 0
[%%expect{|
Line 2, characters 27-38:
2 |   match f () with x -> x | exception _ -> 0
                               ^^^^^^^^^^^
Error: The expression is "partial"
       but is expected to be "total"
         because it is used inside the function at lines 1-2, characters 29-43
         which is expected to be "total".
|}]

let (make_object @ total) () = object method value = 1 end
[%%expect{|
Line 1, characters 31-58:
1 | let (make_object @ total) () = object method value = 1 end
                                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The object is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 26-58
         which is expected to be "total".
|}]

let install_probe () = [%probe "totality" ()]
[%%expect{|
val install_probe : unit -> unit = <fun>
|}]

let (use_probe @ total) () = [%probe "totality" ()]
[%%expect{|
Line 1, characters 29-51:
1 | let (use_probe @ total) () = [%probe "totality" ()]
                                 ^^^^^^^^^^^^^^^^^^^^^^
Error: The expression is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 24-51
         which is expected to be "total".
|}]

let (probe_enabled @ total) () = [%probe_is_enabled "totality"]
[%%expect{|
Line 1, characters 33-63:
1 | let (probe_enabled @ total) () = [%probe_is_enabled "totality"]
                                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The expression is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 28-63
         which is expected to be "total".
|}]

let (comprehension @ total) n = [i for i = 0 to n]
[%%expect{|
Line 1, characters 32-50:
1 | let (comprehension @ total) n = [i for i = 0 to n]
                                    ^^^^^^^^^^^^^^^^^^
Error: The loop is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 28-50
         which is expected to be "total".
|}]

let (unused_recursive @ total) () =
  let rec _loop () = _loop () in
  ()
[%%expect{|
val unused_recursive : unit -> unit = <fun>
|}]

let (add_one @ total) = (+) 1
[%%expect{|
Line 1, characters 24-29:
1 | let (add_one @ total) = (+) 1
                            ^^^^^
Error: This value is "partial" but is expected to be "total".
|}]

module rec Recursive_total : sig
  val f : unit -> unit @@ total
end = struct
  let (f @ total) () = Recursive_total.f ()
end
[%%expect{|
Line 4, characters 23-40:
4 |   let (f @ total) () = Recursive_total.f ()
                           ^^^^^^^^^^^^^^^^^
Error: The value "Recursive_total.f" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 4, characters 18-43
         which is expected to be "total".
|}]

module rec Independent_total : sig
  val f : int -> int @@ total
end = struct
  let (f @ total) x = x + 1
end
[%%expect{|
module rec Independent_total : sig val f : int -> int @@ total end
|}]

let (int_equal @ total) (x : int) y = x = y
let (int_not_equal @ total) (x : int) y = x <> y
let (int_less @ total) (x : int) y = x < y
let (int_less_equal @ total) (x : int) y = x <= y
let (int_greater @ total) (x : int) y = x > y
let (int_greater_equal @ total) (x : int) y = x >= y
let (int_compare @ total) (x : int) y = compare x y
let (bool_equal @ total) (x : bool) y = x = y
[%%expect{|
val int_equal : int -> int -> bool = <fun>
val int_not_equal : int -> int -> bool = <fun>
val int_less : int -> int -> bool = <fun>
val int_less_equal : int -> int -> bool = <fun>
val int_greater : int -> int -> bool = <fun>
val int_greater_equal : int -> int -> bool = <fun>
val int_compare : int -> int -> int = <fun>
val bool_equal : bool -> bool -> bool = <fun>
|}]

let (inferred_equal @ total) x = x = 0
let (use_inferred @ total) x =
  let equal y = y = 0 in
  equal x
let (equal_value @ total) = ((=) : int -> int -> bool)
let (qualified_equal @ total) (x : int) y = Stdlib.(=) x y
type integer = int
let (alias_equal @ total) (x : integer) y = x = y
[%%expect{|
val inferred_equal : int -> bool = <fun>
val use_inferred : int -> bool = <fun>
val equal_value : int -> int -> bool = <fun>
val qualified_equal : int -> int -> bool = <fun>
type integer = int
val alias_equal : integer -> integer -> bool = <fun>
|}]

let (constrained_equal @ total) x = ((=) : _ -> _ -> bool) x 0
let (mode_constrained_equal @ total) x = ((=) : @ total) x 0
let (locally_opened_equal @ total) x = Stdlib.((=)) x 0
external scalar_equal : int -> int -> bool = "%equal"
let (use_scalar_external @ total) x y = scalar_equal x y
[%%expect{|
val constrained_equal : int -> bool = <fun>
val mode_constrained_equal : int -> bool = <fun>
val locally_opened_equal : int -> bool = <fun>
external scalar_equal : int -> int -> bool = "%equal"
val use_scalar_external : int -> int -> bool = <fun>
|}]

let ordinary_equal x y = x = y
let ordinary_function_equal (x : int -> int) y = x = y
[%%expect{|
val ordinary_equal : 'a -> 'a -> bool = <fun>
val ordinary_function_equal : (int -> int) -> (int -> int) -> bool = <fun>
|}]

let polymorphic_value =
  let (eq @ total) = (=) in
  eq 0 0
[%%expect{|
Line 2, characters 21-24:
2 |   let (eq @ total) = (=) in
                         ^^^
Error: This value is "partial" but is expected to be "total".
|}]

let (polymorphic_equal @ total) x y = x = y
[%%expect{|
Line 1, characters 40-41:
1 | let (polymorphic_equal @ total) x y = x = y
                                            ^
Error: The value "(=)" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 32-43
         which is expected to be "total".
|}]

let (function_equal @ total) (x : int -> int) y = x = y
[%%expect{|
Line 1, characters 52-53:
1 | let (function_equal @ total) (x : int -> int) y = x = y
                                                        ^
Error: The value "(=)" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 29-55
         which is expected to be "total".
|}]

let (mutable_equal @ total) (x : int ref) y = x = y
[%%expect{|
Line 1, characters 48-49:
1 | let (mutable_equal @ total) (x : int ref) y = x = y
                                                    ^
Error: The value "(=)" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 28-51
         which is expected to be "total".
|}]

let (cyclic_equal @ total) (x : int list) y = x = y
[%%expect{|
Line 1, characters 48-49:
1 | let (cyclic_equal @ total) (x : int list) y = x = y
                                                    ^
Error: The value "(=)" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 27-51
         which is expected to be "total".
|}]

let (physical_equal @ total) (x : int) y = x == y
[%%expect{|
Line 1, characters 45-47:
1 | let (physical_equal @ total) (x : int) y = x == y
                                                 ^^
Error: The value "(==)" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 29-49
         which is expected to be "total".
|}]

let shadowed_equal =
  let ( = ) x y = print_endline "comparison"; Stdlib.(=) x y in
  let (f @ total) (x : int) y = x = y in
  f
[%%expect{|
Line 3, characters 34-35:
3 |   let (f @ total) (x : int) y = x = y in
                                      ^
Error: The value "(=)" is "partial"
         because it closes over the value "print_endline" at line 2, characters 18-31
         which is "partial".
       However, the value "(=)" highlighted is expected to be "total"
         because it is used inside the function at line 3, characters 18-37
         which is expected to be "total".
|}]

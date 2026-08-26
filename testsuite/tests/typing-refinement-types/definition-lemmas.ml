(* TEST
 flags = "-extension refinement_types";
 expect;
*)

module Definitions = struct
  let[@def] next x = x + 2
  let[@def] choose b x y = if b then x + 1 else y - 1
end;;
[%%expect{|
module Definitions :
  sig
    val next : int -> int
    val next_def : (x : int) -> {u : unit | (next x) = (x + 2)}
    val choose : bool @ immutable -> int -> int -> int
    val choose_def :
      (b : bool) ->
      (x : int) ->
      (y : int) -> {u : unit | (choose b x y) = (if b then x + 1 else y - 1)}
  end
|}]

let local () =
  let[@def] next x = x + 2 in
  let x = 3 in
  let refine_ proof = next_def x in
  ();;
[%%expect{|
val local : unit -> unit = <fun>
|}]

let[@def] top x = x + 1;;
[%%expect{|
Line 1, characters 0-23:
1 | let[@def] top x = x + 1;;
    ^^^^^^^^^^^^^^^^^^^^^^^
Error: The value "top" is "partial"
       but is expected to be "total"
         because it is used in an expression (at line 1, characters 0-23).
|}]

let[@def] effectful x = print_endline "effect"; x + 1;;
[%%expect{|
Line 1, characters 24-37:
1 | let[@def] effectful x = print_endline "effect"; x + 1;;
                            ^^^^^^^^^^^^^
Error: The value "print_endline" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 20-53
         which is expected to be "total".
|}]

let[@def] rec recursive x = recursive x;;
[%%expect{|
Line 1, characters 3-9:
1 | let[@def] rec recursive x = recursive x;;
       ^^^^^^
Error: The def attribute requires a single nonrecursive function binding
|}]

let[@def] polymorphic x = x;;
[%%expect{|
Line 1, characters 22-23:
1 | let[@def] polymorphic x = x;;
                          ^
Error: Definition lemmas require a scalar function with simple unlabelled parameters
|}]

let[@def] labelled ~x = x + 1;;
[%%expect{|
Line 1, characters 19-21:
1 | let[@def] labelled ~x = x + 1;;
                       ^^
Error: Definition lemmas require a scalar function with simple unlabelled parameters
|}]

let[@def] duplicate x = x + 1 [@@def];;
[%%expect{|
Line 1, characters 30-37:
1 | let[@def] duplicate x = x + 1 [@@def];;
                                  ^^^^^^^
Error: Duplicate def attribute
|}]

let[@def 1] payload x = x + 1;;
[%%expect{|
Line 1, characters 3-11:
1 | let[@def 1] payload x = x + 1;;
       ^^^^^^^^
Error: The def attribute takes no payload
|}]

let collision_def = 0
let[@def] collision x = x + 1;;
[%%expect{|
val collision_def : int = 0
Line 2, characters 0-29:
2 | let[@def] collision x = x + 1;;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The generated name collision_def is already bound
|}]

let escape () =
  let[@def] f x = x + 1 in
  let x = 3 in f_def x;;
[%%expect{|
Line 3, characters 2-22:
3 |   let x = 3 in f_def x;;
      ^^^^^^^^^^^^^^^^^^^^
Error: the refinement type of this expression escapes the scope of binding "x"
|}]

let via_unification slot =
  let[@def] f x = x + 1 in
  slot := f_def;;
[%%expect{|
Lines 2-3, characters 2-15:
2 | ..let[@def] f x = x + 1 in
3 |   slot := f_def..
Error: the refinement type of this expression escapes the scope of binding "f"
|}]

let[@def] ( ++ ) x = x + 1;;
[%%expect{|
Line 1, characters 0-26:
1 | let[@def] ( ++ ) x = x + 1;;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Definition lemmas require an identifier function name
|}]

let[@def] f x = x + 1 and g x = x - 1;;
[%%expect{|
Line 1, characters 3-9:
1 | let[@def] f x = x + 1 and g x = x - 1;;
       ^^^^^^
Error: The def attribute requires a single nonrecursive function binding
|}]

module Hidden : sig val f : int -> int @@ total end = struct
  let[@def] f x = x + 2
end;;
[%%expect{|
module Hidden : sig val f : int -> int @@ total end
|}]

let hidden = Hidden.f_def;;
[%%expect{|
Line 1, characters 13-25:
1 | let hidden = Hidden.f_def;;
                 ^^^^^^^^^^^^
Error: Unbound value "Hidden.f_def"
|}]

module Trailing = struct
  let f (x : int) = x [@@def]
end;;
[%%expect{|
module Trailing :
  sig val f : int -> int val f_def : (x : int) -> {u : unit | (f x) = x} end
|}]

module Source_position = struct
  let[@def] f (x : int) = __LINE__
end;;
[%%expect{|
Line 2, characters 26-34:
2 |   let[@def] f (x : int) = __LINE__
                              ^^^^^^^^
Error: Definition lemmas require a scalar function with simple unlabelled parameters
|}]

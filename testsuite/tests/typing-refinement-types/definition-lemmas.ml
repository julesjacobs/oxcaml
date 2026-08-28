(* TEST
 flags = "-extension refinement_types";
 expect;
*)

module Comparisons = struct
  let[@def] clamp n = if n >= 0 then n else 0
  let[@def] normalize (b : bool) = if b = true then b else false
end;;
[%%expect{|
module Comparisons :
  sig
    val clamp : int -> int
    val clamp_def :
      (n : int) -> {u : unit | (clamp n) === (if n >= 0 then n else 0)}
    val normalize : bool -> bool
    val normalize_def :
      (b : bool) ->
      {u : unit | (normalize b) === (if b = true then b else false)}
  end
|}]

module Definitions = struct
  let[@def] next x = x + 2
  let[@def] choose b x y = if b then x + 1 else y - 1
end;;
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
val top : int -> int = <fun>
val top_def : (x : int) -> {u : unit | (top x) === (x + 1)} = <fun>
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
val polymorphic : 'a @ immutable -> 'a @ immutable = <fun>
val polymorphic_def : (x : 'a) -> {u : unit | (polymorphic x) === x} = <fun>
|}]

let[@def] dependent :
    (x : int) -> (y : { y : int | y = x }) -> unit =
  fun x y -> ();;
[%%expect{|
val dependent : (x : int) -> ({y : int | y = x} -> unit) @ total nonportable =
  <fun>
val dependent_def :
  (x : int) -> (y : {y : int | y = x}) -> {u : unit | (dependent x y) === ()} =
  <fun>
|}]

let[@def] apply (f @ total) x = f x;;
[%%expect{|
val apply : ('a @ immutable -> 'b) @ total -> 'a @ immutable -> 'b = <fun>
val apply_def :
  (f : ('a @ immutable -> 'b)) ->
  (x : 'a) -> {u : unit | (apply f x) === (f x)} = <fun>
|}]

type 'a box = Box of 'a
let[@def] box x = Box x;;
[%%expect{|
type 'a box = Box of 'a
val box : 'a @ immutable -> 'a box @ immutable = <fun>
val box_def : (x : 'a) -> {u : unit | (box x) === (Box x)} = <fun>
|}]

let[@def] labelled ~x = x + 1;;
[%%expect{|
Line 1, characters 19-21:
1 | let[@def] labelled ~x = x + 1;;
                       ^^
Error: Definition lemmas require a function with simple unlabelled parameters
|}]

let[@def] duplicate x = x + 1 [@@def];;
[%%expect{|
Line 1, characters 30-37:
1 | let[@def] duplicate x = x + 1 [@@def];;
                                  ^^^^^^^
Error: Duplicate def attribute
|}]

let[@def] (mode_checked @ stateless) x = x + 1;;
[%%expect{|
val mode_checked : int -> int = <fun>
val mode_checked_def : (x : int) -> {u : unit | (mode_checked x) === (x + 1)} =
  <fun>
|}]

let[@def] (explicit_total @ total stateless) x = x + 1;;
[%%expect{|
val explicit_total : int -> int = <fun>
val explicit_total_def :
  (x : int) -> {u : unit | (explicit_total x) === (x + 1)} = <fun>
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
  sig
    val f : int -> int
    val f_def : (x : int) -> {u : unit | (f x) === x}
  end
|}]

module Source_position = struct
  let[@def] f (x : int) = __LINE__
end;;
[%%expect{|
Line 2, characters 26-34:
2 |   let[@def] f (x : int) = __LINE__
                              ^^^^^^^^
Error: Definition lemmas cannot preserve zero-argument primitive values
|}]

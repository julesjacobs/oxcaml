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
Line 1, characters 28-37:
1 | let[@def] rec recursive x = recursive x;;
                                ^^^^^^^^^
Error: The value "recursive" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 24-39
         which is expected to be "total".
|}]

let[@def] polymorphic x = x;;
[%%expect{|
val polymorphic : 'a -> 'a = <fun>
val polymorphic_def : (x : 'a) -> {u : unit | (polymorphic x) === x} = <fun>
|}]

let instantiate_int (x : int) =
  let refine_ _equation = polymorphic_def x in ();;
[%%expect{|
val instantiate_int : int -> unit = <fun>
|}]

let instantiate_bool (x : bool) =
  let refine_ _equation = polymorphic_def x in ();;
[%%expect{|
val instantiate_bool : bool -> unit = <fun>
|}]

let captured (g @ total) =
  let[@def] f y = g y in
  let i = 1 in
  let b = true in
  let refine_ p = f_def i in
  let refine_ q = f_def b in
  ();;
[%%expect{|
Line 6, characters 24-25:
6 |   let refine_ q = f_def b in
                            ^
Error: The value "b" has type "bool" but an expression was expected of type "int"
|}]

type fn = int -> int
let[@def] alias_identity : fn = fun x -> x;;
[%%expect{|
type fn = int -> int
val alias_identity : fn = <fun>
val alias_identity_def : (x : int) -> {u : unit | (alias_identity x) === x} =
  <fun>
|}]

module Observer = struct
  type cell = {mutable contents : int}
  type 'a wrapper = {value : 'a}
  let[@def] get (wrapper : 'a wrapper) : 'a = wrapper.value
  let write wrapper = (get wrapper).contents <- 1
end;;
[%%expect{|
module Observer :
  sig
    type cell = { mutable contents : int; }
    type 'a wrapper = { value : 'a; }
    val get : 'a wrapper -> 'a
    val get_def :
      (wrapper : 'a wrapper) ->
      {u : unit | (get wrapper) === (wrapper.value : _)}
    val write : cell wrapper -> unit
  end
|}]

let[@def] read_cell (cell : Observer.cell @ total) = cell.contents;;
[%%expect{|
Line 1, characters 53-57:
1 | let[@def] read_cell (cell : Observer.cell @ total) = cell.contents;;
                                                         ^^^^
Error: The expression is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 20-66
         which is expected to be "total".
|}]

let[@def] dependent : (x : int) -> { y : int | y = x } =
  fun x -> refine_ x;;
[%%expect{|
val dependent : (x : int) -> {y : int | y = x} = <fun>
val dependent_def : (x : int) -> {u : unit | (dependent x) === x} = <fun>
|}]

let[@def] apply (f @ total) x = f x;;
[%%expect{|
val apply : ('a -> 'b) @ total -> 'a -> 'b = <fun>
val apply_def :
  (f : ('a -> 'b)) -> (x : 'a) -> {u : unit | (apply f x) === (f x)} = <fun>
|}]

type 'a box = Box of 'a
let[@def] box x = Box x;;
[%%expect{|
type 'a box = Box of 'a
val box : 'a -> 'a box = <fun>
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

let[@def] proof_erased (x : {x : int | x >= 0}) =
  let refine_ payload = x in
  let proof = () in
  let checked : {u : unit | true} = refine_ proof in
  let refine_ proof = checked in
  payload;;
[%%expect{|
val proof_erased : {x : int | x >= 0} -> int = <fun>
val proof_erased_def :
  (x : {x : int | x >= 0}) ->
  {u : unit | (proof_erased x) === (let payload = x in payload)} = <fun>
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
Error: The def attribute requires a single function binding
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

module Reader : sig val read : int -> int @@ total reading end = struct
  let read x = x
end;;
[%%expect{|
module Reader : sig val read : int -> int @@ total reading shareable end
|}]

let[@def] (reading @ reading) x = Reader.read x;;
[%%expect{|
Line 1, characters 3-47:
1 | let[@def] (reading @ reading) x = Reader.read x;;
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This value is "reading" but is expected to be "stateless".
|}]

let[@def] dependent_parameter : (x : int) -> {v : int | v = x} -> int =
  fun x y -> let refine_ value = y in value;;
[%%expect{|
val dependent_parameter :
  (x : int) -> ({v : int | v = x} -> int) @ total stateful = <fun>
val dependent_parameter_def :
  (x : int) ->
  (y : {v : int | v = x}) ->
  {u : unit | (dependent_parameter x y) === (let value = y in value)} = <fun>
|}]

let use_dependent_parameter : (x : int) -> {v : int | v = x} -> unit =
  fun x y -> let refine_ proof = dependent_parameter_def x y in ();;
[%%expect{|
val use_dependent_parameter : (x : int) -> {v : int | v = x} -> unit = <fun>
|}]

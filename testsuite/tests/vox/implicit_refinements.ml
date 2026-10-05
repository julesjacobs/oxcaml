(* TEST
 has-z3;
 flags = "-extension refinement_types -principal";
 expect;
*)

external ( >= ) : int -> int -> bool @@ total = "%greaterequal"
external ( <= ) : int -> int -> bool @@ total = "%lessequal"
external ( = ) : int -> int -> bool @@ total = "%equal";;
[%%expect{|
external ( >= ) : int -> int -> bool = "%greaterequal"
external ( <= ) : int -> int -> bool = "%lessequal"
external ( = ) : int -> int -> bool = "%equal"
|}]

let zero : {n : int | n >= 0} = 0
let use_parameter (x : {n : int | n >= 0}) : {n : int | n >= 0} = x
let plain = zero + 1
let return_local () = let x = 3 in (x : {n : int | n = x});;
[%%expect{|
val zero : {n : int | n >= 0} = 0
val use_parameter : {n : int | n >= 0} -> {n : int | n >= 0} = <fun>
val plain : int = 1
val return_local : unit -> int = <fun>
|}]

let local_branch (b : bool) : {n : int | n >= 0} =
  if b then let n = 0 in n else raise Exit
let local_explicit_branch (b : bool) : {n : int | n >= 0} =
  if b then let n = 0 in refine_ n else raise Exit
let local_runtime_branch (b : bool) : {n : int | n >= 0} =
  if b then let n = 0 in assume_ n else raise Exit;;
[%%expect{|
val local_branch : bool -> {n : int | n >= 0} = <fun>
val local_explicit_branch : bool -> {n : int | n >= 0} = <fun>
val local_runtime_branch : bool -> {n : int | n >= 0} = <fun>
|}]

let step : (x : int) -> {y : int | y >= x} = fun x ->
  if x = 13 then failwith "step" else x
let chain (x : int) : {y : int | y >= x} = step (step (step (step x)))
let branch (b : bool) (x : int) : {y : int | y >= x} =
  step (if b then step x else step (step x))
let consume (x : {n : int | n >= 0}) = x + 1
let checked = consume (step 0);;
[%%expect{|
val step : (x : int) -> {y : int | y >= x} = <fun>
val chain : (x : int) -> {y : int | y >= x} = <fun>
val branch : bool -> (x : int) -> {y : int | y >= x} = <fun>
val consume : {n : int | n >= 0} -> int = <fun>
val checked : int = 1
|}]

let wrong : {n : int | n >= 0} = -1;;
[%%expect{|
Line 1, characters 33-35:
1 | let wrong : {n : int | n >= 0} = -1;;
                                     ^^
Error: Refinement could not be proved (counterexample)
|}]

let wrong_if_argument = consume (if true then -1 else -1);;
[%%expect{|
Line 1, characters 46-48:
1 | let wrong_if_argument = consume (if true then -1 else -1);;
                                                  ^^
Error: Refinement could not be proved (counterexample)
|}]

let wrong_match_argument b = consume (match b with true -> -1 | false -> -1);;
[%%expect{|
Line 1, characters 59-61:
1 | let wrong_match_argument b = consume (match b with true -> -1 | false -> -1);;
                                                               ^^
Error: Refinement could not be proved (counterexample)
|}]

let wrong_let_argument = consume (let x = -1 in x);;
[%%expect{|
Line 1, characters 48-49:
1 | let wrong_let_argument = consume (let x = -1 in x);;
                                                    ^
Error: Refinement could not be proved (counterexample)
|}]

let wrong_chain (x : int) : {y : int | y >= 0} = step (step (step x));;
[%%expect{|
Line 1, characters 49-69:
1 | let wrong_chain (x : int) : {y : int | y >= 0} = step (step (step x));;
                                                     ^^^^^^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let refined_elements : {n : int | n >= 0} list = [0; 1]
let head = function [] -> 0 | x :: _ -> x + 1
let use_elements = head refined_elements;;
[%%expect{|
val refined_elements : {n : int | n >= 0} list = [0; 1]
val head : int list -> int = <fun>
Line 3, characters 24-40:
3 | let use_elements = head refined_elements;;
                            ^^^^^^^^^^^^^^^^
Error: The value "refined_elements" has type "{n : int | n >= 0} list"
       but an expression was expected of type "int list"
       Type "{n : int | n >= 0}" is not compatible with type "int"
|}]

let inferred_branch b x : {n : int | n >= 0} =
  if b then step 0 else x;;
[%%expect{|
val inferred_branch : bool -> {n : int | n >= 0} -> {n : int | n >= 0} =
  <fun>
|}]

let callback : (f : (int -> {n : int | n >= 0})) -> int = fun f ->
  consume (f 0)
let lambda_result = callback (fun _ -> 0);;
[%%expect{|
val callback : (int -> {n : int | n >= 0}) -> int = <fun>
val lambda_result : int = 1
|}]

let extracted : {n : int | n >= 0} list -> int = function
  | [] -> 0
  | x :: _ -> consume x;;
[%%expect{|
val extracted : {n : int | n >= 0} list -> int = <fun>
|}]

let no_deep_conversion (xs : int list) : {n : int | n >= 0} list = xs;;
[%%expect{|
Line 1, characters 67-69:
1 | let no_deep_conversion (xs : int list) : {n : int | n >= 0} list = xs;;
                                                                       ^^
Error: The value "xs" has type "int list" but an expression was expected of type
         "{n : int | n >= 0} list"
       Type "int" is not compatible with type "{n : int | n >= 0}"
|}]

let identity : (x : int) -> {y : int | y = x} = fun x -> x
let scoped = identity (identity 1)
let shadowed (x : int) : {y : int | y = x} =
  let y = identity (identity x) in
  let x = 0 in
  y + x;;
[%%expect{|
val identity : (x : int) -> {y : int | y = x} = <fun>
val scoped : int = 1
val shadowed : (x : int) -> {y : int | y = x} = <fun>
|}]

let user_argument =
  let argument = 5 in identity (argument + 1);;
[%%expect{|
val user_argument : int = 6
|}]

let check_relation : (r : (int -> bool)) @ total ghost ->
    {n : int | r n} -> int = fun r n -> n
let anonymous_relation = check_relation (ghost_ (fun n -> n >= 0)) 0;;
[%%expect{|
val check_relation : (r : (int -> bool)) @ ghost -> {n : int | r n} -> int =
  <fun>
val anonymous_relation : int = 0
|}]

let dependent_pair : {p : int * int | (let x, y = p in x <= y)} -> int =
  fun (x, y) -> (y : {n : int | n >= x});;
[%%expect{|
val dependent_pair : {p : int * int | match p with | (x, y) -> x <= y} -> int =
  <fun>
|}]

let impossible () : {n : int | false} = raise Exit
let later_failure () =
  let take _ y = y in
  take (impossible ()) (consume (-1));;
[%%expect{|
val impossible : unit -> {n : int | false} = <fun>
Line 4, characters 32-36:
4 |   take (impossible ()) (consume (-1));;
                                    ^^^^
Error: Refinement could not be proved (counterexample)
|}]

let follow : (x : int) -> {y : int | y >= x} -> int = fun x y -> y
let order = ref []
let mark : (x : int) -> {n : int | n = x} = fun x ->
  order := x :: !order; x
let ordered = follow (mark 1) (mark 2)
let observed_order = !order;;
[%%expect{|
val follow : (x : int) -> {y : int | y >= x} -> int = <fun>
val order : '_weak1 list ref = {contents = []}
val mark : (x : int) -> {n : int | n = x} = <fun>
val ordered : int = 2
val observed_order : int list = [1; 2]
|}]

let local_module_argument =
  follow (identity 1) (let module M = struct end in 2);;
[%%expect{|
val local_module_argument : int = 2
|}]

let wrong_local_module_argument =
  follow (identity 1) (let module M = struct end in -1);;
[%%expect{|
Line 2, characters 52-54:
2 |   follow (identity 1) (let module M = struct end in -1);;
                                                        ^^
Error: Refinement could not be proved (counterexample)
|}]

let use_premise : (x : int) -> {u : unit | x >= 0} ->
    {n : int | n >= 0} = fun x _proof -> x;;
[%%expect{|
val use_premise : (x : int) -> {u : unit | x >= 0} -> {n : int | n >= 0} =
  <fun>
|}]

let apply_relation : (r : (int -> bool)) @ total ghost ->
    (unit -> {n : int | r n}) -> unit -> int =
  fun r f () -> f ()
let anonymous_callback =
  apply_relation (ghost_ (fun n -> n >= 0)) (fun () -> 0) ();;
[%%expect{|
val apply_relation :
  (r : (int -> bool)) @ ghost -> (unit -> {n : int | r n}) -> unit -> int =
  <fun>
val anonymous_callback : int = 0
|}]

let bad_anonymous_callback =
  apply_relation (ghost_ (fun n -> n >= 0)) (fun () -> -1) ();;
[%%expect{|
Line 2, characters 55-57:
2 |   apply_relation (ghost_ (fun n -> n >= 0)) (fun () -> -1) ();;
                                                           ^^
Error: Refinement could not be proved (counterexample)
|}]

type nonnegative = {n : int | n >= 0}
type small = {n : nonnegative | n <= 3}
let small : small = 2
let small_payload = small + 1;;
[%%expect{|
type nonnegative = {n : int | n >= 0}
type small = {n : nonnegative | n <= 3}
val small : small = 2
val small_payload : int = 3
|}]

let bad_small : small = -1;;
[%%expect{|
Line 1, characters 24-26:
1 | let bad_small : small = -1;;
                            ^^
Error: Refinement could not be proved (counterexample)
|}]

let local_partial () =
  let f = follow (identity 1) in
  f 2
let nested_partial () = (follow (identity 1)) 2
let block_partial = let f = follow (identity 1) in f 2;;
[%%expect{|
val local_partial : unit -> int = <fun>
val nested_partial : unit -> int = <fun>
val block_partial : int = 2
|}]

let escaped_partial = follow (identity 1);;
[%%expect{|
Line 1, characters 22-41:
1 | let escaped_partial = follow (identity 1);;
                          ^^^^^^^^^^^^^^^^^^^
Error: the refinement type of this expression escapes the scope of binding "*argument*"
|}]

let annotated_pair ((x, y) : {p : int * int | let x, y = p in x <= y}) =
  (y : {n : int | x <= n})
let nested_pattern (xs : {p : int * int | let x, y = p in x <= y} list) =
  match xs with [] -> 0 | (x, y) :: _ -> (y : {n : int | x <= n});;
[%%expect{|
val annotated_pair : {p : int * int | match p with | (x, y) -> x <= y} -> int =
  <fun>
val nested_pattern :
  {p : int * int | match p with | (x, y) -> x <= y} list -> int = <fun>
|}]

let no_function_result_fact (f : unit -> {n : int | false}) =
  (0 : {n : int | false});;
[%%expect{|
Line 2, characters 3-4:
2 |   (0 : {n : int | false});;
       ^
Error: Refinement could not be proved (counterexample)
|}]

module Positive = struct
  let (only @ total) (n : {n : int | n >= 0}) : int = n
end
type unchecked_predicate = {u : unit | Positive.only (-1) = 0};;
[%%expect{|
module Positive : sig val only : {n : int | n >= 0} -> int end
Line 4, characters 53-57:
4 | type unchecked_predicate = {u : unit | Positive.only (-1) = 0};;
                                                         ^^^^
Error: The constant "-1" has type "int" but an expression was expected of type
         "{n : int | n >= 0}"
|}]

let nested_take : (x : int) -> {n : {m : int | m >= x} | n <= 3} -> int =
  fun x n -> n
let nested_ok = nested_take (identity 1) 2;;
[%%expect{|
val nested_take : (x : int) -> {n : {m : int | m >= x} | n <= 3} -> int =
  <fun>
val nested_ok : int = 2
|}]

let bad_nested_inner = nested_take (identity 1) 0;;
[%%expect{|
Line 1, characters 48-49:
1 | let bad_nested_inner = nested_take (identity 1) 0;;
                                                    ^
Error: Refinement could not be proved (counterexample)
|}]

let bad_nested_outer = nested_take (identity 1) 4;;
[%%expect{|
Line 1, characters 48-49:
1 | let bad_nested_outer = nested_take (identity 1) 4;;
                                                    ^
Error: Refinement could not be proved (counterexample)
|}]

external total_identity : (x : int) -> {y : int | y = x} @@ total = "%identity";;
[%%expect{|
external total_identity : (x : int) -> {y : int | y = x} = "%identity"
|}]

let ghost_local_argument () =
  let f = ghost_ (fun n -> total_identity (n + 1)) in
  (() : {u : unit | f 0 === 1 && f 1 === 2});;
[%%expect{|
val ghost_local_argument : unit -> unit = <fun>
|}]

let ghost_distinct_arguments () =
  let f = ghost_ (fun n -> total_identity (n + 1)) in
  (() : {u : unit | f 0 === f 1});;
[%%expect{|
Line 3, characters 3-5:
3 |   (() : {u : unit | f 0 === f 1});;
       ^^
Error: Refinement could not be proved (counterexample)
|}]

type refined_function = {f : unit -> unit | true}
let partial_refined_function : refined_function =
  fun () -> print_endline "partial";;
[%%expect{|
type refined_function = {f : unit -> unit | true}
Line 3, characters 12-25:
3 |   fun () -> print_endline "partial";;
                ^^^^^^^^^^^^^
Error: The value "print_endline" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 3, characters 2-35
         which is expected to be "total".
|}]

let selected_callback b =
  apply_relation (ghost_ (fun n -> n >= 0))
    (if b then (fun () -> 0) else (fun () -> 1)) ();;
[%%expect{|
val selected_callback : bool -> int = <fun>
|}]

let pair_argument : (x : int) -> ({n : int | n >= x} * unit) -> int =
  fun x (n, _) -> n
let constructed_argument () =
  pair_argument (identity 1) (2, ());;
[%%expect{|
val pair_argument : (x : int) -> {n : int | n >= x} * unit -> int = <fun>
val constructed_argument : unit -> int = <fun>
|}]

module Polymorphic_intro = struct
  let identity : ('a : immutable_data).
      (x : 'a) -> {y : 'a | y === x} = fun x -> x

  let callback : ('a : immutable_data).
      (x : 'a) -> ({y : 'a | y === x} -> unit) -> unit =
    fun x f -> f x
end;;
[%%expect{|
module Polymorphic_intro :
  sig
    val identity : ('a : immutable_data). (x : 'a) -> {y : 'a | y === x}
    val callback :
      ('a : immutable_data). (x : 'a) -> ({y : 'a | y === x} -> unit) -> unit
  end
|}]

module Wrong_polymorphic_intro = struct
  let wrong : ('a : immutable_data).
      (x : 'a) -> (y : 'a) @ total -> {result : 'a | result === x} =
    fun x y -> y
end;;
[%%expect{|
Line 4, characters 15-16:
4 |     fun x y -> y
                   ^
Error: Refinement could not be proved (counterexample)
|}]

module Tuple_intro = struct
  let rec (pair @ total) :
      (n : int) -> {p : int * int | let x, y = p in x <= n && y <= n} =
    fun n -> if n <= 0 then (n, n) else pair (n - 1)
  [@@decreases n]
end;;
[%%expect{|
module Tuple_intro :
  sig
    val pair :
      (n : int) ->
      {p : int * int | match p with | (x, y) -> (x <= n) && (y <= n)}
  end
|}]

module Polymorphic_argument_contract = struct
  type ('a : immutable_data) box = {values : 'a iarray; limit : int}

  let[@def] (get @ total) : ('a : immutable_data).
      (box : 'a box) -> {i : int | i = box.limit} -> int =
    fun box index -> index

  let (client @ total) : ('a : immutable_data).
      (box : 'a box) -> unit =
    fun box -> get_def box box.limit
end;;
[%%expect{|
module Polymorphic_argument_contract :
  sig
    type ('a : immutable_data) box = { values : 'a iarray; limit : int; }
    val get :
      ('a : immutable_data).
        (box : 'a box) -> ({i : int | i = box.limit} -> int) @ total stateful
    val get_def :
      ('a : immutable_data).
        (box : 'a box) ->
        (index : {i : int | i = box.limit}) ->
        {u : unit | (get box index) === index}
    val client : ('a : immutable_data). 'a box -> unit
  end
|}]

module Wrong_polymorphic_argument_contract = struct
  open Polymorphic_argument_contract
  let (client @ total) : ('a : immutable_data).
      (box : 'a box) -> unit =
    fun box -> get_def box (box.limit + 1)
end;;
[%%expect{|
Line 5, characters 27-42:
5 |     fun box -> get_def box (box.limit + 1)
                               ^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

(* TEST
 has-z3;
 flags = "-extension refinement_types -principal";
 expect;
*)

external ( >= ) : int -> int -> bool @@ total = "%greaterequal"
external ( = ) : int -> int -> bool @@ total = "%equal"
let identity : (x : int) -> {y : int | y = x} = fun x -> x
let call : (lower : int) -> (unit -> {n : int | n >= lower}) -> int =
  fun lower f -> f ()
let take : (lower : int) -> {n : int | n >= lower} -> int = fun lower n -> n
let consume (n : {n : int | n >= 0}) = n;;
[%%expect{|
external ( >= ) : int -> int -> bool = "%greaterequal"
external ( = ) : int -> int -> bool = "%equal"
val identity : (x : int) -> {y : int | y = x} = <fun>
val call : (lower : int) -> (unit -> {n : int | n >= lower}) -> int = <fun>
val take : (lower : int) -> {n : int | n >= lower} -> int = <fun>
val consume : {n : int | n >= 0} -> int = <fun>
|}]

let selected b = call (identity 1) (if b then fun () -> 2 else fun () -> 3)
let matched v = call (identity 1)
  (match v with Some n when n >= 1 -> (fun () -> n) | _ -> (fun () -> 1))
let captured b = call (identity 1)
  (if b then let n = identity 2 in fun () -> n else let n = identity 3 in fun () -> n)
let branch_facts n = call (identity 1)
  (if n >= 1 then (fun () -> n) else (fun () -> 1));;
[%%expect{|
val selected : bool -> int = <fun>
val matched : int option -> int = <fun>
val captured : bool -> int = <fun>
val branch_facts : int -> int = <fun>
|}]

let wrong_selected b = call (identity 1) (if b then fun () -> 0 else fun () -> 3);;
[%%expect{|
Line 1, characters 62-63:
1 | let wrong_selected b = call (identity 1) (if b then fun () -> 0 else fun () -> 3);;
                                                                  ^
Error: Refinement could not be proved (counterexample)
|}]

let wrong_match v = call (identity 1)
  (match v with Some n when n >= 0 -> (fun () -> n) | _ -> (fun () -> 1));;
[%%expect{|
Line 2, characters 49-50:
2 |   (match v with Some n when n >= 0 -> (fun () -> n) | _ -> (fun () -> 1));;
                                                     ^
Error: Refinement could not be proved (counterexample)
|}]

let pair : (lower : int) -> ({n : int | n >= lower} * unit) -> int =
  fun lower (n, _) -> n
let list : (lower : int) -> {n : int | n >= lower} list -> int = fun lower _ -> lower
type 'a box = { item : 'a }
let record : (lower : int) -> {n : int | n >= lower} box -> int = fun lower r -> r.item
let array : (lower : int) -> {n : int | n >= lower} iarray -> int = fun lower _ -> lower
let constructed () =
  pair (identity 1) (2, ()), list (identity 1) [2; 3],
  record (identity 1) {item = 2}, array (identity 1) [:2; 3:]
let conditional_pair n = pair (identity 1) (if n >= 1 then (n, ()) else (1, ()))
let matching_pair v = pair (identity 1)
  (match v with Some n when n >= 1 -> (n, ()) | _ -> (1, ()))
let local_pair () = pair (identity 1) (let n = identity 2 in (n, ()))
let module_pair () = pair (identity 1)
  (let module M = struct let n = identity 2 end in (M.n, ()));;
[%%expect{|
val pair : (lower : int) -> {n : int | n >= lower} * unit -> int = <fun>
val list : (lower : int) -> {n : int | n >= lower} list -> int = <fun>
type 'a box = { item : 'a; }
val record : (lower : int) -> {n : int | n >= lower} box -> int = <fun>
val array : (lower : int) -> {n : int | n >= lower} iarray -> int = <fun>
val constructed : unit -> int * int * int * int = <fun>
val conditional_pair : int -> int = <fun>
val matching_pair : int option -> int = <fun>
val local_pair : unit -> int = <fun>
val module_pair : unit -> int = <fun>
|}]

let wrong_pair () = pair (identity 1) (0, ());;
[%%expect{|
Line 1, characters 39-40:
1 | let wrong_pair () = pair (identity 1) (0, ());;
                                           ^
Error: Refinement could not be proved (counterexample)
|}]

let wrong_list () = list (identity 1) [2; 0];;
[%%expect{|
Line 1, characters 42-43:
1 | let wrong_list () = list (identity 1) [2; 0];;
                                              ^
Error: Refinement could not be proved (counterexample)
|}]

let wrong_record () = record (identity 1) {item = 0};;
[%%expect{|
Line 1, characters 50-51:
1 | let wrong_record () = record (identity 1) {item = 0};;
                                                      ^
Error: Refinement could not be proved (counterexample)
|}]

let wrong_array () = array (identity 1) [:2; 0:];;
[%%expect{|
Line 1, characters 45-46:
1 | let wrong_array () = array (identity 1) [:2; 0:];;
                                                 ^
Error: Refinement could not be proved (counterexample)
|}]

let relation : (r : (int -> bool)) @ total ghost ->
    (unit -> {n : int | r n}) -> int = fun r f -> f ()
let selected_relation b = relation
  (if b then ghost_ (fun n -> n >= 0) else ghost_ (fun n -> n >= 1))
  (if b then (fun () -> 0) else (fun () -> 1));;
[%%expect{|
val relation :
  (r : (int -> bool)) @ ghost -> (unit -> {n : int | r n}) -> int = <fun>
val selected_relation : bool -> int = <fun>
|}]

let wrong_relation b = relation
  (if b then ghost_ (fun n -> n >= 0) else ghost_ (fun n -> n >= 1))
  (if b then (fun () -> 1) else (fun () -> 0));;
[%%expect{|
Line 3, characters 43-44:
3 |   (if b then (fun () -> 1) else (fun () -> 0));;
                                               ^
Error: Refinement could not be proved (counterexample)
|}]

let events = ref []
let mark : (n : int) -> {r : int | r = n} = fun n -> events := n :: !events; n
let ordered b =
  (ignore (mark 3); call) (mark 1)
    (ignore (mark 2); if b then (fun () -> ignore (mark 4); 2) else (fun () -> 3))
let result = ordered true
let observed = !events;;
[%%expect{|
val events : '_weak1 list ref = {contents = []}
val mark : (n : int) -> {r : int | r = n} = <fun>
val ordered : bool -> int = <fun>
val result : int = 2
val observed : int list = [4; 3; 1; 2]
|}]

let skipped () = take (raise Exit) (-1)
let skipped_pair () = pair (raise Exit) (-1, ())
let skipped_closure b = call (raise Exit) (if b then fun () -> -1 else fun () -> -2);;
[%%expect{|
val skipped : unit -> int = <fun>
val skipped_pair : unit -> int = <fun>
val skipped_closure : bool -> int = <fun>
|}]

let internal_call () = pair (raise Exit) (consume (-1), ());;
[%%expect{|
Line 1, characters 50-54:
1 | let internal_call () = pair (raise Exit) (consume (-1), ());;
                                                      ^^^^
Error: Refinement could not be proved (counterexample)
|}]

let condition () = call (raise Exit)
  (if consume (-1) = 0 then (fun () -> 0) else (fun () -> 1));;
[%%expect{|
Line 2, characters 14-18:
2 |   (if consume (-1) = 0 then (fun () -> 0) else (fun () -> 1));;
                  ^^^^
Error: Refinement could not be proved (counterexample)
|}]

let guard (v : int option) = call (raise Exit)
  (match v with Some n when consume (-1) = 0 -> (fun () -> n) | _ -> (fun () -> 1));;
[%%expect{|
Line 2, characters 36-40:
2 |   (match v with Some n when consume (-1) = 0 -> (fun () -> n) | _ -> (fun () -> 1));;
                                        ^^^^
Error: Refinement could not be proved (counterexample)
|}]

let local_initializer () = pair (raise Exit) (let n = consume (-1) in (n, ()));;
[%%expect{|
Line 1, characters 62-66:
1 | let local_initializer () = pair (raise Exit) (let n = consume (-1) in (n, ()));;
                                                                  ^^^^
Error: Refinement could not be proved (counterexample)
|}]

let module_initializer () = pair (raise Exit)
  (let module M = struct let n = consume (-1) end in (M.n, ()));;
[%%expect{|
Line 2, characters 41-45:
2 |   (let module M = struct let n = consume (-1) end in (M.n, ()));;
                                             ^^^^
Error: Refinement could not be proved (counterexample)
|}]

let invoked_early () = pair (raise Exit)
  (let f : unit -> {n : int | n >= 0} = fun () -> -1 in (f (), ()));;
[%%expect{|
Line 2, characters 50-52:
2 |   (let f : unit -> {n : int | n >= 0} = fun () -> -1 in (f (), ()));;
                                                      ^^
Error: Refinement could not be proved (counterexample)
|}]

let explicit () = take (raise Exit) (refine_ (-1));;
[%%expect{|
Line 1, characters 36-50:
1 | let explicit () = take (raise Exit) (refine_ (-1));;
                                        ^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let explicit_component () = pair (raise Exit) (refine_ (-1), ());;
[%%expect{|
Line 1, characters 47-59:
1 | let explicit_component () = pair (raise Exit) (refine_ (-1), ());;
                                                   ^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let runtime_checked n =
  try ignore (take (raise Exit) (assume_ n : {n : int | n >= 0})); false
  with Assert_failure _ -> true | Exit -> false
let runtime_order = runtime_checked (-1), runtime_checked 1;;
[%%expect{|
val runtime_checked : int @ total -> bool = <fun>
val runtime_order : bool * bool = (true, false)
|}]

let early_dependency () = pair (identity 1) (take (identity 1) 0, ());;
[%%expect{|
Line 1, characters 63-64:
1 | let early_dependency () = pair (identity 1) (take (identity 1) 0, ());;
                                                                   ^
Error: Refinement could not be proved (counterexample)
|}]

let outer_source () = take (identity 1)
  ((if true then 0 else 0) : {n : int | n >= 1});;
[%%expect{|
Line 2, characters 17-18:
2 |   ((if true then 0 else 0) : {n : int | n >= 1});;
                     ^
Error: Refinement could not be proved (counterexample)
|}]

let wrong_branch_pair b = pair (identity 1) (if b then (0, ()) else (2, ()));;
[%%expect{|
Line 1, characters 56-57:
1 | let wrong_branch_pair b = pair (identity 1) (if b then (0, ()) else (2, ()));;
                                                            ^
Error: Refinement could not be proved (counterexample)
|}]

let projected_early () = take (raise Exit)
  ({item = (-1 : {n : int | n >= 0})}).item;;
[%%expect{|
Line 2, characters 12-14:
2 |   ({item = (-1 : {n : int | n >= 0})}).item;;
                ^^
Error: Refinement could not be proved (counterexample)
|}]

let nested_tuple : (lower : int) ->
    (({n : {m : int | m >= lower} | n >= 2} * unit) * unit) -> int =
  fun lower ((n, _), _) -> n
let nested_constructed () = nested_tuple (identity 1) ((2, ()), ());;
[%%expect{|
val nested_tuple :
  (lower : int) ->
  ({n : {m : int | m >= lower} | n >= 2} * unit) * unit -> int = <fun>
val nested_constructed : unit -> int = <fun>
|}]

let wrong_nested_constructed () = nested_tuple (identity 1) ((1, ()), ());;
[%%expect{|
Line 1, characters 62-63:
1 | let wrong_nested_constructed () = nested_tuple (identity 1) ((1, ()), ());;
                                                                  ^
Error: Refinement could not be proved (counterexample)
|}]

type observation = { number : int }
let use_projection : (n : int) -> (unit -> {x : int | x = n}) -> int =
  fun n get -> get ()
let projected_callback : (r : observation) ->
    (unit -> {x : int | x = r.number}) -> int =
  fun r get -> use_projection r.number get
let require_zero : (n : int) -> {u : unit | n = 0} -> unit =
  fun n premise -> ()
let projected_explicit (r : {r : observation | r.number = 0}) =
  require_zero r.number (refine_ ());;
[%%expect{|
type observation = { number : int; }
val use_projection : (n : int) -> (unit -> {x : int | x = n}) -> int = <fun>
val projected_callback :
  (r : observation) -> (unit -> {x : int | x = r.number}) -> int = <fun>
val require_zero : (n : int) -> {u : unit | n = 0} -> unit = <fun>
val projected_explicit : {r : observation | r.number = 0} -> unit = <fun>
|}]

let wrong_projection (r : observation) =
  require_zero r.number (refine_ ());;
[%%expect{|
Line 2, characters 24-36:
2 |   require_zero r.number (refine_ ());;
                            ^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let adapt_result : (h : int) -> (k : {n : int | n = h}) ->
    ((x : int) -> {y : int | y = x + h}) ->
    (x : int) -> {y : int | y = x + k} = fun h k f -> f
let adapt_input : (h : int) -> (k : {n : int | n = h}) ->
    ({x : int | x >= h} -> int) -> {x : int | x >= k} -> int =
  fun h k f -> f;;
[%%expect{|
val adapt_result :
  (h : int) ->
  (k : {n : int | n = h}) ->
  ((x : int) -> {y : int | y = (x + h)}) ->
  (x : int) -> {y : int | y = (x + k)} = <fun>
val adapt_input :
  (h : int) ->
  (k : {n : int | n = h}) ->
  ({x : int | x >= h} -> int) -> {x : int | x >= k} -> int = <fun>
|}]

let bad_result_adapter : (h : int) -> (k : int) ->
    ((x : int) -> {y : int | y = x + h}) ->
    (x : int) -> {y : int | y = x + k} = fun h k f -> f;;
[%%expect{|
Line 3, characters 54-55:
3 |     (x : int) -> {y : int | y = x + k} = fun h k f -> f;;
                                                          ^
Error: Refinement could not be proved (counterexample)
|}]

let bad_input_adapter : (h : int) -> (k : int) ->
    ({x : int | x >= h} -> int) -> {x : int | x >= k} -> int =
  fun h k f -> f;;
[%%expect{|
Line 3, characters 15-16:
3 |   fun h k f -> f;;
                   ^
Error: Refinement could not be proved (counterexample)
|}]

module Gadt_callbacks = struct
  type _ tag = Int : int tag | Bool : bool tag
  type 'a callback = unit -> 'a
  let choose : type a. a tag -> a callback = fun tag ->
    match tag with
    | Int -> fun () -> 1
    | Bool -> fun () -> true
end;;
[%%expect{|
module Gadt_callbacks :
  sig
    type _ tag = Int : int tag | Bool : bool tag
    type 'a callback = unit -> 'a
    val choose : 'a tag -> 'a callback
  end
|}]

let bad_partial_adapter : (h : int) -> (k : {n : int | n = h}) ->
    ((x : int) -> {y : int | y = x + h}) ->
    ((x : int) -> {y : int | y = x + k}) @ total =
  fun h k f -> f;;
[%%expect{|
Line 4, characters 15-16:
4 |   fun h k f -> f;;
                   ^
Error: The value "f" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 4, characters 15-16
         which is expected to be "total".
|}]

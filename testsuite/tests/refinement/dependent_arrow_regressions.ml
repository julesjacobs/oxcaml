(* TEST
 expect;
*)

let inferred = function x -> (x : int{ _ = x })
let inferred_two : int{ _ = 2 } = inferred 2

let alias_lambda : (whole : int) -> int{ _ = whole } =
  fun (inner as whole) -> let _ = whole in inner

let alias_cases : (whole : int) -> int{ _ = whole } = function
  | (inner as whole) -> let _ = whole in inner

let keep ?ignored:_ (value : int{ _ > 0 }) : int{ _ = value } = value
let optional_eta : (value : int{ _ > 0 }) -> int{ _ = value } = keep
let optional_eta_applied : int{ _ = 2 } = optional_eta 2

[%%expect {|
val inferred : (argument : int) -> int{ _ = argument } = <fun>
val inferred_two : int{ _ = 2 } = 2
val alias_lambda : (whole : int) -> int{ _ = whole } = <fun>
val alias_cases : (whole : int) -> int{ _ = whole } = <fun>
val keep : ?ignored:'a -> (value : int{ _ > 0 }) -> int{ _ = value } = <fun>
val optional_eta : (value : int{ _ > 0 }) -> int{ _ = value } = <fun>
val optional_eta_applied : int{ _ = 2 } = 2
|}]

let rootless : (whole : int) -> int{ _ = whole } = function
  | 0 -> 0
  | value -> value

[%%expect {|
Line 2, characters 4-5:
2 |   | 0 -> 0
        ^
Error: a dependent function case requires a root variable or alias pattern
|}]

module type Higher_order = sig
  val apply : (function_ : (int -> int)) -> int{ _ = function_ 0 }
end

[%%expect {|
module type Higher_order =
  sig val apply : (function_ : (int -> int)) -> int{ _ = function_ 0 } end
|}]

module Omitted_call_subject : sig end = struct
  module Total : sig
    val combine : a:int -> b:int -> int @@ total
    val wrap : (a:int -> int) -> int @@ total
  end = struct
    let combine ~a ~b = a + b
    let wrap f = f ~a:0
  end

  let observe : (x : int) -> int{ _ = x } = fun x -> x
  let result = observe (Total.wrap (Total.combine ~b:1))
end

[%%expect {|
Line 11, characters 35-55:
11 |   let result = observe (Total.wrap (Total.combine ~b:1))
                                        ^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed: an omitted argument cannot yet be represented in a verification condition
|}]

module Unrelated_case_refinement : sig end = struct
  let f = function
    | 0 -> (1 : int{ _ = 1 })
    | n -> n
end

[%%expect {|
module Unrelated_case_refinement : sig end
|}]

module Deferred_labeled_domain : sig end = struct
  let f ~(x : int) ~(y : int{ _ = x }) = ()
  let partial = f ~y:1
  let () = partial ~x:1
end

[%%expect {|
module Deferred_labeled_domain : sig end
|}]

let nested_capture (outer : int) =
  let inner (value : int) : unit{ outer = outer && value = value } = () in
  inner

let nested_capture_used = nested_capture 1 2

let nested_unreferenced (outer : int) =
  let inner (value : int) : unit{ value = value } = () in
  let _ = outer in
  inner

let nested_twice (outer : int) =
  let middle (middle_value : int) =
    let inner (value : int)
        : unit{
            outer = outer
            && middle_value = middle_value
            && value = value
          } =
      ()
    in
    inner
  in
  middle

[%%expect {|
val nested_capture :
  (outer : int) -> (value : int) -> unit{ outer = outer && value = value } =
  <fun>
val nested_capture_used : unit{ 1 = 1 && 2 = 2 } = ()
val nested_unreferenced : int -> (value : int) -> unit{ value = value } =
  <fun>
val nested_twice :
  (outer : int) ->
  (middle_value : int) ->
  (value : int) ->
  unit{ outer = outer && middle_value = middle_value && value = value } =
  <fun>
|}]

let nested_wrong (outer : int) =
  let inner (value : int) : unit{ outer = value } = () in
  inner

[%%expect {|
Line 2, characters 52-54:
2 |   let inner (value : int) : unit{ outer = value } = () in
                                                        ^^
Error: Refinement verification failed (not-proved)
|}]

type _ principal_gadt = Principal_int : int principal_gadt

let principal_gadt_plain : type a. a principal_gadt -> a =
  fun Principal_int -> 3

let principal_gadt_object =
  object (self)
    method private value = 3
    method get : type a. a principal_gadt -> a =
      fun Principal_int -> (self#value : int)
  end

[%%expect {|
type _ principal_gadt = Principal_int : int principal_gadt
val principal_gadt_plain : 'a principal_gadt -> 'a = <fun>
val principal_gadt_object : < get : 'a. 'a principal_gadt -> 'a > = <obj>
|}]

type recursive_nat = Zero | Successor of recursive_nat

let rec total_copy @ total =
  fun (value : recursive_nat) : recursive_nat{ _ = value } ->
    match value with
    | Zero -> Zero
    | Successor tail -> Successor (total_copy tail)

[%%expect {|
type recursive_nat = Zero | Successor of recursive_nat
val total_copy : (value : recursive_nat) -> recursive_nat{ _ = value } =
  <fun>
|}]

let rec total_copy_wrong @ total =
  fun (value : recursive_nat) : recursive_nat{ _ = value } ->
    match value with
    | Zero -> Zero
    | Successor tail -> total_copy_wrong tail

[%%expect {|
Line 5, characters 24-45:
5 |     | Successor tail -> total_copy_wrong tail
                            ^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (disproved)
|}]

let[@vox.def] rec total_copy_def (value : recursive_nat)
    : recursive_nat{ _ = value } =
  match value with
  | Zero -> Zero
  | Successor tail -> Successor (total_copy_def tail)

[%%expect {|
val total_copy_def : (value : recursive_nat) -> recursive_nat{ _ = value } =
  <fun>
val total_copy_def_def :
  (value : recursive_nat) ->
  unit{
   total_copy_def value = (match value with | Zero -> Zero | Successor tail -> Successor (total_copy_def tail))
   } =
  <fun>
|}]

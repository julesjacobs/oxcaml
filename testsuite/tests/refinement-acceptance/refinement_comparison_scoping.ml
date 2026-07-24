(* TEST
 expect;
*)

(* Direct integer comparison applications are total in ordinary code.  The
   polymorphic primitives remain partial as values and at non-integer types. *)

let expects_total (f @ total) = f
[%%expect {|
val expects_total : 'a @ total -> 'a = <fun>
|}]

let (integer_comparisons @ total) (left : int) (right : int) =
  left = right, left <> right,
  left < right, left <= right, left > right, left >= right
(* CR vox: [-principal] currently prints inferred [total logical] modes on
   leading parameters here; keep the accepted behavior until mode printing is
   normalized. *)
[%%expect {|
val integer_comparisons :
  int @ total logical -> int -> bool * bool * bool * bool * bool * bool =
  <fun>
|}]

type count = int

let (alias_and_refined_comparisons @ total)
    (left : count{ _ >= 0 }) (right : count) =
  left = right, left < right
[%%expect {|
type count = int
val alias_and_refined_comparisons :
  count{ _ >= 0 } @ total logical -> count -> bool * bool = <fun>
|}]

let[@vox.def] between (lower : int) (value : int) (upper : int) =
  lower <= value && value <= upper

let (between_reflexive @ total) (value : int)
    : unit{ between value value value = true } =
  between_def value value value;
  ()
[%%expect {|
val between : int @ total logical -> int @ total logical -> int -> bool =
  <fun>
val between_def :
  (lower : int) @ total logical ->
  (value : int) @ total logical ->
  (upper : int) ->
  unit{ between lower value upper = (lower <= value && value <= upper) } =
  <fun>
val between_reflexive :
  (value : int) -> unit{ between value value value = true } = <fun>
|}]

let () = ignore (expects_total ( = ))
[%%expect {|
Line 1, characters 31-36:
1 | let () = ignore (expects_total ( = ))
                                   ^^^^^
Error: This value is "partial" but is expected to be "total".
|}]

let () = ignore (expects_total ( <> ))
[%%expect {|
Line 1, characters 31-37:
1 | let () = ignore (expects_total ( <> ))
                                   ^^^^^^
Error: This value is "partial" but is expected to be "total".
|}]

let integer_equal = ( = )

let () =
  let f = fun (left : int) right -> integer_equal left right in
  ignore (expects_total f)
[%%expect {|
val integer_equal : 'a -> 'a -> bool = <fun>
Line 5, characters 24-25:
5 |   ignore (expects_total f)
                            ^
Error: This value is "partial"
         because it closes over the value "integer_equal" at line 4, characters 36-49
         which is "partial".
       However, the highlighted expression is expected to be "total".
|}]

let () =
  let f = fun (left : int list) right -> left = right in
  ignore (expects_total f)
[%%expect {|
Line 3, characters 24-25:
3 |   ignore (expects_total f)
                            ^
Error: This value is "partial" but is expected to be "total".
|}]

let () =
  let f = fun (left : int -> int) right -> left = right in
  ignore (expects_total f)
[%%expect {|
Line 3, characters 24-25:
3 |   ignore (expects_total f)
                            ^
Error: This value is "partial" but is expected to be "total".
|}]

(* Application rewriting must not turn a comparison selected as a function
   value into a direct comparison head. *)
let () =
  let f = fun (left : int) right -> left |> ( = ) right in
  ignore (expects_total f)
[%%expect {|
Line 3, characters 24-25:
3 |   ignore (expects_total f)
                            ^
Error: This value is "partial"
         because it closes over the value "(=)" at line 2, characters 44-49
         which is "partial".
       However, the highlighted expression is expected to be "total".
|}]

let () =
  let f = fun choice (value : int) ->
    value |> (if choice then ( = ) else ( <> ))
  in
  ignore (expects_total f)
[%%expect {|
Line 5, characters 24-25:
5 |   ignore (expects_total f)
                            ^
Error: This value is "partial"
         because it closes over the value "(=)" at line 3, characters 29-34
         which is "partial".
       However, the highlighted expression is expected to be "total".
|}]

let () =
  let f = fun (value : int) -> (fun () -> ( = )) () @@ value in
  ignore (expects_total f)
[%%expect {|
Line 3, characters 24-25:
3 |   ignore (expects_total f)
                            ^
Error: This value is "partial"
         because it closes over the value "(=)" at line 2, characters 42-47
         which is "partial".
       However, the highlighted expression is expected to be "total".
|}]

(* A non-integer comparison remains opaque when supplied to a dependent
   parameter.  In particular, [value = value] and [value <= value] are false
   for NaN at runtime, so neither may be lowered as a stable logical call. *)
let consume_true (condition : bool) (_proof : unit{ condition = true }) = ()
[%%expect {|
val consume_true : (condition : bool) -> unit{ condition = true } -> unit =
  <fun>
|}]

let int_equality_is_transparent (value : int) =
  consume_true (value = value) ()
[%%expect {|
val int_equality_is_transparent : int -> unit = <fun>
|}]

let nan_equality_is_opaque (value : float) =
  consume_true (value = value) ()
[%%expect {|
Line 2, characters 31-33:
2 |   consume_true (value = value) ()
                                   ^^
Error: Refinement verification failed (not-proved)
|}]

let nan_order_is_opaque (value : float) =
  consume_true (value <= value) ()
[%%expect {|
Line 2, characters 32-34:
2 |   consume_true (value <= value) ()
                                    ^^
Error: Refinement verification failed (not-proved)
|}]

(* An explicit totality modality on an external describes termination, not
   agreement between the primitive's runtime behavior and its logical model.
   Rewritten, labeled, and aliased applications therefore remain opaque at a
   non-integer carrier. *)
external total_float_equal : float -> float -> bool @@ total = "%equal"
external total_float_not_equal : float -> float -> bool @@ total = "%notequal"
external total_float_less : float -> float -> bool @@ total = "%lessthan"
external total_float_less_equal : float -> float -> bool @@ total = "%lessequal"
external total_float_greater : float -> float -> bool @@ total = "%greaterthan"
external total_float_greater_equal : float -> float -> bool @@ total
  = "%greaterequal"
external labeled_total_float_equal :
  left:float -> right:float -> bool @@ total = "%equal"

let total_float_equal_alias = total_float_equal
[%%expect {|
external total_float_equal : float -> float -> bool = "%equal"
external total_float_not_equal : float -> float -> bool = "%notequal"
external total_float_less : float -> float -> bool = "%lessthan"
external total_float_less_equal : float -> float -> bool = "%lessequal"
external total_float_greater : float -> float -> bool = "%greaterthan"
external total_float_greater_equal : float -> float -> bool = "%greaterequal"
external labeled_total_float_equal : left:float -> right:float -> bool
  = "%equal"
val total_float_equal_alias : float -> float -> bool = <fun>
|}]

(* A trusted external's declared totality describes termination and is
   independent of whether its result has a logical comparison model.  All
   equivalent application spellings must therefore remain total. *)
let (explicit_total_float_equal_alias @ total) = total_float_equal

let (declared_total_float_direct @ total) left right =
  total_float_equal left right

let (declared_total_float_pipe @ total) left right =
  right |> total_float_equal left

let (declared_total_float_apply @ total) left right =
  total_float_equal left @@ right

let (declared_total_float_labeled @ total) left right =
  labeled_total_float_equal ~left ~right

let (declared_total_float_alias @ total) left right =
  explicit_total_float_equal_alias left right
[%%expect {|
val explicit_total_float_equal_alias : float -> float -> bool = <fun>
val declared_total_float_direct : float @ total logical -> float -> bool =
  <fun>
val declared_total_float_pipe : float @ total logical -> float -> bool =
  <fun>
val declared_total_float_apply : float @ total logical -> float -> bool =
  <fun>
val declared_total_float_labeled : float @ total logical -> float -> bool =
  <fun>
val declared_total_float_alias : float @ total logical -> float -> bool =
  <fun>
|}]

let total_float_equal_direct_is_opaque value =
  consume_true (total_float_equal value value) ()
[%%expect {|
Line 2, characters 47-49:
2 |   consume_true (total_float_equal value value) ()
                                                   ^^
Error: Refinement verification failed (not-proved)
|}]

let total_float_equal_pipe_is_opaque value =
  consume_true (value |> total_float_equal value) ()
[%%expect {|
Line 2, characters 50-52:
2 |   consume_true (value |> total_float_equal value) ()
                                                      ^^
Error: Refinement verification failed (not-proved)
|}]

let total_float_equal_apply_is_opaque value =
  consume_true (total_float_equal value @@ value) ()
[%%expect {|
Line 2, characters 50-52:
2 |   consume_true (total_float_equal value @@ value) ()
                                                      ^^
Error: Refinement verification failed (not-proved)
|}]

let labeled_total_float_equal_is_opaque value =
  consume_true
    (labeled_total_float_equal ~left:value ~right:value) ()
[%%expect {|
Line 3, characters 57-59:
3 |     (labeled_total_float_equal ~left:value ~right:value) ()
                                                             ^^
Error: Refinement verification failed (not-proved)
|}]

let returns_boolean (condition : bool) : bool{ _ = condition } = condition
[%%expect {|
val returns_boolean : (condition : bool) -> bool{ _ = condition } = <fun>
|}]

let integer_dependent_result_is_transparent (value : int) =
  let result = returns_boolean (value = value) in
  consume_true result ()
[%%expect {|
val integer_dependent_result_is_transparent : int -> unit = <fun>
|}]

(* A dependent result type must not preserve a structural float-comparison
   subject that a later binder could reinterpret as mathematical equality. *)
let labeled_float_dependent_result_is_opaque value =
  let result =
    returns_boolean
      (labeled_total_float_equal ~left:value ~right:value)
  in
  consume_true result ()
[%%expect {|
Line 6, characters 22-24:
6 |   consume_true result ()
                          ^^
Error: Refinement verification failed (not-proved)
|}]

let total_float_equal_alias_is_opaque value =
  consume_true (total_float_equal_alias value value) ()
[%%expect {|
Line 2, characters 53-55:
2 |   consume_true (total_float_equal_alias value value) ()
                                                         ^^
Error: Refinement verification failed (not-proved)
|}]

let total_float_equal_local_alias_is_opaque value =
  let compare = total_float_equal in
  consume_true (compare value value) ()
[%%expect {|
Line 3, characters 37-39:
3 |   consume_true (compare value value) ()
                                         ^^
Error: Refinement verification failed (not-proved)
|}]

let total_float_equal_partial_head_is_opaque value =
  let compare_with_value = total_float_equal value in
  consume_true (compare_with_value value) ()
[%%expect {|
Line 3, characters 42-44:
3 |   consume_true (compare_with_value value) ()
                                              ^^
Error: Refinement verification failed (not-proved)
|}]

let total_float_equal_conditional_head_is_opaque choice value =
  consume_true
    ((if choice then total_float_equal else total_float_equal) value value) ()
[%%expect {|
Line 3, characters 76-78:
3 |     ((if choice then total_float_equal else total_float_equal) value value) ()
                                                                                ^^
Error: Refinement verification failed (not-proved)
|}]

let total_float_equal_lambda_head_is_opaque value =
  consume_true
    ((fun left right -> total_float_equal left right) value value) ()
[%%expect {|
Line 3, characters 67-69:
3 |     ((fun left right -> total_float_equal left right) value value) ()
                                                                       ^^
Error: Refinement verification failed (not-proved)
|}]

let total_float_equal_returned_from_lambda_is_opaque value =
  consume_true (((fun () -> total_float_equal) ()) value value) ()
[%%expect {|
Line 2, characters 64-66:
2 |   consume_true (((fun () -> total_float_equal) ()) value value) ()
                                                                    ^^
Error: Refinement verification failed (not-proved)
|}]

let total_float_not_equal_is_opaque value =
  consume_true (value |> total_float_not_equal value) ()
[%%expect {|
Line 2, characters 54-56:
2 |   consume_true (value |> total_float_not_equal value) ()
                                                          ^^
Error: Refinement verification failed (not-proved)
|}]

let total_float_less_is_opaque value =
  consume_true (value |> total_float_less value) ()
[%%expect {|
Line 2, characters 49-51:
2 |   consume_true (value |> total_float_less value) ()
                                                     ^^
Error: Refinement verification failed (not-proved)
|}]

let total_float_less_equal_is_opaque value =
  consume_true (value |> total_float_less_equal value) ()
[%%expect {|
Line 2, characters 55-57:
2 |   consume_true (value |> total_float_less_equal value) ()
                                                           ^^
Error: Refinement verification failed (not-proved)
|}]

let total_float_greater_is_opaque value =
  consume_true (value |> total_float_greater value) ()
[%%expect {|
Line 2, characters 52-54:
2 |   consume_true (value |> total_float_greater value) ()
                                                        ^^
Error: Refinement verification failed (not-proved)
|}]

let total_float_greater_equal_is_opaque value =
  consume_true (value |> total_float_greater_equal value) ()
[%%expect {|
Line 2, characters 58-60:
2 |   consume_true (value |> total_float_greater_equal value) ()
                                                              ^^
Error: Refinement verification failed (not-proved)
|}]

let () =
  let f = fun (left : float) right -> left < right in
  ignore (expects_total f)
[%%expect {|
Line 3, characters 24-25:
3 |   ignore (expects_total f)
                            ^
Error: This value is "partial" but is expected to be "total".
|}]

type boxed_int = Boxed of int

let () =
  let f = fun (left : boxed_int) right -> left = right in
  ignore (expects_total f)
[%%expect {|
type boxed_int = Boxed of int
Line 5, characters 24-25:
5 |   ignore (expects_total f)
                            ^
Error: This value is "partial" but is expected to be "total".
|}]

external opaque_int : unit -> int = "vox_test_opaque_int"

let () =
  let f = fun () -> opaque_int () < 0 in
  ignore (expects_total f)
[%%expect {|
external opaque_int : unit -> int = "vox_test_opaque_int"
Line 5, characters 24-25:
5 |   ignore (expects_total f)
                            ^
Error: This value is "partial"
         because it closes over the value "opaque_int" at line 4, characters 20-30
         which is "partial".
       However, the highlighted expression is expected to be "total".
|}]

let returns_argument (value : int) : int{ _ = value } = value
[%%expect {|
val returns_argument : (value : int) -> int{ _ = value } = <fun>
|}]

let require_equal (left : int) (_right : int{ _ = left }) = ()
[%%expect {|
val require_equal : (left : int) -> int{ _ = left } -> unit = <fun>
|}]

let unstable_argument_value_is_preserved () =
  let argument = read_int () in
  let result = returns_argument argument in
  require_equal argument result
[%%expect {|
val unstable_argument_value_is_preserved : unit -> unit = <fun>
|}]

(* Separate evaluations of an unstable argument must remain separate in a
   dependent result contract, even when their source shapes are identical. *)
let unstable_dependent_results_do_not_alias () =
  let first = returns_argument (read_int ()) in
  let second = returns_argument (read_int ()) in
  require_equal first second
[%%expect {|
Line 4, characters 22-28:
4 |   require_equal first second
                          ^^^^^^
Error: Refinement verification failed (not-proved)
|}]

(* A structurally recursive function is total and can therefore occur in a
   refinement predicate.  The reflexive predicate keeps this test focused on
   predicate totality rather than recursive equation emission. *)
let rec structural_length @ total = function
  | [] -> 0
  | _ :: tail -> 1 + structural_length tail

type structural_predicate =
  int{
    structural_length ([] : int list)
    = structural_length ([] : int list)
  }

[%%expect {|
val structural_length : 'a list -> int = <fun>
type structural_predicate =
    int{ structural_length [] = structural_length [] }
|}]

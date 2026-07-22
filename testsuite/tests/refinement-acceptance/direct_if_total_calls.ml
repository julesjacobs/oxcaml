(* TEST
 expect;
*)

external int_equal : int -> int -> bool @@ total = "%equal"

let (is_zero @ total) (x : int) : bool{ _ = int_equal x 0 } =
  int_equal x 0

let direct_then x =
  if is_zero x then (x : int{ _ = 0 }) else 0

let direct_else x =
  if is_zero x then 0 else (x : int{ _ <> 0 })

let bound_then x =
  let result = is_zero x in
  if result then (x : int{ _ = 0 }) else 0

let is_zero_alias = is_zero

let aliased_direct x =
  if is_zero_alias x then (x : int{ _ = 0 }) else 0

[%%expect {|
external int_equal : int -> int -> bool = "%equal"
val is_zero : (x : int) -> bool{ _ = (x = 0) } = <fun>
val direct_then : int -> int = <fun>
val direct_else : int -> int = <fun>
val bound_then : int -> int = <fun>
val is_zero_alias : (x : int) -> bool{ _ = (x = 0) } = <fun>
val aliased_direct : int -> int = <fun>
|}]

external partial_is_zero : x:int -> bool{ _ = int_equal x 0 }
  = "vox_partial_is_zero"

(* A call without a stable total head does not contribute a branch
   observation.  Its result contract alone does not establish [x = 0]. *)
let partial_direct_rejected x =
  if partial_is_zero ~x then (x : int{ _ = 0 }) else 0

[%%expect {|
external partial_is_zero : x:(x : int) -> bool{ _ = (x = 0) }
  = "vox_partial_is_zero"
Line 7, characters 29-47:
7 |   if partial_is_zero ~x then (x : int{ _ = 0 }) else 0
                                 ^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

(* Nested stable conditions and higher-order total predicate parameters use
   the same observation rule as a direct named total call. *)
let nested_let_condition x =
  if (let answer = is_zero x in answer)
  then (x : int{ _ = 0 })
  else 0

let higher_order_condition
    (predicate : ((x:int) -> bool{ _ = int_equal x 0 }) @ total)
    x =
  if predicate x then (x : int{ _ = 0 }) else 0

[%%expect {|
val nested_let_condition : int -> int = <fun>
val higher_order_condition :
  ((x : int) -> bool{ _ = (x = 0) }) @ total -> int -> int = <fun>
|}]

type mutable_box = { mutable flag : bool }

let mutable_field_rejected box =
  if box.flag then (0 : int{ false }) else 0

[%%expect {|
type mutable_box = { mutable flag : bool; }
Line 4, characters 19-37:
4 |   if box.flag then (0 : int{ false }) else 0
                       ^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (disproved)
|}]

(* A short-circuit expression can still return without evaluating its raising
   RHS, so the enclosing result obligation remains reachable. *)
let true_or_raise_reachable () : int{ false } =
  if true || raise Exit then 0 else 1

[%%expect {|
Line 2, characters 29-30:
2 |   if true || raise Exit then 0 else 1
                                 ^
Error: Refinement verification failed (disproved)
|}]

let false_and_raise_reachable () : int{ false } =
  if false && raise Exit then 0 else 1

[%%expect {|
Line 2, characters 30-31:
2 |   if false && raise Exit then 0 else 1
                                  ^
Error: Refinement verification failed (disproved)
|}]

let next_flag () = read_int () > 0

let require_true (value : bool{ _ = true }) = value

(* Distinct evaluations of an ordinary call remain distinct. *)
let repeated_ordinary_call_rejected () =
  if next_flag () then require_true (next_flag ()) else false

[%%expect {|
val next_flag : unit -> bool = <fun>
val require_true : bool{ _ = true } -> bool = <fun>
Line 7, characters 36-50:
7 |   if next_flag () then require_true (next_flag ()) else false
                                        ^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

let ordinary_nested_rejected () =
  if next_flag () && true
  then require_true (next_flag ())
  else false

[%%expect {|
Line 3, characters 20-34:
3 |   then require_true (next_flag ())
                        ^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

(* A mutable read is not a stable branch subject. *)
let mutable_condition_rejected () =
  let mutable flag = true in
  if flag then (0 : int{ false }) else 0

[%%expect {|
Line 3, characters 15-33:
3 |   if flag then (0 : int{ false }) else 0
                   ^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (disproved)
|}]

let unreachable_direct_condition () : int{ false } =
  if raise Exit then 0 else 1

let unreachable_nested_condition () : int{ false } =
  if Sys.opaque_identity (raise Exit) then 0 else 1

[%%expect {|
val unreachable_direct_condition : unit -> int{ false } = <fun>
val unreachable_nested_condition : unit -> int{ false } = <fun>
|}]

let established_key = Sys.opaque_identity 0

external establish_key : unit -> unit{ established_key = 7 } @@ total
  = "%identity"

(* Facts established while evaluating the condition are available only after
   the condition has completed, so both result branches may use them. *)
let condition_precedes_branch flag =
  if (ignore (establish_key ()); flag)
  then (established_key : int{ _ = 7 })
  else (established_key : int{ _ = 7 })

[%%expect {|
val established_key : int = 0
external establish_key : unit -> unit{ established_key = 7 } = "%identity"
val condition_precedes_branch : bool -> int = <fun>
|}]

let require_key (_ : unit{ established_key = 7 }) = true

(* A later branch cannot establish a precondition needed by the condition. *)
let branch_cannot_precede_condition () =
  if require_key () then ignore (establish_key ()) else ()

[%%expect {|
val require_key : unit{ established_key = 7 } -> bool = <fun>
Line 5, characters 17-19:
5 |   if require_key () then ignore (establish_key ()) else ()
                     ^^
Error: Refinement verification failed (not-proved)
|}]

(* Facts from an unevaluated RHS never escape the short-circuit expression. *)
let false_and_rhs_fact_rejected () =
  ignore (false && (ignore (establish_key ()); true));
  (established_key : int{ _ = 7 })

[%%expect {|
Line 3, characters 2-34:
3 |   (established_key : int{ _ = 7 })
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

let true_or_rhs_fact_rejected () =
  ignore (true || (ignore (establish_key ()); false));
  (established_key : int{ _ = 7 })

[%%expect {|
Line 3, characters 2-34:
3 |   (established_key : int{ _ = 7 })
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

let requires_zero (_ : int{ _ = 0 }) = true

(* The left observation is available inside the conditionally evaluated RHS. *)
let left_observation_proves_rhs x =
  if is_zero x && requires_zero x then 0 else 1

[%%expect {|
val requires_zero : int{ _ = 0 } -> bool = <fun>
val left_observation_proves_rhs : int -> int = <fun>
|}]

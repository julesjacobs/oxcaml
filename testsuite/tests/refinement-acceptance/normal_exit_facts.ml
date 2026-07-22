(* TEST
 flags += "-keywords 5.3";
 expect;
*)

external int_equal : int -> int -> bool @@ total = "%equal"

let (is_zero @ total) x : bool{ _ = int_equal x 0 } = int_equal x 0

(* A completed total call contributes both its result contract and the
   observation that the condition was true on the then path. *)
let direct_call_condition x =
  if is_zero x then (x : int{ _ = 0 }) else 0

let negated_call_condition x =
  if is_zero x then 0 else (x : int{ _ <> 0 })

let nested_call_condition x =
  if is_zero x then
    if not (is_zero x) then 0 else (x : int{ _ = 0 })
  else 0

let is_zero_alias = is_zero

module Reexport = struct
  let is_zero = is_zero_alias
end

let aliased_call_condition x =
  if is_zero_alias x then (x : int{ _ = 0 }) else 0

let reexported_call_condition x =
  if Reexport.is_zero x then (x : int{ _ = 0 }) else 0

[%%expect {|
external int_equal : int -> int -> bool = "%equal"
val is_zero : (x : int) -> bool{ _ = (x = 0) } = <fun>
val direct_call_condition : int -> int = <fun>
val negated_call_condition : int -> int = <fun>
val nested_call_condition : int -> int = <fun>
val is_zero_alias : (x : int) -> bool{ _ = (x = 0) } = <fun>
module Reexport : sig val is_zero : (x : int) -> bool{ _ = (x = 0) } end
val aliased_call_condition : int -> int = <fun>
val reexported_call_condition : int -> int = <fun>
|}]

external partial_is_zero : x:int -> bool{ _ = int_equal x 0 }
  = "vox_partial_is_zero"

(* A call that is not known total is deliberately not reified as a stable
   branch observation. *)
let partial_call_condition_rejected x =
  if partial_is_zero ~x then (x : int{ _ = 0 }) else 0

[%%expect {|
external partial_is_zero : x:(x : int) -> bool{ _ = (x = 0) }
  = "vox_partial_is_zero"
Line 7, characters 29-47:
7 |   if partial_is_zero ~x then (x : int{ _ = 0 }) else 0
                                 ^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

(* Reads from mutable binders must not become persistent branch facts. *)
let mutable_condition_rejected () =
  let mutable flag = true in
  if flag then (0 : int{ false }) else 0

[%%expect {|
Line 3, characters 15-33:
3 |   if flag then (0 : int{ false }) else 0
                   ^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (disproved)
|}]

let join_key = Sys.opaque_identity 0

external join_law : unit -> unit{ join_key = 7 } @@ total = "%identity"

(* Every normally returning arm establishes the same proposition. *)
let all_value_arms flag =
  let () =
    match flag with
    | true -> ignore (join_law ())
    | false -> ignore (join_law ())
  in
  (join_key : int{ _ = 7 })

(* One arm is not enough: that proposition is absent on a returning path. *)
let one_value_arm_rejected flag =
  let () =
    match flag with
    | true -> ignore (join_law ())
    | false -> ()
  in
  (join_key : int{ _ = 7 })

[%%expect {|
val join_key : int = 0
external join_law : unit -> unit{ join_key = 7 } = "%identity"
val all_value_arms : bool -> int{ _ = 7 } = <fun>
Line 21, characters 2-27:
21 |   (join_key : int{ _ = 7 })
       ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

(* A path that definitely does not return is excluded from the intersection. *)
let nonreturning_value_arm flag =
  let () =
    match flag with
    | true -> ignore (join_law ())
    | false -> raise Exit
  in
  (join_key : int{ _ = 7 })

let guarded_value_arms flag guard =
  let () =
    match flag with
    | true when guard -> ignore (join_law ())
    | _ -> ignore (join_law ())
  in
  (join_key : int{ _ = 7 })

let nested_value_arms outer inner =
  let () =
    match outer with
    | true ->
      (match inner with
       | true -> ignore (join_law ())
       | false -> ignore (join_law ()))
    | false -> ignore (join_law ())
  in
  (join_key : int{ _ = 7 })

[%%expect {|
val nonreturning_value_arm : bool -> int{ _ = 7 } = <fun>
val guarded_value_arms : bool -> bool -> int{ _ = 7 } = <fun>
val nested_value_arms : bool -> bool -> int{ _ = 7 } = <fun>
|}]

(* A guard that cannot return makes its RHS unreachable.  The result
   refinement is therefore checked only on the fallback arm. *)
let nonreturning_guard_result flag : int{ _ = 7 } =
  match flag with
  | true when raise Exit -> 0
  | _ -> 7

[%%expect {|
val nonreturning_guard_result : bool -> int{ _ = 7 } = <fun>
|}]

(* A returning guarded arm still has to establish the outer result. *)
let returning_guard_wrong_result flag guard : int{ _ = 7 } =
  match flag with
  | true when guard -> 0
  | _ -> 7

[%%expect {|
Line 3, characters 23-24:
3 |   | true when guard -> 0
                           ^
Error: Refinement verification failed (disproved)
|}]

(* Returning exception paths participate in the same intersection. *)
let value_and_exception_arms flag =
  let () =
    match (if flag then () else raise Exit) with
    | () -> ignore (join_law ())
    | exception Exit -> ignore (join_law ())
  in
  (join_key : int{ _ = 7 })

let exception_arm_missing_rejected flag =
  let () =
    match (if flag then () else raise Exit) with
    | () -> ignore (join_law ())
    | exception Exit -> ()
  in
  (join_key : int{ _ = 7 })

[%%expect {|
val value_and_exception_arms : bool -> int{ _ = 7 } = <fun>
Line 15, characters 2-27:
15 |   (join_key : int{ _ = 7 })
       ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

type _ Effect.t += Join_effect : unit Effect.t

let value_and_effect_arms flag =
  let () =
    match (if flag then () else Effect.perform Join_effect) with
    | () -> ignore (join_law ())
    | effect Join_effect, _ -> ignore (join_law ())
  in
  (join_key : int{ _ = 7 })

let effect_arm_missing_rejected flag =
  let () =
    match (if flag then () else Effect.perform Join_effect) with
    | () -> ignore (join_law ())
    | effect Join_effect, _ -> ()
  in
  (join_key : int{ _ = 7 })

[%%expect {|
type _ Stdlib.Effect.t += Join_effect : unit Effect.t
val value_and_effect_arms : bool -> int{ _ = 7 } = <fun>
Line 17, characters 2-27:
17 |   (join_key : int{ _ = 7 })
       ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

(* Outer match-result refinements are checked at each normally returning
   value, exception, and effect leaf, including mixed matches. *)
let mixed_exception_result flag : int{ _ = 7 } =
  match (if flag then 7 else raise Exit) with
  | value -> value
  | exception Exit -> 7

[%%expect {|
val mixed_exception_result : bool -> int{ _ = 7 } = <fun>
|}]

let mixed_exception_wrong_result flag : int{ _ = 7 } =
  match (if flag then 7 else raise Exit) with
  | value -> value
  | exception Exit -> 8

[%%expect {|
Line 4, characters 22-23:
4 |   | exception Exit -> 8
                          ^
Error: Refinement verification failed (disproved)
|}]

type _ Effect.t += Result_effect : int Effect.t

let mixed_effect_result flag : int{ _ = 7 } =
  match (if flag then 7 else Effect.perform Result_effect) with
  | value -> value
  | effect Result_effect, _ -> 7

[%%expect {|
type _ Stdlib.Effect.t += Result_effect : int Effect.t
val mixed_effect_result : bool -> int{ _ = 7 } = <fun>
|}]

let mixed_effect_wrong_result flag : int{ _ = 7 } =
  match (if flag then 7 else Effect.perform Result_effect) with
  | value -> value
  | effect Result_effect, _ -> 8

[%%expect {|
Line 4, characters 31-32:
4 |   | effect Result_effect, _ -> 8
                                   ^
Error: Refinement verification failed (disproved)
|}]

(* A let expression closes its local binders but retains established facts
   that mention only the surrounding scope. *)
let let_expression_fact () =
  (let local = 0 in
   ignore local;
   ignore (join_law ()));
  (join_key : int{ _ = 7 })

let nested_let_expression_fact () =
  (let outer_local = 0 in
   let inner_local = outer_local in
   ignore inner_local;
   ignore (join_law ()));
  (join_key : int{ _ = 7 })

[%%expect {|
val let_expression_fact : unit -> int{ _ = 7 } = <fun>
val nested_let_expression_fact : unit -> int{ _ = 7 } = <fun>
|}]

let stable_let_result_summary () =
  let result =
    (let local = 7 in local)
  in
  (result : int{ _ = 7 })

[%%expect {|
val stable_let_result_summary : unit -> int{ _ = 7 } = <fun>
|}]

let stable_destructuring_let_is_conservative () =
  let result =
    let left, _right = 7, 8 in
    left
  in
  (result : int{ true })

[%%expect {|
val stable_destructuring_let_is_conservative : unit -> int{ true } = <fun>
|}]

let unstable_let_result_rejected () =
  let result =
    (let local = read_int () in local)
  in
  (result : int{ _ = 7 })

[%%expect {|
Line 5, characters 2-25:
5 |   (result : int{ _ = 7 })
      ^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

(* An unannotated parameter may acquire the demanded refinement through AUTO;
   the same-named inner binder is irrelevant to that imposition. *)
let shadowed_parameter_imposition x =
  (let x = (7 : int{ _ = 7 }) in ignore x);
  (x : int{ _ = 7 })

[%%expect {|
val shadowed_parameter_imposition : int{ _ = 7 } -> int{ _ = 7 } = <fun>
|}]

(* An explicit parameter annotation disables AUTO imposition, so the inner
   binder still cannot establish anything about the outer one. *)
let local_binder_does_not_escape (x : int) =
  (let x = (7 : int{ _ = 7 }) in ignore x);
  (x : int{ _ = 7 })

[%%expect {|
Line 3, characters 2-20:
3 |   (x : int{ _ = 7 })
      ^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

(* Generic applications evaluate their head and supplied arguments before the
   callee is invoked.  If any of those evaluations cannot return, no callee
   domain or result obligation is reachable. *)
let requires_false_first (_ : unit{ false }) (_ : unit) = ()
let requires_false_second (_ : unit) (_ : unit{ false }) = ()

let nonreturning_last_argument () =
  ignore
    ((requires_false_first () (raise Exit)) : unit{ false })

let nonreturning_first_argument () =
  ignore
    ((requires_false_second (raise Exit) ()) : unit{ false })

[%%expect {|
val requires_false_first : unit{ false } -> unit -> unit = <fun>
val requires_false_second : unit -> unit{ false } -> unit = <fun>
val nonreturning_last_argument : unit -> unit = <fun>
val nonreturning_first_argument : unit -> unit = <fun>
|}]

let returning_argument_contract_rejected () =
  requires_false_first () ()

[%%expect {|
Line 2, characters 23-25:
2 |   requires_false_first () ()
                           ^^
Error: Refinement verification failed (disproved)
|}]

(* A nonreturning condition cannot reach either result branch. *)
let nonreturning_if_condition () : int{ false } =
  if raise Exit then 0 else 1

[%%expect {|
val nonreturning_if_condition : unit -> int{ false } = <fun>
|}]

let returning_if_wrong_result flag : int{ _ = 7 } =
  if flag then 8 else 7

[%%expect {|
Line 2, characters 15-16:
2 |   if flag then 8 else 7
                   ^
Error: Refinement verification failed (disproved)
|}]

(* A nonreturning scrutinee cannot reach a value arm, but an exception arm is
   still a possible result path. *)
let nonreturning_match_value_arm () : int{ false } =
  match raise Exit with
  | _ -> 0

let nonreturning_match_exception_result () : int{ _ = 7 } =
  match raise Exit with
  | _ -> 0
  | exception Exit -> 7

[%%expect {|
val nonreturning_match_value_arm : unit -> int{ false } = <fun>
val nonreturning_match_exception_result : unit -> int{ _ = 7 } = <fun>
|}]

let nonreturning_match_exception_wrong () : int{ _ = 7 } =
  match raise Exit with
  | _ -> 7
  | exception Exit -> 8

[%%expect {|
Line 4, characters 22-23:
4 |   | exception Exit -> 8
                          ^
Error: Refinement verification failed (disproved)
|}]

(* The left operand is unconditional, while facts from a short-circuit right
   operand survive only inside that operand, not after the join. *)
let (join_key_is_7 @ total) () : bool{ _ = int_equal join_key 7 } =
  int_equal join_key 7

external left_establishes_join : unit -> bool{ join_key = 7 } @@ total
  = "%identity"

let short_and_left_fact () =
  let _ = left_establishes_join () && true in
  (join_key : int{ _ = 7 })

let short_or_left_fact () =
  let _ = left_establishes_join () || false in
  (join_key : int{ _ = 7 })

let short_and_taken_path () =
  join_key_is_7 ()
  && (ignore (join_key : int{ _ = 7 }); true)

let short_or_taken_path () =
  join_key_is_7 ()
  || (ignore (join_key : int{ _ <> 7 }); false)

[%%expect {|
val join_key_is_7 : unit -> bool{ _ = (join_key = 7) } = <fun>
external left_establishes_join : unit -> bool{ join_key = 7 } = "%identity"
val short_and_left_fact : unit -> int{ _ = 7 } = <fun>
val short_or_left_fact : unit -> int{ _ = 7 } = <fun>
val short_and_taken_path : unit -> bool = <fun>
val short_or_taken_path : unit -> bool = <fun>
|}]

let impossible_bool () : bool{ false } = raise Exit

let short_and_rhs_does_not_escape () =
  let _ = false && impossible_bool () in
  (0 : int{ false })

[%%expect {|
val impossible_bool : unit -> bool{ false } = <fun>
Line 5, characters 2-20:
5 |   (0 : int{ false })
      ^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (disproved)
|}]

let short_or_rhs_does_not_escape () =
  let _ = true || impossible_bool () in
  (0 : int{ false })

[%%expect {|
Line 3, characters 2-20:
3 |   (0 : int{ false })
      ^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (disproved)
|}]

(* Normal-completion analysis is compositional: a nested control expression
   whose discriminant cannot return also cannot reach an enclosing callee or
   enclosing result obligation. *)
let nested_nonreturning_if_argument () =
  requires_false_first (if raise Exit then () else ()) ()

let nested_nonreturning_match_argument () =
  requires_false_first
    (match (if raise Exit then () else ()) with
     | () -> ())
    ()

let nested_nonreturning_if_result () : int{ false } =
  if (if raise Exit then true else false) then 0 else 1

let nested_nonreturning_match_result () : int{ false } =
  match (match raise Exit with _ -> ()) with
  | () -> 0

[%%expect {|
val nested_nonreturning_if_argument : unit -> unit = <fun>
val nested_nonreturning_match_argument : unit -> unit = <fun>
val nested_nonreturning_if_result : unit -> int{ false } = <fun>
val nested_nonreturning_match_result : unit -> int{ false } = <fun>
|}]

(* If the nested control expression can return, the same impossible domain
   remains an ordinary, reachable obligation. *)
let nested_returning_argument_rejected flag =
  requires_false_first (if flag then () else ()) ()

[%%expect {|
Line 2, characters 23-48:
2 |   requires_false_first (if flag then () else ()) ()
                           ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (disproved)
|}]

(* A stable total guard contributes its true observation on the taken edge
   and its false observation on the fallthrough edge. *)
let guard_taken_observation x =
  match () with
  | () when is_zero x -> (x : int{ _ = 0 })
  | () -> 0

let guard_false_observation x =
  match () with
  | () when is_zero x -> 0
  | () -> (x : int{ _ <> 0 })

[%%expect {|
val guard_taken_observation : int -> int{ _ = 0 } = <fun>
val guard_false_observation : int -> int = <fun>
|}]

let guard_key = Sys.opaque_identity 0

external guard_law : unit -> bool{ guard_key = 7 } @@ total = "%identity"

(* Facts established by evaluating an irrefutably selected guard hold on
   both its true and false normal exits. *)
let guard_fact_on_every_exit () =
  let () =
    match () with
    | () when guard_law () -> ()
    | () -> ()
  in
  (guard_key : int{ _ = 7 })

let guard_fact_at_fallthrough_leaf () : int{ _ = 7 } =
  match () with
  | () when guard_law () -> guard_key
  | () -> guard_key

[%%expect {|
val guard_key : int = 0
external guard_law : unit -> bool{ guard_key = 7 } = "%identity"
val guard_fact_on_every_exit : unit -> int{ _ = 7 } = <fun>
val guard_fact_at_fallthrough_leaf : unit -> int{ _ = 7 } = <fun>
|}]

(* A refutable pattern can bypass its guard, so the guard fact is not valid
   after the match. *)
let refutable_guard_fact_rejected flag =
  let () =
    match flag with
    | true when guard_law () -> ()
    | _ -> ()
  in
  (guard_key : int{ _ = 7 })

[%%expect {|
Line 7, characters 2-28:
7 |   (guard_key : int{ _ = 7 })
      ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

(* Once an irrefutable handler consumes sequential fallthrough, walking later
   unreachable guarded handlers must not make a still-later handler reachable. *)
let[@warning "-11"] unreachable_try_handlers_do_not_revive () =
  let () =
    try raise Exit with
    | _ -> ignore (join_law ())
    | _ when true -> ()
    | _ -> ()
  in
  (join_key : int{ _ = 7 })

let[@warning "-11"] unreachable_exception_handlers_do_not_revive () =
  let () =
    match raise Exit with
    | _ -> ()
    | exception _ -> ignore (join_law ())
    | exception _ when true -> ()
    | exception _ -> ()
  in
  (join_key : int{ _ = 7 })

type _ Effect.t += Unreachable_handler_effect : int Effect.t

let[@warning "-11"] unreachable_effect_handlers_do_not_revive () =
  let _ =
    match Effect.perform Unreachable_handler_effect with
    | value -> ignore (join_law ()); value
    | effect _, _ -> ignore (join_law ()); 0
    | effect _, _ when true -> 0
    | effect _, _ -> 0
  in
  (join_key : int{ _ = 7 })

[%%expect {|
val unreachable_try_handlers_do_not_revive : unit -> int{ _ = 7 } = <fun>
val unreachable_exception_handlers_do_not_revive : unit -> int{ _ = 7 } =
  <fun>
type _ Stdlib.Effect.t += Unreachable_handler_effect : int Effect.t
val unreachable_effect_handlers_do_not_revive : unit -> int{ _ = 7 } = <fun>
|}]

exception Mixed_return of int

(* A mixed value/exception arm can remain reachable through normal value
   completion after its exception alternative has become unreachable. *)
let[@warning "-11"] mixed_arm_all_returning_paths_establish_fact g =
  let r =
    match g () with
    | exception _ -> ignore (join_law ()); 7
    | x | exception Mixed_return x -> ignore (join_law ()); x
  in
  ignore r;
  (join_key : int{ _ = 7 })

[%%expect {|
exception Mixed_return of int
val mixed_arm_all_returning_paths_establish_fact :
  (unit -> int) -> int{ _ = 7 } = <fun>
|}]

(* The same value route without the fact must participate in the returning
   join even though the exception route of this arm is unreachable. *)
let[@warning "-11"] mixed_arm_fact_free_value_path_rejected g =
  let r =
    match g () with
    | exception _ -> ignore (join_law ()); 7
    | x | exception Mixed_return x -> x
  in
  ignore r;
  (join_key : int{ _ = 7 })

[%%expect {|
Line 8, characters 2-27:
8 |   (join_key : int{ _ = 7 })
      ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

(* The independent value route also makes the arm's inherited result mark
   reachable, even after exception fallthrough has been exhausted. *)
let[@warning "-11"] mixed_arm_value_result_rejected g : int{ _ = 7 } =
  match g () with
  | exception _ -> 7
  | x | exception Mixed_return x -> x

[%%expect {|
Line 4, characters 36-37:
4 |   | x | exception Mixed_return x -> x
                                        ^
Error: Refinement verification failed (not-proved)
|}]

(* Isolating control: a pure value arm already participates in the join. *)
let pure_value_arm_fact_free_path_rejected g =
  let r =
    match g () with
    | exception _ -> ignore (join_law ()); 7
    | x -> x
  in
  ignore r;
  (join_key : int{ _ = 7 })

[%%expect {|
Line 8, characters 2-27:
8 |   (join_key : int{ _ = 7 })
      ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

(* Isolating control: when exception fallthrough is still reachable, the
   mixed arm participates through both of its alternatives. *)
let mixed_arm_reachable_exception_control_rejected g =
  let r =
    match g () with
    | exception (Failure _) -> ignore (join_law ()); 7
    | x | exception Mixed_return x -> x
  in
  ignore r;
  (join_key : int{ _ = 7 })

[%%expect {|
Line 8, characters 2-27:
8 |   (join_key : int{ _ = 7 })
      ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

(* A guard observation on the earlier exception route cannot prove a claim
   about an arbitrary input when the mixed arm returns through its value
   route.  This control uses no trusted law. *)
let[@warning "-11"] mixed_arm_guard_observation_rejected n g =
  let r =
    match g () with
    | exception _ when is_zero n -> 7
    | exception _ -> raise Exit
    | x | exception Mixed_return x -> x
  in
  ignore r;
  (n : int{ _ = 0 })

[%%expect {|
Line 9, characters 2-20:
9 |   (n : int{ _ = 0 })
      ^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

(* An irrefutable mixed arm consumes both completion families.  A later
   mixed arm is locally checked but cannot weaken the returning join. *)
let[@warning "-11"] mixed_irrefutable_consumes_value_fallthrough
    (g : unit -> exn) =
  let result =
    match g () with
    | x | exception x -> ignore (join_law ()); x
    | x | exception x -> x
  in
  ignore result;
  (join_key : int{ _ = 7 })

[%%expect {|
val mixed_irrefutable_consumes_value_fallthrough :
  (unit -> exn) -> int{ _ = 7 } = <fun>
|}]

(* Inherited result marks likewise exclude a later unreachable mixed arm. *)
let[@warning "-11"] unreachable_mixed_arm_has_no_result_mark
    (g : unit -> exn) : int{ _ = 7 } =
  match g () with
  | _ | exception _ -> 7
  | _ | exception _ -> 8

[%%expect {|
val unreachable_mixed_arm_has_no_result_mark : (unit -> exn) -> int{ _ = 7 } =
  <fun>
|}]

(* Unreachable mixed arms are still walked for their own local obligations. *)
let[@warning "-11"] unreachable_mixed_arm_local_obligation_rejected
    (g : unit -> exn) =
  match g () with
  | x | exception x -> x
  | x | exception x -> ignore (0 : int{ false }); x

[%%expect {|
Line 5, characters 30-48:
5 |   | x | exception x -> ignore (0 : int{ false }); x
                                  ^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (disproved)
|}]

(* A refutable mixed value alternative preserves value fallthrough, so the
   fact-free fallback still participates in the returning join. *)
let refutable_mixed_arm_preserves_value_fallthrough (g : unit -> exn) =
  let result =
    match g () with
    | Failure _ | exception Failure _ -> ignore (join_law ()); Exit
    | x | exception x -> x
  in
  ignore result;
  (join_key : int{ _ = 7 })

[%%expect {|
Line 8, characters 2-27:
8 |   (join_key : int{ _ = 7 })
      ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

(* Pure value sequencing is the reference behavior for an irrefutable value
   alternative consuming later normal-value reachability. *)
let[@warning "-11"] pure_irrefutable_consumes_value_fallthrough
    (g : unit -> int) =
  let result =
    match g () with
    | x -> ignore (join_law ()); x
    | x -> x
    | exception _ -> ignore (join_law ()); 0
  in
  ignore result;
  (join_key : int{ _ = 7 })

[%%expect {|
val pure_irrefutable_consumes_value_fallthrough :
  (unit -> int) -> int{ _ = 7 } = <fun>
|}]

(* Consuming the normal-value family must not consume the independent
   exception family when the first exception alternative is refutable. *)
let[@warning "-11"] mixed_value_and_exception_edges_stay_independent
    (g : unit -> exn) =
  let result =
    match g () with
    | _ | exception Failure _ -> ignore (join_law ()); Exit
    | x | exception x -> x
  in
  ignore result;
  (join_key : int{ _ = 7 })

[%%expect {|
Line 9, characters 2-27:
9 |   (join_key : int{ _ = 7 })
      ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

(* An unstable guard cannot be identified with its later syntactic
   observation, even though evaluation order is otherwise the same. *)
let unstable_guard_observation_rejected x =
  match () with
  | () when partial_is_zero ~x -> (x : int{ _ = 0 })
  | () -> 0

[%%expect {|
Line 3, characters 34-52:
3 |   | () when partial_is_zero ~x -> (x : int{ _ = 0 })
                                      ^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

(* A [try] join contains only paths that can actually complete normally. *)
let try_nonreturning_normal_path () =
  let () =
    try raise Exit with
    | Exit -> ignore (join_law ())
  in
  (join_key : int{ _ = 7 })

let try_nested_nonreturning_normal_path () =
  let () =
    try (if raise Exit then () else ()) with
    | Exit -> ignore (join_law ())
  in
  (join_key : int{ _ = 7 })

[%%expect {|
val try_nonreturning_normal_path : unit -> int{ _ = 7 } = <fun>
val try_nested_nonreturning_normal_path : unit -> int{ _ = 7 } = <fun>
|}]

(* A genuinely returning protected path still participates in the join. *)
let try_returning_path_missing_fact_rejected flag =
  let () =
    try (if flag then () else raise Exit) with
    | Exit -> ignore (join_law ())
  in
  (join_key : int{ _ = 7 })

[%%expect {|
Line 6, characters 2-27:
6 |   (join_key : int{ _ = 7 })
      ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

(* A synthetic match result retains a nested nonrecursive let's binder
   context while it is lowered to a logical equality. *)
let match_nested_let_result () : int{ _ = 7 } =
  match (let local = 7 in local) with
  | value -> value

[%%expect {|
val match_nested_let_result : unit -> int{ _ = 7 } = <fun>
|}]

let match_nested_let_wrong_result () : int{ _ = 8 } =
  match (let local = 7 in local) with
  | value -> value

[%%expect {|
Line 3, characters 13-18:
3 |   | value -> value
                 ^^^^^
Error: Refinement verification failed (disproved)
|}]

(* A match-result summary is all-or-nothing.  Dropping an unsupported normal
   result would strengthen the remaining disjunction and be unsound. *)
let unsupported_inner_match_rejected flag : int{ _ = 7 } =
  match (if flag then 7 else (match flag with true -> 8 | false -> 9)) with
  | value -> value

[%%expect {|
Line 3, characters 13-18:
3 |   | value -> value
                 ^^^^^
Error: Refinement verification failed (not-proved)
|}]

let unsupported_inner_try_rejected flag : int{ _ = 7 } =
  match (if flag then 7 else (try 9 with Exit -> 8)) with
  | value -> value

[%%expect {|
Line 3, characters 13-18:
3 |   | value -> value
                 ^^^^^
Error: Refinement verification failed (not-proved)
|}]

(* Abandoning the summary does not reject the program itself or change its
   runtime value; it only withholds the unsupported specific proposition. *)
let unsupported_summary_runtime flag : int{ true } =
  match (if flag then 7 else (match flag with true -> 8 | false -> 9)) with
  | value -> value

let unsupported_summary_runtime_result = unsupported_summary_runtime false

[%%expect {|
val unsupported_summary_runtime : bool -> int{ true } = <fun>
val unsupported_summary_runtime_result : int{ true } = 9
|}]

type _ Effect.t += Summary_give : int Effect.t

let observe_seven (value : int{ _ = 7 }) = value

(* A handled [perform] can reach the value arm when its continuation is
   resumed.  Since the resumed value is not summarized, both domain and
   inline obligations remain fail-closed. *)
let resumed_effect_domain_rejected flag =
  match (if flag then 7 else Effect.perform Summary_give) with
  | value -> observe_seven value
  | effect Summary_give, continuation ->
    ignore (Effect.Deep.continue continuation 5);
    0

[%%expect {|
type _ Stdlib.Effect.t += Summary_give : int Effect.t
val observe_seven : int{ _ = 7 } -> int = <fun>
Line 10, characters 27-32:
10 |   | value -> observe_seven value
                                ^^^^^
Error: Refinement verification failed (not-proved)
|}]

let resumed_effect_annotation_rejected flag =
  match (if flag then 7 else Effect.perform Summary_give) with
  | value -> ignore (value : int{ _ = 7 }); 0
  | effect Summary_give, continuation ->
    ignore (Effect.Deep.continue continuation 5);
    0

[%%expect {|
Line 3, characters 20-42:
3 |   | value -> ignore (value : int{ _ = 7 }); 0
                        ^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

let resumed_effect_runtime flag =
  match (if flag then 7 else Effect.perform Summary_give) with
  | value -> value
  | effect Summary_give, continuation ->
    Effect.Deep.continue continuation 5

let resumed_effect_runtime_result = resumed_effect_runtime false

let discarded_effect_summary flag : int{ _ = 7 } =
  match (if flag then 7 else Effect.perform Summary_give) with
  | value -> value
  | effect Summary_give, _ -> 7

let discarded_effect_runtime_result = discarded_effect_summary false

[%%expect {|
val resumed_effect_runtime : bool -> int = <fun>
val resumed_effect_runtime_result : int = 5
val discarded_effect_summary : bool -> int{ _ = 7 } = <fun>
val discarded_effect_runtime_result : int{ _ = 7 } = 7
|}]

let opaque_try_result () = 9

(* Try-result summaries obey the same all-path rule.  The opaque normal call
   must not be dropped in favor of the representable handler result. *)
let unsupported_try_result_rejected () =
  let result = try opaque_try_result () with Exit -> 8 in
  (result : int{ _ = 8 })

[%%expect {|
val opaque_try_result : unit -> int = <fun>
Line 7, characters 2-25:
7 |   (result : int{ _ = 8 })
      ^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

let unsupported_try_runtime () =
  let result = try opaque_try_result () with Exit -> 8 in
  (result : int{ true })

let unsupported_try_runtime_result = unsupported_try_runtime ()

let representable_try_result () =
  let result = try 8 with Exit -> 8 in
  (result : int{ _ = 8 })

let representable_try_runtime_result = representable_try_result ()

[%%expect {|
val unsupported_try_runtime : unit -> int{ true } = <fun>
val unsupported_try_runtime_result : int{ true } = 9
val representable_try_result : unit -> int{ _ = 8 } = <fun>
val representable_try_runtime_result : int{ _ = 8 } = 8
|}]

(* A try-result summary keeps a normally completing nonrecursive let's binder
   context so the result can be lowered as a logical let. *)
let try_nested_let_result () =
  let result = try (let local = 7 in local) with Exit -> 7 in
  (result : int{ _ = 7 })

[%%expect {|
val try_nested_let_result : unit -> int{ _ = 7 } = <fun>
|}]

let try_nested_let_wrong_result () =
  let result = try (let local = 7 in local) with Exit -> 7 in
  (result : int{ _ = 8 })

[%%expect {|
Line 3, characters 2-25:
3 |   (result : int{ _ = 8 })
      ^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (disproved)
|}]

let (try_total_seven @ total) () : int{ _ = 7 } = 7

let try_nested_total_call_result () =
  let result =
    try (let local = try_total_seven () in local) with Exit -> 7
  in
  (result : int{ _ = 7 })

[%%expect {|
val try_total_seven : unit -> int{ _ = 7 } = <fun>
val try_nested_total_call_result : unit -> int{ _ = 7 } = <fun>
|}]

(* Recursive, mutable, and ordinary-call wrappers stay fail-closed. *)
let try_recursive_let_result_rejected () =
  let result =
    try (let rec identity value = value in identity 7) with Exit -> 7
  in
  (result : int{ _ = 7 })

[%%expect {|
Line 5, characters 2-25:
5 |   (result : int{ _ = 7 })
      ^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

let try_mutable_let_result_rejected () =
  let result = try (let mutable local = 7 in local) with Exit -> 7 in
  (result : int{ _ = 7 })

[%%expect {|
Line 3, characters 2-25:
3 |   (result : int{ _ = 7 })
      ^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

let try_ordinary_call_result_rejected () =
  let result =
    try (let local = opaque_try_result () in local) with Exit -> 7
  in
  (result : int{ _ = 7 })

[%%expect {|
Line 5, characters 2-25:
5 |   (result : int{ _ = 7 })
      ^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

(* Interrupted match arms and try handlers use the same sequential guard
   edges as value arms. *)
let exception_guard_true_observation x =
  match raise Exit with
  | _ -> 0
  | exception Exit when is_zero x -> (x : int{ _ = 0 })
  | exception _ -> 0

let try_guard_true_observation x =
  try raise Exit with
  | Exit when is_zero x -> (x : int{ _ = 0 })
  | _ -> 0

[%%expect {|
val exception_guard_true_observation : int -> int = <fun>
val try_guard_true_observation : int -> int{ _ = 0 } = <fun>
|}]

(* An irrefutable handler's completed guard facts reach both the taken edge and
   the next handler after a false result. *)
let exception_guard_fact_on_every_exit () =
  let () =
    match raise Exit with
    | _ -> ()
    | exception _ when guard_law () -> ()
    | exception _ -> ()
  in
  (guard_key : int{ _ = 7 })

let try_guard_fact_on_every_exit () =
  let () =
    try raise Exit with
    | _ when guard_law () -> ()
    | _ -> ()
  in
  (guard_key : int{ _ = 7 })

[%%expect {|
val exception_guard_fact_on_every_exit : unit -> int{ _ = 7 } = <fun>
val try_guard_fact_on_every_exit : unit -> int{ _ = 7 } = <fun>
|}]

(* A refutable handler pattern can bypass its guard, so later handlers must
   intersect the guard-false path with the original mismatch path. *)
let exception_guard_mismatch_rejected () =
  let () =
    match raise Exit with
    | _ -> ()
    | exception Not_found when guard_law () -> ()
    | exception Exit -> ()
  in
  (guard_key : int{ _ = 7 })

[%%expect {|
Line 8, characters 2-28:
8 |   (guard_key : int{ _ = 7 })
      ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

let try_guard_mismatch_rejected () =
  let () =
    try raise Exit with
    | Not_found when guard_law () -> ()
    | Exit -> ()
  in
  (guard_key : int{ _ = 7 })

[%%expect {|
Line 7, characters 2-28:
7 |   (guard_key : int{ _ = 7 })
      ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

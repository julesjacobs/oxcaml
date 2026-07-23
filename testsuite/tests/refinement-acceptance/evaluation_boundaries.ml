(* TEST
 flags += "-extension comprehensions";
 expect;
*)

module Make_fact () : sig
  val p : bool
  val law : unit{ p = true }
end = struct
  let p = true
  let law = (() : unit{ p = true })
end
[%%expect {|
module Make_fact :
  functor () -> sig val p : bool val law : unit{ p = true } end
|}]

module Sequence_fact = Make_fact ()
module Sequence_other = Make_fact ()
[%%expect {|
module Sequence_fact : sig val p : bool val law : unit{ p = true } end
module Sequence_other : sig val p : bool val law : unit{ p = true } end
|}]

let sequence_result_uses_right () : unit{ Sequence_fact.p = true } =
  Sequence_fact.law;
  ()
[%%expect {|
val sequence_result_uses_right : unit -> unit{ Sequence_fact.p = true } =
  <fun>
|}]

let sequence_multiple_facts ()
    : unit{
        Sequence_fact.p = true
        && Sequence_other.p = true
      } =
  Sequence_fact.law;
  Sequence_other.law;
  ()
[%%expect {|
val sequence_multiple_facts :
  unit -> unit{ Sequence_fact.p = true && Sequence_other.p = true } = <fun>
|}]

let sequence_nested_result ()
    : unit{
        Sequence_fact.p = true
        && Sequence_other.p = true
      } =
  Sequence_fact.law;
  (Sequence_other.law; ())
[%%expect {|
val sequence_nested_result :
  unit -> unit{ Sequence_fact.p = true && Sequence_other.p = true } = <fun>
|}]

external called_sequence_law : unit -> unit{ Sequence_fact.p = true }
  @@ total = "%identity"

let sequence_called_proof_result_uses_right ()
    : unit{ Sequence_fact.p = true } =
  called_sequence_law ();
  ()
[%%expect {|
external called_sequence_law : unit -> unit{ Sequence_fact.p = true }
  = "%identity"
val sequence_called_proof_result_uses_right :
  unit -> unit{ Sequence_fact.p = true } = <fun>
|}]

let sequence_imperative_call_keeps_prior_fact ()
    : unit{ Sequence_fact.p = true } =
  called_sequence_law ();
  print_int 0;
  ()
[%%expect {|
val sequence_imperative_call_keeps_prior_fact :
  unit -> unit{ Sequence_fact.p = true } = <fun>
|}]

external partial_sequence_law : unit -> unit{ Sequence_fact.p = true }
  = "%identity"

let sequence_partial_call_establishes_fact_on_return ()
    : unit{ Sequence_fact.p = true } =
  partial_sequence_law ();
  ()
[%%expect {|
external partial_sequence_law : unit -> unit{ Sequence_fact.p = true }
  = "%identity"
val sequence_partial_call_establishes_fact_on_return :
  unit -> unit{ Sequence_fact.p = true } = <fun>
|}]

let requires_sequence_fact (_ : unit{ Sequence_fact.p = true }) = ()

let sequence_nested_in_refined_argument () =
  requires_sequence_fact
    (called_sequence_law ();
     print_int 0;
     ())
[%%expect {|
val requires_sequence_fact : unit{ Sequence_fact.p = true } -> unit = <fun>
val sequence_nested_in_refined_argument : unit -> unit = <fun>
|}]

let sequence_partial_call_in_refined_argument () =
  requires_sequence_fact
    (partial_sequence_law ();
     ())
[%%expect {|
val sequence_partial_call_in_refined_argument : unit -> unit = <fun>
|}]

let sequence_nested_let_argument () =
  requires_sequence_fact
    (let value = 0 in
     called_sequence_law ();
     print_int value;
     ())

let sequence_nested_open_argument () =
  requires_sequence_fact
    (let open Stdlib in
     called_sequence_law ();
     ())
[%%expect {|
val sequence_nested_let_argument : unit -> unit = <fun>
val sequence_nested_open_argument : unit -> unit = <fun>
|}]

let requires_false_sequence (_ : unit{ false }) = ()

let sequence_false_refined_argument_rejected () =
  requires_false_sequence
    (called_sequence_law ();
     ())
[%%expect {|
val requires_false_sequence : unit{ false } -> unit = <fun>
Line 6, characters 5-7:
6 |      ())
         ^^
Error: Refinement verification failed (disproved)
|}]

let sequence_false_nested_let_argument_rejected () =
  requires_false_sequence
    (let value = 0 in
     called_sequence_law ();
     print_int value;
     ())
[%%expect {|
Line 6, characters 5-7:
6 |      ())
         ^^
Error: Refinement verification failed (disproved)
|}]

let[@warning "-21"] sequence_nonreturning_refined_argument () =
  requires_false_sequence
    (raise Exit;
     ())
[%%expect {|
val sequence_nonreturning_refined_argument : unit -> unit = <fun>
|}]

let sequence_checks_left_local_obligation () : unit{ true } =
  (() : unit{ false });
  ()
[%%expect {|
Line 2, characters 2-22:
2 |   (() : unit{ false });
      ^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (disproved)
|}]

let sequence_refined_nonunit_still_warns () =
  (0 : int{ _ = 0 });
  ()
[%%expect {|
Line 2, characters 2-20:
2 |   (0 : int{ _ = 0 });
      ^^^^^^^^^^^^^^^^^^
Warning 10 [non-unit-statement]: this expression should have type unit.

val sequence_refined_nonunit_still_warns : unit -> unit = <fun>
|}]

let sequence_checks_right_as_result () : int{ _ = 7 } =
  Sequence_fact.law;
  8
[%%expect {|
Line 3, characters 2-3:
3 |   8
      ^
Error: Refinement verification failed (disproved)
|}]

let sequence_false_result_rejected () : unit{ false } =
  called_sequence_law ();
  ()
[%%expect {|
Line 3, characters 2-4:
3 |   ()
      ^^
Error: Refinement verification failed (disproved)
|}]

let[@warning "-21"] sequence_nonreturning_prefix () : int{ false } =
  raise Exit;
  0
[%%expect {|
val sequence_nonreturning_prefix : unit -> int{ false } = <fun>
|}]

let sequence_result_distributes_over_if flag : int{ _ >= 0 } =
  Sequence_fact.law;
  if flag then 0 else 1
[%%expect {|
val sequence_result_distributes_over_if : bool -> int{ _ >= 0 } = <fun>
|}]

let sequence_result_checks_each_if_arm flag : int{ _ >= 0 } =
  Sequence_fact.law;
  if flag then 0 else -1
[%%expect {|
Line 3, characters 22-24:
3 |   if flag then 0 else -1
                          ^^
Error: Refinement verification failed (disproved)
|}]

let if_without_else_checks_implicit_unit (flag : bool)
    : unit{ flag = true } =
  if flag then ()
[%%expect {|
Line 3, characters 2-17:
3 |   if flag then ()
      ^^^^^^^^^^^^^^^
Error: Refinement verification failed (disproved)
|}]

let if_without_else_only_false_returns (flag : bool)
    : unit{ flag = false } =
  if flag then raise Exit
[%%expect {|
val if_without_else_only_false_returns :
  (flag : bool) -> unit{ flag = false } = <fun>
|}]

let mutable_result_checks_body_leaf () : int{ _ = 7 } =
  let mutable value = 0 in
  value <- 8;
  8
[%%expect {|
Line 4, characters 2-3:
4 |   8
      ^
Error: Refinement verification failed (disproved)
|}]

let[@warning "-21-26"] mutable_nonreturning_initializer () : int{ false } =
  let mutable value = raise Exit in
  value <- 8;
  8
[%%expect {|
val mutable_nonreturning_initializer : unit -> int{ false } = <fun>
|}]

let open_result_checks_body_leaf () : int{ _ = 7 } =
  let open Stdlib in
  8
[%%expect {|
Line 3, characters 2-3:
3 |   8
      ^
Error: Refinement verification failed (disproved)
|}]

let local_exception_result_checks_body_leaf () : int{ _ = 7 } =
  let exception Local in
  8
[%%expect {|
Line 3, characters 2-3:
3 |   8
      ^
Error: Refinement verification failed (disproved)
|}]

let local_module_result_checks_body_leaf () : int{ _ = 7 } =
  let module Local = struct end in
  8
[%%expect {|
Line 3, characters 2-3:
3 |   8
      ^
Error: Refinement verification failed (disproved)
|}]

let nonempty_local_module_checks_body_leaf () : int{ _ = 7 } =
  let module Local = struct let value = 1 end in
  8
[%%expect {|
Line 3, characters 2-3:
3 |   8
      ^
Error: Refinement verification failed (disproved)
|}]

module type Empty_module = sig end
(* CR vox: [-principal] should preserve the ordinary compact rendering here. *)
[%%expect {|
module type Empty_module = sig end
|}]

let[@warning "-21"] local_module_nonreturning_initializer () : int{ false } =
  let module Local = (val (raise Exit) : Empty_module) in
  8
[%%expect {|
val local_module_nonreturning_initializer : unit -> int{ false } = <fun>
|}]

let simultaneous_no_left_to_right () =
  let module F = Make_fact () in
  let _left = F.law
  and _right = (() : unit{ F.p = true }) in
  ()
[%%expect {|
Line 4, characters 15-40:
4 |   and _right = (() : unit{ F.p = true }) in
                   ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

let simultaneous_no_right_to_left () =
  let module F = Make_fact () in
  let _left = (() : unit{ F.p = true })
  and _right = F.law in
  ()
[%%expect {|
Line 3, characters 14-39:
3 |   let _left = (() : unit{ F.p = true })
                  ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

let simultaneous_body_sees_all () =
  let module P = Make_fact () in
  let module Q = Make_fact () in
  let _left = P.law and _right = Q.law in
  ignore (() : unit{ P.p = true && Q.p = true })
[%%expect {|
val simultaneous_body_sees_all : unit -> unit = <fun>
|}]

let simultaneous_mutation_isolated () =
  let module F = Make_fact () in
  let cell = ref false in
  let _left = F.law
  and _right = (cell := true; (() : unit{ F.p = true })) in
  ()
[%%expect {|
Line 5, characters 30-55:
5 |   and _right = (cell := true; (() : unit{ F.p = true })) in
                                  ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

let simultaneous_nonreturning_isolated () =
  let _left = raise Exit
  and _right = (() : unit{ false }) in
  ()
[%%expect {|
Line 3, characters 15-35:
3 |   and _right = (() : unit{ false }) in
                   ^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (disproved)
|}]

let simultaneous_nonreturning_skips_outer_result_mark () =
  let (_ : unit{ false }) =
    let _left = raise Exit
    and _right = () in
    ()
  in
  ()
[%%expect {|
val simultaneous_nonreturning_skips_outer_result_mark : unit -> unit = <fun>
|}]

let simultaneous_try_isolated flag =
  let module F = Make_fact () in
  let _left =
    try
      if flag then raise Exit;
      ignore F.law
    with Exit -> ignore F.law
  and _right = (() : unit{ F.p = true }) in
  ()
[%%expect {|
Line 8, characters 15-40:
8 |   and _right = (() : unit{ F.p = true }) in
                   ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

let simultaneous_try_merges_for_body flag =
  let module F = Make_fact () in
  let _left =
    try
      if flag then raise Exit;
      ignore F.law
    with Exit -> ignore F.law
  and _right = () in
  ignore (() : unit{ F.p = true })
[%%expect {|
val simultaneous_try_merges_for_body : bool -> unit = <fun>
|}]

let for_bounds_no_left_to_right () =
  let module F = Make_fact () in
  for _index = (ignore F.law; 0)
    to (ignore (() : unit{ F.p = true }); 0)
  do () done
[%%expect {|
Line 4, characters 15-40:
4 |     to (ignore (() : unit{ F.p = true }); 0)
                   ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

let for_bounds_no_right_to_left () =
  let module F = Make_fact () in
  for _index = (ignore (() : unit{ F.p = true }); 0)
    to (ignore F.law; 0)
  do () done
[%%expect {|
Line 3, characters 23-48:
3 |   for _index = (ignore (() : unit{ F.p = true }); 0)
                           ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

let for_body_sees_bounds () =
  let module P = Make_fact () in
  let module Q = Make_fact () in
  for _index = (ignore P.law; 0) to (ignore Q.law; 0) do
    ignore (() : unit{ P.p = true && Q.p = true })
  done
[%%expect {|
val for_body_sees_bounds : unit -> unit = <fun>
|}]

let for_exit_keeps_bound_fact () =
  let module F = Make_fact () in
  for _index = (ignore F.law; 1) to 0 do () done;
  ignore (() : unit{ F.p = true })
[%%expect {|
val for_exit_keeps_bound_fact : unit -> unit = <fun>
|}]

let for_zero_iterations_drop_body_fact () =
  let module F = Make_fact () in
  for _index = 1 to 0 do ignore F.law done;
  ignore (() : unit{ F.p = true })
[%%expect {|
Line 4, characters 9-34:
4 |   ignore (() : unit{ F.p = true })
             ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

let while_body_sees_condition () =
  let module F = Make_fact () in
  while (ignore F.law; false) do
    ignore (() : unit{ F.p = true })
  done
[%%expect {|
val while_body_sees_condition : unit -> unit = <fun>
|}]

let while_body_sees_taken_condition (flag : bool) =
  while flag do
    ignore (() : unit{ flag = true })
  done
[%%expect {|
val while_body_sees_taken_condition : bool -> unit = <fun>
|}]

let while_zero_iterations_drop_body_fact () =
  let module F = Make_fact () in
  while false do ignore F.law done;
  ignore (() : unit{ F.p = true })
[%%expect {|
Line 4, characters 9-34:
4 |   ignore (() : unit{ F.p = true })
             ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

let while_body_does_not_prove_next_condition () =
  let module F = Make_fact () in
  while (ignore (() : unit{ F.p = true }); false) do
    ignore F.law
  done
[%%expect {|
Line 3, characters 16-41:
3 |   while (ignore (() : unit{ F.p = true }); false) do
                    ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

let comprehension_endpoints_are_isolated () =
  let module F = Make_fact () in
  [ _index
    for _index = (ignore F.law; 0)
    to (ignore (() : unit{ F.p = true }); 0) ]
[%%expect {|
Line 5, characters 15-40:
5 |     to (ignore (() : unit{ F.p = true }); 0) ]
                   ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

let comprehension_group_is_isolated () =
  let module F = Make_fact () in
  [ left, right
    for left = (ignore F.law; 0) to 0
    and right = 0 to (ignore (() : unit{ F.p = true }); 0) ]
[%%expect {|
Line 5, characters 29-54:
5 |     and right = 0 to (ignore (() : unit{ F.p = true }); 0) ]
                                 ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

let comprehension_bodies_see_sources () =
  let module F = Make_fact () in
  let list =
    [ (() : unit{ F.p = true })
      for _index = (ignore F.law; 0) to 0 ]
  in
  let array =
    [| (() : unit{ F.p = true })
       for _index = (ignore F.law; 0) to 0 |]
  in
  let iarray =
    [: (() : unit{ F.p = true })
       for _index = (ignore F.law; 0) to 0 :]
  in
  ignore (list, array, iarray)
[%%expect {|
val comprehension_bodies_see_sources : unit -> unit = <fun>
|}]

let comprehension_nested_clause_flow () =
  let module F = Make_fact () in
  [ right
    for _left = (ignore F.law; 0) to 0
    for right = (ignore (() : unit{ F.p = true }); 0) to 0 ]
[%%expect {|
val comprehension_nested_clause_flow : unit -> int list = <fun>
|}]

let comprehension_when_flows_to_body () =
  let module F = Make_fact () in
  let _values =
    [ (() : unit{ F.p = true })
      for _index = 0 to 0
      when (ignore F.law; true) ]
  in
  ()
[%%expect {|
val comprehension_when_flows_to_body : unit -> unit = <fun>
|}]

let comprehension_zero_iterations_drop_body_fact () =
  let module F = Make_fact () in
  let _values = [ignore F.law for _index = 1 to 0] in
  ignore (() : unit{ F.p = true })
[%%expect {|
Line 4, characters 9-34:
4 |   ignore (() : unit{ F.p = true })
             ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

let comprehension_zero_iterations_drop_when_fact () =
  let module F = Make_fact () in
  let _values = [() for _index = 1 to 0 when (ignore F.law; true)] in
  ignore (() : unit{ F.p = true })
[%%expect {|
Line 4, characters 9-34:
4 |   ignore (() : unit{ F.p = true })
             ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

let optional_default_does_not_prove_body
    ?(value =
      let module F = Make_fact () in
      ignore F.law;
      0)
    () =
  ignore value;
  let module F = Make_fact () in
  ignore (() : unit{ F.p = true })
[%%expect {|
Line 9, characters 9-34:
9 |   ignore (() : unit{ F.p = true })
             ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

module Optional_fact = Make_fact ()
[%%expect {|
module Optional_fact : sig val p : bool val law : unit{ p = true } end
|}]

let earlier_default_does_not_prove_later_default
    ?(left = (ignore Optional_fact.law; 0))
    ?(right = (ignore (() : unit{ Optional_fact.p = true }); 0))
    () =
  ignore (left, right)
[%%expect {|
Line 3, characters 22-59:
3 |     ?(right = (ignore (() : unit{ Optional_fact.p = true }); 0))
                          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

let optional_parameter_contract
    ?(value : int{ _ = 7 } = 7)
    () =
  ignore (value : int{ _ = 7 })
[%%expect {|
val optional_parameter_contract : ?value:int{ _ = 7 } -> unit -> unit = <fun>
|}]

let optional_default_checks_own_ordered_evidence
    ?(value =
      let module F = Make_fact () in
      ignore F.law;
      ignore (() : unit{ F.p = true });
      0)
    () =
  ignore value
[%%expect {|
val optional_default_checks_own_ordered_evidence : ?value:int -> unit -> unit =
  <fun>
|}]

let lazy_body_fact_does_not_escape () =
  let module F = Make_fact () in
  ignore (lazy (ignore F.law));
  ignore (() : unit{ F.p = true })
[%%expect {|
Line 4, characters 9-34:
4 |   ignore (() : unit{ F.p = true })
             ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

let lazy_body_obligations_are_checked () =
  lazy (() : unit{ false })
[%%expect {|
Line 2, characters 7-27:
2 |   lazy (() : unit{ false })
           ^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (disproved)
|}]

let lazy_body_sees_captured_fact () =
  let module F = Make_fact () in
  ignore F.law;
  let _value = lazy (() : unit{ F.p = true }) in
  ()
[%%expect {|
val lazy_body_sees_captured_fact : unit -> unit = <fun>
|}]

let lazy_body_uses_own_ordered_evidence () =
  let module F = Make_fact () in
  let _value = lazy (ignore F.law; (() : unit{ F.p = true })) in
  ()
[%%expect {|
val lazy_body_uses_own_ordered_evidence : unit -> unit = <fun>
|}]

let lazy_mutation_is_delayed () =
  let module F = Make_fact () in
  let cell = ref false in
  ignore (lazy (cell := true; ignore F.law));
  ignore (() : unit{ F.p = true })
[%%expect {|
Line 5, characters 9-34:
5 |   ignore (() : unit{ F.p = true })
             ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

let probe_handler_fact_does_not_escape () =
  let module F = Make_fact () in
  [%probe "evaluation_boundary" (ignore F.law)];
  ignore (() : unit{ F.p = true })
[%%expect {|
Line 4, characters 9-34:
4 |   ignore (() : unit{ F.p = true })
             ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

let assert_condition_postfact_escapes_when_enabled () =
  let module F = Make_fact () in
  assert (ignore F.law; true);
  ignore (() : unit{ F.p = true })
[%%expect {|
val assert_condition_postfact_escapes_when_enabled : unit -> unit = <fun>
|}]

let assert_true_condition_fact (flag : bool) =
  assert flag;
  ignore (() : unit{ flag = true })
[%%expect {|
val assert_true_condition_fact : bool -> unit = <fun>
|}]

let if_branch_assert_fact_does_not_escape (flag : bool) (value : int) =
  (if flag then assert (value > 0));
  ignore (value : int{ _ > 0 })
[%%expect {|
Line 3, characters 9-31:
3 |   ignore (value : int{ _ > 0 })
             ^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

let match_arm_assert_fact_does_not_escape (flag : bool) (value : int) =
  (match flag with
   | true -> assert (value > 0)
   | false -> ());
  ignore (value : int{ _ > 0 })
[%%expect {|
Line 5, characters 9-31:
5 |   ignore (value : int{ _ > 0 })
             ^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

let try_handler_assert_fact_does_not_escape (flag : bool) (value : int) =
  (try if flag then raise Exit with Exit -> assert (value > 0));
  ignore (value : int{ _ > 0 })
[%%expect {|
Line 3, characters 9-31:
3 |   ignore (value : int{ _ > 0 })
             ^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

let tuple_siblings_do_not_share_assert_facts (value : int) =
  ignore (assert (value > 0), (value : int{ _ > 0 }))
[%%expect {|
Line 2, characters 30-52:
2 |   ignore (assert (value > 0), (value : int{ _ > 0 }))
                                  ^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

let discard_two (_left : unit) (_right : int) = ()
[%%expect {|
val discard_two : unit -> int -> unit = <fun>
|}]

let application_arguments_do_not_share_assert_facts (value : int) =
  discard_two (assert (value > 0)) (value : int{ _ > 0 })
[%%expect {|
Line 2, characters 35-57:
2 |   discard_two (assert (value > 0)) (value : int{ _ > 0 })
                                       ^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

let ( let* ) (value : int) (_body : int -> int) = value
[%%expect {|
val ( let* ) : int -> (int -> int) -> int = <fun>
|}]

let letop_body_assert_fact_does_not_escape (value : int) =
  let _result =
    let* bound = value in
    assert (bound > 0);
    bound
  in
  ignore (value : int{ _ > 0 })
[%%expect {|
Line 7, characters 9-31:
7 |   ignore (value : int{ _ > 0 })
             ^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

let letop_body_fact_does_not_escape () =
  let module F = Make_fact () in
  let _result =
    let* value = 1 in
    ignore F.law;
    value
  in
  ignore (() : unit{ F.p = true })
[%%expect {|
Line 8, characters 9-34:
8 |   ignore (() : unit{ F.p = true })
             ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

module Class_fact = Make_fact ()
[%%expect {|
module Class_fact : sig val p : bool val law : unit{ p = true } end
|}]

class delayed_class_initializer = object
  val _value = (ignore Class_fact.law; 0)
end
[%%expect {|
class delayed_class_initializer : object val _value : int end
|}]

let class_initializer_fact_does_not_escape_declaration =
  ignore (() : unit{ Class_fact.p = true })
[%%expect {|
Line 2, characters 9-43:
2 |   ignore (() : unit{ Class_fact.p = true })
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

module Object_fact = Make_fact ()
[%%expect {|
module Object_fact : sig val p : bool val law : unit{ p = true } end
|}]

let object_initializer_fact_is_immediate () =
  let _object = object
    val _value = (ignore Object_fact.law; 0)
  end
  in
  ignore (() : unit{ Object_fact.p = true })
[%%expect {|
val object_initializer_fact_is_immediate : unit -> unit = <fun>
|}]

module Structure_sibling_fact = Make_fact ()
[%%expect {|
module Structure_sibling_fact :
  sig val p : bool val law : unit{ p = true } end
|}]

let _structure_left = Structure_sibling_fact.law
and _structure_right = (() : unit{ Structure_sibling_fact.p = true })
[%%expect {|
Line 2, characters 23-69:
2 | and _structure_right = (() : unit{ Structure_sibling_fact.p = true })
                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

module Structure_left_fact = Make_fact ()
module Structure_right_fact = Make_fact ()
[%%expect {|
module Structure_left_fact : sig val p : bool val law : unit{ p = true } end
module Structure_right_fact : sig val p : bool val law : unit{ p = true } end
|}]

let _structure_left = Structure_left_fact.law
and _structure_right = Structure_right_fact.law
[%%expect {|
val _structure_left : unit = ()
val _structure_right : unit = ()
|}]

let structure_body_sees_all_returning_rhs_facts =
  ignore
    (() : unit{
      Structure_left_fact.p = true && Structure_right_fact.p = true
    })
[%%expect {|
val structure_body_sees_all_returning_rhs_facts : unit = ()
|}]

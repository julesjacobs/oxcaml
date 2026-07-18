(* TEST
 flags += "-keywords 5.3";
 expect;
*)

type match_box =
  | Match_box of int
  | Match_empty

(* The current constructor equation composes with the scrutinee refinement;
   constructor injectivity proves the payload refinement. *)
let match_positive (value : match_box{ _ = Match_box 3 }) =
  match value with
  | Match_box payload -> (payload : int{ _ = 3 })
  | Match_empty -> (0 : int{ _ = 3 })
[%%expect {|
type match_box = Match_box of int | Match_empty
val match_positive : match_box{ _ = Match_box 3 } -> int{ _ = 3 } = <fun>
|}]

type match_record =
  { first : int;
    second : int;
  }

(* A record component is tied to the immutable projection of the refined
   scrutinee, so its refinement is available in the arm. *)
let match_projection
    (value : match_record{ _.first > 0 }) =
  match value with
  | { first; _ } -> (first : int{ _ > 0 })
[%%expect {|
type match_record = { first : int; second : int; }
val match_projection : match_record{ _.first > 0 } -> int{ _ > 0 } = <fun>
|}]

type match_choice =
  | Match_a of int
  | Match_b
  | Match_c

(* The fallback sees the failures of both earlier guard-free arms. *)
let match_negative value =
  match value with
  | Match_a _ -> Match_c
  | Match_b -> Match_c
  | _ -> (value : match_choice{ _ = Match_c })
[%%expect {|
type match_choice = Match_a of int | Match_b | Match_c
val match_negative : match_choice -> match_choice = <fun>
|}]

(* A guarded arm must not contribute a negative fact: its constructor may
   have matched even though the guard was false. *)
let match_guarded_negative value guard =
  match value with
  | Match_a _ when guard -> Match_c
  | Match_b -> Match_c
  | _ -> (value : match_choice{ _ = Match_c })
[%%expect {|
Line 5, characters 9-46:
5 |   | _ -> (value : match_choice{ _ = Match_c })
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

(* Failure of a deep ground pattern excludes only that exact value, not the
   constructor head. *)
let match_deep_negative value =
  match value with
  | Match_a 0 -> Match_c
  | Match_b -> Match_c
  | _ -> (value : match_choice{ _ = Match_c })
[%%expect {|
Line 5, characters 9-46:
5 |   | _ -> (value : match_choice{ _ = Match_c })
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

(* An or-pattern does not justify excluding either constructor separately. *)
let match_or_negative value =
  match value with
  | Match_a _ | Match_b -> Match_c
  | rest -> (rest : match_choice{ _ = Match_c })
[%%expect {|
Line 4, characters 12-48:
4 |   | rest -> (rest : match_choice{ _ = Match_c })
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

type match_wrapped = Match_wrapped of (int * int)

let match_nested_tuple (value : match_wrapped{ _ = Match_wrapped (3, 4) }) =
  match value with
  | Match_wrapped (first, second) ->
    (first + second : int{ _ = 7 })
[%%expect {|
type match_wrapped = Match_wrapped of (int * int)
val match_nested_tuple :
  match_wrapped{ _ = Match_wrapped (3, 4) } -> int{ _ = 7 } = <fun>
|}]

let match_next () = read_int ()
[%%expect {|
val match_next : unit -> int = <fun>
|}]

(* An impure scrutinee is named once; an arm fact about that stable name must
   not be mistaken for a fact about a second evaluation. *)
let match_impure_scrutinee_scope () =
  match match_next () with
  | 0 ->
    let _ = (match_next () : int{ _ = 0 }) in
    0
  | _ -> 0
[%%expect {|
Line 4, characters 12-42:
4 |     let _ = (match_next () : int{ _ = 0 }) in
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

(* A false postcondition is useful here because it can prove an obligation
   only on a path where the call has returned normally. *)
external match_raise_false : exn -> int{ false } = "%raise"

let match_fact_source () : int{ false } = match_raise_false Exit
[%%expect {|
external match_raise_false : exn -> int{ false } = "%raise"
val match_fact_source : unit -> int{ false } = <fun>
|}]

(* Facts learned in one mutually exclusive value arm do not enter another
   arm or the join. *)
let match_value_arm_scope flag =
  match flag with
  | true ->
    ignore (match_fact_source ());
    0
  | false -> (0 : int{ false })
[%%expect {|
Line 6, characters 13-31:
6 |   | false -> (0 : int{ false })
                 ^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (disproved)
|}]

let match_value_post_scope flag =
  let _ =
    match flag with
    | true -> ignore (match_fact_source ())
    | false -> ()
  in
  (0 : int{ false })
[%%expect {|
Line 7, characters 2-20:
7 |   (0 : int{ false })
      ^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (disproved)
|}]

(* An exception arm is reachable without normal completion of the scrutinee,
   so it must not see the scrutinee call's false postcondition. *)
let match_exception_handler_scope () =
  match ignore (match_fact_source ()) with
  | () -> ()
  | exception _ ->
    let _ = (0 : int{ false }) in
    ()
[%%expect {|
Line 5, characters 12-30:
5 |     let _ = (0 : int{ false }) in
                ^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (disproved)
|}]

let match_exception_handler_local_scope () =
  match raise Exit with
  | () -> ()
  | exception _ ->
    ignore (match_fact_source ());
    let _ = (0 : int{ false }) in
    ()
[%%expect {|
val match_exception_handler_local_scope : unit -> unit = <fun>
|}]

(* A returning exception arm also gives the match join a path on which the
   scrutinee call never returned. *)
let match_exception_post_scope () =
  let () =
    match ignore (match_fact_source ()) with
    | () -> ()
    | exception _ -> ()
  in
  (0 : int{ false })
[%%expect {|
Line 7, characters 2-20:
7 |   (0 : int{ false })
      ^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (disproved)
|}]

type _ Effect.t += Match_scope_effect : unit Effect.t

let match_after_effect (_ : unit) : int{ false } = match_raise_false Exit
let match_effect_source () : int{ false } =
  match_after_effect (Effect.perform Match_scope_effect)
[%%expect {|
type _ Stdlib.Effect.t += Match_scope_effect : unit Effect.t
val match_after_effect : unit -> int{ false } = <fun>
val match_effect_source : unit -> int{ false } = <fun>
|}]

(* Effect handlers have the same interrupted entry boundary as exception
   handlers. *)
let match_effect_handler_scope () =
  match ignore (match_effect_source ()) with
  | () -> ()
  | effect Match_scope_effect, _ ->
    let _ = (0 : int{ false }) in
    ()
[%%expect {|
Line 5, characters 12-30:
5 |     let _ = (0 : int{ false }) in
                ^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (disproved)
|}]

let match_effect_handler_local_scope () =
  match Effect.perform Match_scope_effect with
  | () -> ()
  | effect Match_scope_effect, _ ->
    ignore (match_fact_source ());
    let _ = (0 : int{ false }) in
    ()
[%%expect {|
val match_effect_handler_local_scope : unit -> unit = <fun>
|}]

let match_effect_post_scope () =
  let () =
    match ignore (match_effect_source ()) with
    | () -> ()
    | effect Match_scope_effect, _ -> ()
  in
  (0 : int{ false })
[%%expect {|
Line 7, characters 2-20:
7 |   (0 : int{ false })
      ^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (disproved)
|}]

(* With only ordinary value arms, every path reaching the join has completed
   the scrutinee, so its normal-return fact remains available. *)
let match_normal_post_scope () =
  let _ =
    match ignore (match_fact_source ()) with
    | () -> ()
  in
  (0 : int{ false })
[%%expect {|
val match_normal_post_scope : unit -> int{ false } = <fun>
|}]

(* Resetting an interrupted arm to its entry state must preserve facts that
   were already established before evaluation of the scrutinee. *)
let match_preexisting_scope () =
  ignore (match_fact_source ());
  let () =
    match raise Exit with
    | () -> ()
    | exception _ -> ()
  in
  (0 : int{ false })
[%%expect {|
val match_preexisting_scope : unit -> int{ false } = <fun>
|}]

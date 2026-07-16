(* TEST include stdlib_stable; flags = "-w -220"; expect;
*)

(* SOUNDNESS CAMPAIGN — Family 1 (totality laundering via late inference).

   The hand-written total-context restrictions (ref/while/for/raise/lazy/ mutable
   construction) are enforced by [reject_in_total_context], which fires only when the
   enclosing closure's totality is ALREADY pinned to [total] at the moment its body is
   typed (a "snapshot" via [check_const_conservative], which returns [None] while totality
   is still an unsolved inference variable).

   The attack: keep the closure's totality unpinned while its body — containing a partial
   operation — is typed, then force it to [total] afterwards. If the snapshot skipped the
   restriction, we obtain a [total] closure performing a partial operation. Each of these
   MUST be rejected. Any that is ACCEPTED is a soundness finding. *)

let expects_total (f @ total) = f

(* A1: ref allocation laundered into total by later use. *)
let bad_ref () = ref 0
let escaped_ref = expects_total bad_ref

[%%expect
  {|
val expects_total : 'a @ total -> 'a = <fun>
val bad_ref : unit -> int ref = <fun>
Line 5, characters 32-39:
5 | let escaped_ref = expects_total bad_ref
                                    ^^^^^^^
Error: This value is "partial" but is expected to be "total".
|}]

(* A2: while-loop divergence laundered into total by later use. *)
let bad_loop () =
  while true do
    ()
  done
;;

let escaped_loop = expects_total bad_loop

[%%expect
  {|
val bad_loop : unit -> 'a = <fun>
Line 7, characters 33-41:
7 | let escaped_loop = expects_total bad_loop
                                     ^^^^^^^^
Error: This value is "partial" but is expected to be "total".
|}]

(* A3: exception raise laundered into total by later use. *)
let bad_raise () = raise Exit
let escaped_raise = expects_total bad_raise

[%%expect
  {|
val bad_raise : unit -> 'a = <fun>
Line 2, characters 34-43:
2 | let escaped_raise = expects_total bad_raise
                                      ^^^^^^^^^
Error: This value is "partial" but is expected to be "total".
|}]

(* A4: lazy laundered into total by later use. *)
let bad_lazy () = lazy 0
let escaped_lazy = expects_total bad_lazy

[%%expect
  {|
val bad_lazy : unit -> int lazy_t = <fun>
Line 2, characters 33-41:
2 | let escaped_lazy = expects_total bad_lazy
                                     ^^^^^^^^
Error: This value is "partial" but is expected to be "total".
|}]

(* A5: mutable-record construction laundered into total by later use. *)
type r = { mutable m : int }

let bad_mkmut x = { m = x }
let escaped_mkmut = expects_total bad_mkmut

[%%expect
  {|
type r = { mutable m : int; }
val bad_mkmut : int -> r = <fun>
Line 4, characters 34-43:
4 | let escaped_mkmut = expects_total bad_mkmut
                                      ^^^^^^^^^
Error: This value is "partial" but is expected to be "total".
|}]

(* A6: force to total via a plain type/mode annotation on a later binding instead of a
   function call. *)
let bad_ref2 () = ref 0
let escaped_ref2 : (unit -> int ref) @ total = bad_ref2

[%%expect
  {|
val bad_ref2 : unit -> int ref = <fun>
Line 2, characters 47-55:
2 | let escaped_ref2 : (unit -> int ref) @ total = bad_ref2
                                                   ^^^^^^^^
Error: This value is "partial" but is expected to be "total".
|}]

(* A7: launder through a tuple, then extract and force to total. *)
let pair = (fun () -> ref 0), 0
let escaped_pair = expects_total (fst pair)

[%%expect
  {|
val pair : (unit -> int ref) * int = (<fun>, 0)
Line 2, characters 33-43:
2 | let escaped_pair = expects_total (fst pair)
                                     ^^^^^^^^^^
Error: This value is "partial" but is expected to be "total".
|}]

(* A8: THE ROBUST FORM. A local (let-in) binding is not zapped to legacy at a structure
   boundary, so its totality stays an unpinned inference variable while the body [ref 0]
   is typed (snapshot sees "not total" -> restriction skipped), then [expects_total]
   forces it to total. If A1's top-level form is repelled only by legacy-zapping rather
   than by a real constraint from the [ref] op, this let-in form exposes the snapshot
   hole. *)
let escaped_local_ref =
  let bad () = ref 0 in
  expects_total bad
;;

[%%expect
  {|
Line 3, characters 16-19:
3 |   expects_total bad
                    ^^^
Error: This value is "partial"
         because it closes over the value "ref" at line 2, characters 15-18
         which is "partial".
       However, the highlighted expression is expected to be "total".
|}]

(* A9 (the leaking while-loop case) has moved to late_inference_construct_matrix.ml, the
   dedicated OPEN-FINDING file. This file records only the cases that are correctly
   REPELLED. *)

(* A10: let-in raise -> total value that raises. *)
let escaped_local_raise =
  let bad () = raise Exit in
  expects_total bad
;;

[%%expect
  {|
Line 3, characters 16-19:
3 |   expects_total bad
                    ^^^
Error: This value is "partial"
         because it closes over the value "raise" at line 2, characters 15-20
         which is "partial".
       However, the highlighted expression is expected to be "total".
|}]

(* A11: no expects_total at all — force via a local type/mode annotation. *)
let escaped_local_annot =
  let bad () = ref 0 in
  (bad : (unit -> int ref) @ total)
;;

[%%expect
  {|
Line 3, characters 3-6:
3 |   (bad : (unit -> int ref) @ total)
       ^^^
Error: This value is "partial"
         because it closes over the value "ref" at line 2, characters 15-18
         which is "partial".
       However, the highlighted expression is expected to be "total".
|}]

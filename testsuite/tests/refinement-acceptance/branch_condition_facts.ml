(* TEST
 expect;
*)

(* ============================================================= *)
(* ACCEPTANCE CORPUS: branch-local condition facts                *)
(*                                                                *)
(* plan.html "How checking works" (afterwards pass) describes a    *)
(* scoped list of facts flowing through the tree.  A conditional   *)
(* contributes the condition (in the then-branch) and its negation *)
(* (in the else-branch) as branch-local facts, so an obligation    *)
(* guarded by the test discharges -- and this must hold for an     *)
(* ordinary [if] that carries no refinement mark of its own, which *)
(* is the common [if guard then use-needing-guard] idiom.          *)
(*                                                                 *)
(* Marker legend: see binder_facts.ml.                             *)
(* ============================================================= *)

let needs_pos (x : int{ _ > 0 }) = x
[%%expect {|
val needs_pos : int{ (app[Stdlib!.>] _ 0) } -> int = <fun>
|}]

(* @acc id=bcf_then_guard final=ACCEPT today=ACCEPT stable=yes unlocks=verification
   The then-branch sees the condition as a fact: [y > 0] discharges the
   contract obligation on the guarded call. *)
let bcf_then_guard (y : int) = if y > 0 then needs_pos y else 0
[%%expect {|
val bcf_then_guard : int -> int = <fun>
|}]

(* @acc id=bcf_else_negation final=ACCEPT today=ACCEPT stable=yes unlocks=verification
   The else-branch sees the negated condition: from [not (y <= 0)] the
   obligation [y > 0] discharges. *)
let bcf_else_negation (y : int) = if y <= 0 then 0 else needs_pos y
[%%expect {|
val bcf_else_negation : int -> int = <fun>
|}]

(* @acc id=bcf_no_guard final=REJECT today=REJECT stable=yes unlocks=verification
   Control: with no guard there is no fact, so the contract obligation
   [y > 0] is unprovable. *)
let bcf_no_guard (y : int) = needs_pos y
[%%expect {|
Line 1, characters 39-40:
1 | let bcf_no_guard (y : int) = needs_pos y
                                           ^
Error: Refinement verification failed (not-proved)
|}]

(* @acc id=bcf_marked_if final=ACCEPT today=ACCEPT stable=yes unlocks=verification
   A marked [if] remains path-sensitive: each branch is proved to meet
   the annotation under its own condition fact. *)
let bcf_marked_if (y : int) = (if y > 0 then y else 1 : int{ _ > 0 })
[%%expect {|
val bcf_marked_if : int -> int{ (app[Stdlib!.>] _ 0) } = <fun>
|}]

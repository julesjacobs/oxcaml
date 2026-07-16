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
(* is the common [if guard then use-needing-guard] idiom.         *)
(*                                                                *)
(* The condition fact flows only for a TOTAL/PURE condition (the  *)
(* Q-003 purity gate): a condition that applies an impure/opaque  *)
(* function records no fact, so a fact about one evaluation       *)
(* cannot discharge an obligation about a different one           *)
(* (bcf_impure_condition).                                        *)
(*                                                                *)
(* Marker legend: see binder_facts.ml.                            *)
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

(* An opaque, effectful helper for the purity-gate control below. *)
let bad () = read_int ()
[%%expect {|
val bad : unit -> int = <fun>
|}]

(* @acc id=bcf_impure_condition final=REJECT today=REJECT stable=yes unlocks=verification
   PURITY GATE (Q-003): a branch condition that applies an impure/opaque
   function records NO fact, because a fact about one evaluation of [bad ()]
   must not discharge an obligation about a different evaluation.  So the
   guarded annotation stays unprovable and rejects.  Without the gate this
   accepted, although [bad ()] may return a different value at the two calls --
   a magic-free unsoundness. *)
let bcf_impure_condition () = if bad () > 0 then needs_pos (bad ()) else 0
[%%expect {|
Line 1, characters 59-67:
1 | let bcf_impure_condition () = if bad () > 0 then needs_pos (bad ()) else 0
                                                               ^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

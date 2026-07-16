(* TEST
 expect;
*)

(* ============================================================= *)
(* ACCEPTANCE CORPUS: skeleton use / covariant weakening          *)
(*                                                                *)
(* plan.html "Equality and unification": dropping a refinement is *)
(* allowed where the direction is known -- "in the typechecker's  *)
(* expected-type path, where a refined value is being USED and    *)
(* may be weakened to its skeleton". Combined with "every use of  *)
(* a variable is skeleton-typed", a TOP-LEVEL refined value used  *)
(* in a bare context is accepted (weakened), NOT a clash.         *)
(*                                                                *)
(* This is the counterpart to rigid_unification.ml: the same      *)
(* refined-vs-bare meeting that is a permanent clash when NESTED   *)
(* becomes an accepted weakening when it is a top-level USE. All   *)
(* cases here REJECT today (base has no integration rules, so a    *)
(* top-level refinement is not yet stripped on use) and must FLIP  *)
(* to ACCEPT when the integration rules land -- they are the      *)
(* tripwire proving the strip/weaken behavior actually shipped.   *)
(*                                                                *)
(* Marker legend: see binder_facts.ml.                            *)
(* ============================================================= *)

(* @acc id=sw_annot_to_skeleton final=ACCEPT today=REJECT stable=no unlocks=integration
   A refined value annotated at its own skeleton. Weakening in the
   expected-type path: accepts, no obligation (the target is bare).
   TODAY: rejected -- the base does not yet strip/weaken on use. *)
let sw_annot_to_skeleton (x : int{ _ = 1 }) = (x : int)
[%%expect {|
Line 1, characters 47-48:
1 | let sw_annot_to_skeleton (x : int{ _ = 1 }) = (x : int)
                                                   ^
Error: The value "x" has type "int{ (app[Stdlib!.=] _ 1) }"
       but an expression was expected of type "int"
|}]

(* @acc id=sw_use_in_arith final=ACCEPT today=REJECT stable=no unlocks=integration
   The most common use: a refined value in bare arithmetic. [x] is
   used at [int], so [x + 1] type-checks.
   FINAL: accepts. TODAY: rejected. *)
let sw_use_in_arith (x : int{ _ = 1 }) = x + 1
[%%expect {|
Line 1, characters 41-42:
1 | let sw_use_in_arith (x : int{ _ = 1 }) = x + 1
                                             ^
Error: The value "x" has type "int{ (app[Stdlib!.=] _ 1) }"
       but an expression was expected of type "int"
|}]

(* @acc id=sw_pass_to_bare_param final=ACCEPT today=REJECT stable=no unlocks=integration
   A refined value passed where a BARE parameter is expected: the use
   weakens to the skeleton, so no contract obligation arises.
   FINAL: accepts. TODAY: rejected. *)
let sink (y : int) = y
let sw_pass_to_bare_param (x : int{ _ = 1 }) = sink x
[%%expect {|
val sink : int -> int = <fun>
Line 2, characters 52-53:
2 | let sw_pass_to_bare_param (x : int{ _ = 1 }) = sink x
                                                        ^
Error: The value "x" has type "int{ (app[Stdlib!.=] _ 1) }"
       but an expression was expected of type "int"
|}]

(* @acc id=sw_neutral_if_branches final=ACCEPT today=REJECT stable=no unlocks=integration
   Neutral unification site (if-branches) with a refined and a bare
   branch: both weaken to [int], so the [if] has type [int].
   FINAL: accepts. TODAY: rejected (branches meet refined-vs-bare
   before any stripping). Contrast ru_* where nesting keeps it rigid. *)
let sw_neutral_if_branches b (x : int{ _ = 1 }) = if b then x else 0
[%%expect {|
Line 1, characters 67-68:
1 | let sw_neutral_if_branches b (x : int{ _ = 1 }) = if b then x else 0
                                                                       ^
Error: The constant "0" has type "int" but an expression was expected of type
         "int{ (app[Stdlib!.=] _ 1) }"
|}]

(* TEST
 expect;
*)

(* ============================================================= *)
(* ACCEPTANCE CORPUS: cross-occurrence fact identification        *)
(*                                                                *)
(* The verification pass records a call's refined result as a fact *)
(* keyed to the LOWERED call expression, and the Lean backend      *)
(* identifies structurally-equal opaque terms.  So a fact about    *)
(* one occurrence of an opaque call [g ()] can discharge an        *)
(* obligation about another occurrence of [g ()].  This is:        *)
(*                                                                 *)
(*   - SOUND for a pure/total function, since [g () = g ()]        *)
(*     (fp_pure_xocc);                                             *)
(*   - only UNSOUND for an impure function whose two evaluations   *)
(*     differ -- but such a function cannot be given a proved      *)
(*     refined result today (its body is not representable), so no *)
(*     fact about it is ever recorded (fp_impure_undefinable).     *)
(*     Effectful predicates are in any case covered by the global  *)
(*     "unsound until the totality/logicality modes merge" stub.   *)
(*                                                                 *)
(* The one way to plant a bogus fact about an opaque occurrence is *)
(* via [Obj.magic] (fp_magic_combined): the laundered field/result *)
(* binding records [Obj.magic 0 > 0], which then discharges a      *)
(* later, structurally-equal obligation.  This is the accepted     *)
(* [Obj.magic] hole (see imposition_channels.ml), recorded here as *)
(* an anchor.                                                       *)
(*                                                                 *)
(* Marker legend: see binder_facts.ml.                             *)
(* ============================================================= *)

(* @acc id=fp_pure_xocc final=ACCEPT today=ACCEPT stable=yes unlocks=verification
   A pure refined-result function used at two occurrences: sound. *)
let g () : int{ _ = 1 } = 1
let fp_pure_xocc_a = (g () : int{ _ = 1 })
let fp_pure_xocc_b = (g () : int{ _ = 1 })
[%%expect {|
val g : unit -> int{ (app[Stdlib!.=] _ 1) } = <fun>
val fp_pure_xocc_a : int{ (app[Stdlib!.=] _ 1) } = 1
val fp_pure_xocc_b : int{ (app[Stdlib!.=] _ 1) } = 1
|}]

(* @acc id=fp_impure_undefinable final=REJECT today=REJECT stable=yes unlocks=verification
   An impure function cannot be given a proved refined result, so no
   fact about an opaque occurrence of it is ever recorded: the
   magic-free vehicle for cross-occurrence pollution is not reachable. *)
let counter = ref 0
let fp_impure_undefinable () : int{ _ > 0 } =
  incr counter;
  !counter
[%%expect {|
val counter : int ref = {contents = 0}
Lines 3-4, characters 2-10:
3 | ..incr counter;
4 |   !counter
Error: Refinement verification failed: a sequence cannot yet be represented in a verification condition
|}]

(* @acc id=fp_magic_combined final=ACCEPT today=ACCEPT stable=no unlocks=none
   ACCEPTED [Obj.magic] hole: the laundered field binding plants the fact
   [Obj.magic 0 > 0], which then discharges the later structurally-equal
   annotation obligation. *)
type r = { f : int{ _ > 0 } }
let fp_launder = { f = Obj.magic 0 }
let fp_magic_combined = (Obj.magic 0 : int{ _ > 0 })
[%%expect {|
type r = { f : int{ (app[Stdlib!.>] _ 0) }; }
val fp_launder : r = {f = 0}
val fp_magic_combined : int{ (app[Stdlib!.>] _ 0) } = 0
|}]

(* TEST
 expect;
*)

(* ============================================================= *)
(* ACCEPTANCE CORPUS: refined-slot imposition channels            *)
(*                                                                *)
(* Besides the expression annotation (annotation_obligations.ml)  *)
(* and the function contract argument (contract_obligations.ml), a *)
(* refined type is imposed by a record field, a constructor        *)
(* argument, an array element, a mutable-cell assignment, and an   *)
(* optional-argument default.  This file is the DETERMINATION of   *)
(* whether these unmarked channels can reach a silent false        *)
(* refinement, per the user ruling that unsoundness reachable ONLY *)
(* via [Obj.magic] (an unsafe cast) is ACCEPTED.                   *)
(*                                                                 *)
(* Every magic-free vehicle is sound or blocked:                   *)
(*   - a CONCRETE value clashes rigidly [imp_concrete_...];               *)
(*   - a PARAMETER whose refinement is inferred from the slot      *)
(*     propagates the obligation to call sites, where the contract *)
(*     fires [imp_propagate_...]: [let mk x = { f = x }] gives            *)
(*     [mk : int{_>0} -> r], and [mk 0] is rejected;               *)
(*   - a BOTTOM value (a diverging/never-returning expression)     *)
(*     may inhabit any type soundly, so refining it is sound       *)
(*     [imp_bottom_...];                                                  *)
(*   - an IMPURE function CAN have a proved refined result, but the *)
(*     result fact is its PROVEN contract, which holds for every    *)
(*     evaluation, so cross-occurrence use is sound (fact_pollution *)
(*     .ml); a false contract needs an unsafe cast.  (Impurity and  *)
(*     representability are orthogonal: a sequence-bodied function  *)
(*     is rejected by the representability limit, not by impurity.) *)
(*                                                                 *)
(* The one vehicle that produces a REAL mis-typed value is         *)
(* [Obj.magic] [imp_magic_...]: it laundered a refinement through         *)
(* the slot, accepted silently.  Per the user ruling this is an    *)
(* ACCEPTED KNOWN LIMITATION -- "using Obj.magic is basically      *)
(* saying that you want to do something arbitrary there" -- so the *)
(* imp_magic_* cases are ANCHORS recording the current accept, not *)
(* required rejections.  If a future change makes them reject that  *)
(* is a welcome tightening and these blocks flip loudly.           *)
(*                                                                 *)
(* Marker legend: see binder_facts.ml.                             *)
(* ============================================================= *)

let mk_pos () : int{ _ > 0 } = 1
[%%expect {|
val mk_pos : unit -> int{ (app[Stdlib!.>] _ 0) } = <fun>
|}]

type r = { f : int{ _ > 0 } }
[%%expect {|
type r = { f : int{ (app[Stdlib!.>] _ 0) }; }
|}]

type w = Wrap of int{ _ > 0 }
[%%expect {|
type w = Wrap of int{ (app[Stdlib!.>] _ 0) }
|}]

(* --- concrete: rigid clash (magic-free, sound) --- *)

(* @acc id=imp_concrete_field final=REJECT today=REJECT stable=yes unlocks=verification *)
let imp_concrete_field = { f = 0 }
[%%expect {|
Line 1, characters 31-32:
1 | let imp_concrete_field = { f = 0 }
                                   ^
Error: The constant "0" has type "int" but an expression was expected of type
         "int{ (app[Stdlib!.>] _ 0) }"
|}]

(* @acc id=imp_concrete_ctor final=REJECT today=REJECT stable=yes unlocks=verification *)
let imp_concrete_ctor = Wrap 0
[%%expect {|
Line 1, characters 29-30:
1 | let imp_concrete_ctor = Wrap 0
                                 ^
Error: The constant "0" has type "int" but an expression was expected of type
         "int{ (app[Stdlib!.>] _ 0) }"
|}]

(* --- parameter propagation: obligation moves to call sites (sound) --- *)

(* @acc id=imp_propagate_def final=ACCEPT today=ACCEPT stable=yes unlocks=verification
   The field infers the parameter as refined: [mk : int{ _ > 0 } -> r]. *)
let mk x = { f = x }
[%%expect {|
val mk : int{ (app[Stdlib!.>] _ 0) } -> r = <fun>
|}]

(* @acc id=imp_propagate_badcall final=REJECT today=REJECT stable=yes unlocks=verification
   The contract fires at the call site even though the refined domain
   arrived by inference rather than annotation. *)
let imp_propagate_badcall = mk 0
[%%expect {|
Line 1, characters 31-32:
1 | let imp_propagate_badcall = mk 0
                                   ^
Error: Refinement verification failed (disproved)
|}]

(* @acc id=imp_propagate_goodcall final=ACCEPT today=ACCEPT stable=yes unlocks=verification *)
let imp_propagate_goodcall = mk 5
[%%expect {|
val imp_propagate_goodcall : r = {f = 5}
|}]

(* --- bottom values: sound to refine (never produce a value) --- *)

let rec loop () : 'a = loop ()
[%%expect {|
val loop : unit -> 'a = <fun>
|}]

(* @acc id=imp_bottom_field final=ACCEPT today=ACCEPT stable=yes unlocks=verification
   A diverging value in a refined field: sound, as bottom inhabits every
   type. *)
let imp_bottom_field () = { f = loop () }
[%%expect {|
val imp_bottom_field : unit -> r = <fun>
|}]

(* @acc id=imp_bottom_ctor final=ACCEPT today=ACCEPT stable=yes unlocks=verification *)
let imp_bottom_ctor () = Wrap (loop ())
[%%expect {|
val imp_bottom_ctor : unit -> w = <fun>
|}]

(* --- refined call result: fact discharges the slot (sound) --- *)

(* @acc id=imp_result_field final=ACCEPT today=ACCEPT stable=yes unlocks=verification *)
let imp_result_field = { f = mk_pos () }
[%%expect {|
val imp_result_field : r = {f = 1}
|}]

(* --- Obj.magic: ACCEPTED KNOWN LIMITATION per user ruling (anchor) --- *)

(* @acc id=imp_magic_field final=ACCEPT today=ACCEPT stable=no unlocks=none
   [Obj.magic] launders a refinement into a field.  ACCEPTED hole. *)
let imp_magic_field = { f = Obj.magic 0 }
[%%expect {|
val imp_magic_field : r = {f = 0}
|}]

(* @acc id=imp_magic_ctor final=ACCEPT today=ACCEPT stable=no unlocks=none *)
let imp_magic_ctor = Wrap (Obj.magic 0)
[%%expect {|
val imp_magic_ctor : w = Wrap 0
|}]

(* @acc id=imp_magic_array final=ACCEPT today=ACCEPT stable=no unlocks=none *)
let imp_magic_array : int{ _ > 0 } array = [| Obj.magic 0 |]
[%%expect {|
val imp_magic_array : int{ (app[Stdlib!.>] _ 0) } array = [|0|]
|}]

(* @acc id=imp_magic_mutassign final=ACCEPT today=ACCEPT stable=no unlocks=none *)
let imp_magic_mutassign () =
  let mutable x : int{ _ > 0 } = 1 in
  x <- Obj.magic 0;
  x
[%%expect {|
val imp_magic_mutassign : unit -> int{ (app[Stdlib!.>] _ 0) } = <fun>
|}]

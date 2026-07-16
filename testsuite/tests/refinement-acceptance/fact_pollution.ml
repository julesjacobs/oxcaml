(* TEST
 expect;
*)

(* ============================================================= *)
(* ACCEPTANCE CORPUS: cross-occurrence fact identification        *)
(*                                                                *)
(* The verification pass records a call's refined result as a fact *)
(* keyed to the LOWERED call expression (vox_verify.ml, around the *)
(* [result_subject] binding in [check_application]), and the Lean  *)
(* backend gives structurally-equal opaque reference heads the     *)
(* SAME name (vox_lean.ml, [note_reference]/[same_reference]).  So *)
(* a fact about one occurrence of an opaque call [g ()] can be     *)
(* used for another occurrence of [g ()].  Binder facts, by        *)
(* contrast, are keyed to the binder's [Ident] ([Rbound]) and are  *)
(* scoped, so they never cross occurrences.                        *)
(*                                                                 *)
(* Is cross-occurrence identification sound?  A fact derived from a *)
(* refined RESULT is only recorded once the function's body has     *)
(* PROVEN the result contract, which therefore holds for EVERY      *)
(* evaluation.  So using such a fact at another occurrence is sound *)
(* even for an impure function: both evaluations differ in value    *)
(* but satisfy the same proven contract (fp_impure_definable /      *)
(* fp_impure_xocc).  A FALSE or VALUE-VARYING exact contract cannot *)
(* be held without an unsafe cast: a deterministic-constant body    *)
(* proves an exact contract soundly even when it is impure          *)
(* (fp_impure_const), whereas a value-varying body cannot           *)
(* (fp_impure_no_false_contract).  So the only bogus fact about an  *)
(* opaque occurrence comes from [Obj.magic] (fp_magic_combined),    *)
(* i.e. the accepted hole.                                          *)
(*                                                                 *)
(* Impurity and representability are ORTHOGONAL: an impure function *)
(* can have a proved refined result (above), while a function whose *)
(* body is a SEQUENCE (e.g. [incr r; !r]) is rejected not because   *)
(* it is impure but because a sequence is not yet representable as  *)
(* a verification-condition subject (the [unsupported] arms in      *)
(* vox_verify.ml).  Representability is a completeness limit, not a  *)
(* soundness one.                                                    *)
(*                                                                 *)
(* The branch-condition impurity vehicle -- an impure [if] guard  *)
(* whose fact was identified across evaluations -- is CLOSED by   *)
(* the Q-003 purity gate (branch_condition_facts.ml,              *)
(* bcf_impure_condition records no fact for a non-total           *)
(* condition).  The remaining residual unsoundness is an IMPURE   *)
(* EXPRESSION used                                                *)
(* DIRECTLY in a predicate, where two occurrences are identified as *)
(* EQUAL (fp_impure_expr_in_pred): this is not specific to any      *)
(* imposition channel and is the pre-existing "unsound until the    *)
(* totality/logicality modes merge" stub (plan.html) -- a predicate *)
(* is currently checked as an ordinary expression, so a partial     *)
(* impure [read_int ()] is not yet rejected.  It flips when the     *)
(* mode discipline lands.                                           *)
(*                                                                 *)
(* Marker legend: see binder_facts.ml.  Calls that would block on   *)
(* stdin are guarded under functions so the toplevel does not run   *)
(* them.                                                            *)
(* ============================================================= *)

(* @acc id=fp_pure_xocc final=ACCEPT today=ACCEPT stable=yes unlocks=verification
   A pure refined-result function used at two occurrences: sound. *)
let g_pure () : int{ _ = 1 } = 1
let fp_pure_xocc_a = (g_pure () : int{ _ = 1 })
let fp_pure_xocc_b = (g_pure () : int{ _ = 1 })
[%%expect {|
val g_pure : unit -> int{ (app[Stdlib!.=] _ 1) } = <fun>
val fp_pure_xocc_a : int{ (app[Stdlib!.=] _ 1) } = 1
val fp_pure_xocc_b : int{ (app[Stdlib!.=] _ 1) } = 1
|}]

(* @acc id=fp_impure_definable final=ACCEPT today=ACCEPT stable=yes unlocks=verification
   An impure function CAN be given a proved refined result: the body's
   contract [_ > 0] is proved by case analysis and holds for every
   evaluation. *)
let g_impure () : int{ _ > 0 } =
  let x = read_int () in
  if x > 0 then x else 1
[%%expect {|
val g_impure : unit -> int{ (app[Stdlib!.>] _ 0) } = <fun>
|}]

(* @acc id=fp_impure_xocc final=ACCEPT today=ACCEPT stable=yes unlocks=verification
   Two occurrences of the impure [g_impure ()]: the fact from one meets
   the obligation about the other and discharges.  SOUND: the proven
   contract [_ > 0] holds for every evaluation, so both occurrences
   satisfy it even though they differ in value. *)
let fp_impure_xocc () =
  let _first = (g_impure () : int{ _ > 0 }) in
  (g_impure () : int{ _ > 0 })
[%%expect {|
val fp_impure_xocc : unit -> int{ (app[Stdlib!.>] _ 0) } = <fun>
|}]

(* @acc id=fp_impure_const final=ACCEPT today=ACCEPT stable=yes unlocks=verification
   An impure body CAN prove an exact-value contract when the returned value
   is a deterministic constant: the [read_int ()] side effect does not change
   the result [5], so [_ = 5] holds for every evaluation.  Impurity alone does
   not block an exact contract; a value-varying body does
   (fp_impure_no_false_contract). *)
let fp_impure_const () : int{ _ = 5 } =
  let _x = read_int () in
  5
[%%expect {|
val fp_impure_const : unit -> int{ (app[Stdlib!.=] _ 5) } = <fun>
|}]

(* @acc id=fp_impure_no_false_contract final=REJECT today=REJECT stable=yes unlocks=verification
   An impure function cannot hold a FALSE (exact-value) result contract:
   the body [read_int ()] cannot prove [_ = 5], so no bogus exact fact
   about an opaque occurrence can be minted magic-free. *)
let fp_impure_no_false_contract () : int{ _ = 5 } = read_int ()
[%%expect {|
Line 1, characters 52-63:
1 | let fp_impure_no_false_contract () : int{ _ = 5 } = read_int ()
                                                        ^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

(* @acc id=fp_impure_expr_in_pred final=REJECT today=ACCEPT stable=no unlocks=modes
   RESIDUAL, pre-existing modes-stub unsoundness (NOT an imposition-channel
   issue): an impure expression used directly in a predicate has its two
   occurrences identified as equal, so [read_int () = read_int ()] proves
   although the two reads differ at runtime.  Flips to REJECT when the
   total/logical mode discipline on predicates lands. *)
let fp_impure_expr_in_pred () = (read_int () : int{ _ = read_int () })
[%%expect {|
val fp_impure_expr_in_pred :
  unit ->
  int{ (app[Stdlib!.=] _ (app[Stdlib!.read_int] constructor[unit/7!.()])) } =
  <fun>
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

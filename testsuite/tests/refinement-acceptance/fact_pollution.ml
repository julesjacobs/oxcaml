(* TEST
 expect;
*)

(* ============================================================= *)
(* ACCEPTANCE CORPUS: opaque-call fact isolation                   *)
(*                                                                *)
(* The verification pass records a call's refined result as a fact *)
(* keyed to the LOWERED call expression (vox_verify.ml,            *)
(* [opaque_call_subject]).  Re-visiting the same typed expression  *)
(* reuses its subject, but physically distinct expressions retain  *)
(* distinct subjects even when a PPX gives them the same location. *)
(* [dependent_arrow_unstable_calls.ml] pins that distinction.      *)
(*                                                                 *)
(* A fact derived from a refined RESULT is recorded only after the  *)
(* function's body has proved that result contract.  Each call can  *)
(* therefore use its own result fact even for an impure function:   *)
(* evaluations may differ in value while each satisfies the proved  *)
(* contract (fp_impure_definable / fp_impure_xocc).  A false or     *)
(* value-varying exact contract still requires an unsafe cast: a    *)
(* deterministic-constant body proves an exact contract soundly     *)
(* even when it is impure (fp_impure_const), whereas a varying body *)
(* cannot (fp_impure_no_false_contract).                             *)
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
(* condition).  An IMPURE EXPRESSION used DIRECTLY in a predicate *)
(* is also closed: the total/logical predicate mode discipline     *)
(* rejects the partial [read_int ()] before its two occurrences    *)
(* could be identified as equal (fp_impure_expr_in_pred).          *)
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
val g_pure : unit -> int{ _ = 1 } = <fun>
val fp_pure_xocc_a : int{ _ = 1 } = 1
val fp_pure_xocc_b : int{ _ = 1 } = 1
|}]

(* @acc id=fp_impure_definable final=ACCEPT today=ACCEPT stable=yes unlocks=verification
   An impure function CAN be given a proved refined result: the body's
   contract [_ > 0] is proved by case analysis and holds for every
   evaluation. *)
let g_impure () : int{ _ > 0 } =
  let x = read_int () in
  if x > 0 then x else 1
[%%expect {|
val g_impure : unit -> int{ _ > 0 } = <fun>
|}]

(* @acc id=fp_impure_xocc final=ACCEPT today=ACCEPT stable=yes unlocks=verification
   Two occurrences of the impure [g_impure ()] independently receive the
   proved result contract [_ > 0], so both obligations discharge even when
   the evaluations return different values. *)
let fp_impure_xocc () =
  let _first = (g_impure () : int{ _ > 0 }) in
  (g_impure () : int{ _ > 0 })
[%%expect {|
val fp_impure_xocc : unit -> int{ _ > 0 } = <fun>
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
val fp_impure_const : unit -> int{ _ = 5 } = <fun>
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

(* @acc id=fp_impure_expr_in_pred final=REJECT today=REJECT stable=yes
   An impure expression used directly in a predicate is rejected by the
   total/logical mode discipline, before its two occurrences could be
   identified as equal. *)
let fp_impure_expr_in_pred () = (read_int () : int{ _ = read_int () })
[%%expect {|
Line 1, characters 56-64:
1 | let fp_impure_expr_in_pred () = (read_int () : int{ _ = read_int () })
                                                            ^^^^^^^^
Error: The value "read_int" is "partial"
       but is expected to be "total"
         because it is used in an expression (at line 1, characters 47-69).
|}]

type r = { f : int{ _ > 0 } }
[%%expect {|
type r = { f : int{ _ > 0 }; }
|}]

(* Exact direct [%obj_magic] identity is visible and now leaves an obligation;
   this is the welcome tightening anticipated by imposition_channels.ml. *)
let fp_direct_launder = { f = Obj.magic 0 }
[%%expect {|
Line 1, characters 30-41:
1 | let fp_direct_launder = { f = Obj.magic 0 }
                                  ^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

(* @acc id=fp_magic_combined final=ACCEPT today=ACCEPT stable=no unlocks=none
   ACCEPTED indirect [Obj.magic] hole: after aliasing hides primitive identity,
   the laundered field binding plants a structurally matching fact for the
   later annotation obligation. *)
let fp_hidden_magic = Obj.magic
let fp_launder = { f = fp_hidden_magic 0 }
let fp_magic_combined = (fp_hidden_magic 0 : int{ _ > 0 })
[%%expect {|
val fp_hidden_magic : 'a -> 'b = <fun>
val fp_launder : r = {f = 0}
val fp_magic_combined : int{ _ > 0 } = 0
|}]

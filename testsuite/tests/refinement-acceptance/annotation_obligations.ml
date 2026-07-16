(* TEST
 expect;
*)

(* ============================================================= *)
(* ACCEPTANCE CORPUS: annotation obligations                      *)
(*                                                                *)
(* plan.html "How checking works": an annotation (e : t{p}) types *)
(* [e] against the skeleton [t] and marks the node as a proof     *)
(* obligation. The mark fires exactly where rigid unification     *)
(* would otherwise fail, so annotating a value that ALREADY has   *)
(* the same refinement costs nothing (no obligation). A provable  *)
(* predicate must eventually discharge; an unprovable one must    *)
(* eventually fail with a clean verification error.               *)
(*                                                                *)
(* Marker legend: see binder_facts.ml.                            *)
(* ============================================================= *)

(* @acc id=ao_provable final=ACCEPT today=ACCEPT stable=no unlocks=integration+verification
   Provable predicate on a bare value: [5 >= 0].
   FINAL: accepts (VC [5 >= 0] discharged by Lean).
   TODAY: rejected -- bare [5] rigidly clashes with the refined type. *)
let ao_provable = (5 : int{ _ >= 0 })
[%%expect {|
val ao_provable : int{ (app[Stdlib!.>=] _ 0) } = 5
|}]

(* @acc id=ao_unprovable final=REJECT today=ACCEPT stable=no unlocks=integration+verification
   Unprovable predicate on a bare value: [-5 >= 0] is false.
   FINAL: rejected with a clean VERIFICATION error (unprovable VC).
   TODAY: rejected with a rigid type-clash error -- same outcome
   (reject) but the message must change to a VC failure when the
   verification pass lands. *)
let ao_unprovable = (-5 : int{ _ >= 0 })
[%%expect {|
val ao_unprovable : int{ (app[Stdlib!.>=] _ 0) } = -5
|}]

(* @acc id=ao_same_refinement final=ACCEPT today=ACCEPT stable=yes unlocks=-
   Same-refinement annotation: [x] already carries [int{ _ = 1 }] and
   is annotated with the identical refinement. No obligation is
   incurred (the marks fire only where rigid unification would fail;
   here it succeeds). This is the one behavior already at its final
   form today, and this [%%expect] block is an anchor.
   (Mechanism tightens internally -- rigid-equal today, trivially
   discharged VC once integration lands -- but the OUTCOME and the
   printed signature are stable: accepts.) *)
let ao_same_refinement (x : int{ _ = 1 }) = (x : int{ _ = 1 })
[%%expect {|
val ao_same_refinement :
  int{ (app[Stdlib!.=] _ 1) } -> int{ (app[Stdlib!.=] _ 1) } = <fun>
|}]

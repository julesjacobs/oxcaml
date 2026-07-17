(* TEST
 expect;
*)

(* ============================================================= *)
(* ACCEPTANCE CORPUS: refined annotation inside a total closure   *)
(*                                                                *)
(* This anchor records the interaction between the refinement     *)
(* layer and the totality/logicality mode axes on the first tree  *)
(* that carries both (the v2 -> refinement merge).                *)
(*                                                                *)
(* Per the canonical plan a refinement predicate is checked at    *)
(* total with the variables it mentions presented @ logical -- so *)
(* the predicate belongs to a LOGICAL context and must be isolated *)
(* from the host closure's capture/totality analysis. That        *)
(* isolation is the "logical closure lock" supplied by           *)
(* [with_refinement_typing_frame] (typing/typecore.ml).           *)
(* Comparison primitives are admitted only while checking the     *)
(* predicate, and the lock prevents them from polluting the host   *)
(* closure's capture and totality analysis.                        *)
(*                                                                *)
(* Marker legend: see binder_facts.ml.                            *)
(* ============================================================= *)

let expects_total (f @ total) = f
[%%expect {|
val expects_total : 'a @ total -> 'a = <fun>
|}]

(* @acc id=refined_in_total_toplevel final=ACCEPT today=ACCEPT stable=yes unlocks=verification
   Baseline (no enclosing closure): a refined annotation whose predicate
   is provable is accepted and verified at top level.  This is the
   refinement behavior "as before"; the mode axes do not touch it. *)
let toplevel = (2 : int{ _ > 0 })
[%%expect {|
val toplevel : int{ _ > 0 } = 2
|}]

(* @acc id=refined_in_ordinary_closure final=ACCEPT today=ACCEPT stable=yes unlocks=verification
   The same annotation inside an ORDINARY (non-total) closure is also
   accepted: with no totality requirement on the closure, the predicate's
   comparison is irrelevant to capture analysis. *)
let ordinary = fun () -> (2 : int{ _ > 0 })
[%%expect {|
val ordinary : unit -> int{ _ > 0 } = <fun>
|}]

(* @acc id=refined_in_total_closure final=ACCEPT today=ACCEPT stable=yes
   THE INTERACTION: the same refined annotation inside a closure that is
   required to be [total].  The predicate is checked in
   a logical context, isolated from the closure's capture analysis by the
   logical closure lock in [with_refinement_typing_frame], so its comparison
   does not make the host closure partial.  (The closure is annotated
   [@ total] rather than relying on the later consumer, because under
   -principal a top-level binding's totality is defaulted at the structure
   boundary before the consumer can constrain it.) *)
let refined_in_total @ total = fun () -> (2 : int{ _ > 0 })
let () = ignore (expects_total refined_in_total)
[%%expect {|
val refined_in_total : unit -> int{ _ > 0 } = <fun>
Line 2, characters 16-48:
2 | let () = ignore (expects_total refined_in_total)
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 5 [ignored-partial-application]: this function application is partial,
  maybe some arguments are missing.
|}]

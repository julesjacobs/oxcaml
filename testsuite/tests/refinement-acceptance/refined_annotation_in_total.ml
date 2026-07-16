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
(* isolation is the "logical closure lock" that                   *)
(* [with_refinement_typing_frame] (typing/typecore.ml) still      *)
(* leaves as a VOX2_MODES_TODO: today the frame elaborates the     *)
(* predicate without the lock, so a predicate mentioning a         *)
(* comparison (partial in the totality axis) is seen as a partial  *)
(* value CAPTURED by an enclosing total closure, and the closure   *)
(* over-rejects.  This is conservative (sound): it rejects a       *)
(* program that will be accepted once modes integration wires the  *)
(* logical closure lock at that marker.                            *)
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
val toplevel : int{ (app[Stdlib!.>] _ 0) } = 2
|}]

(* @acc id=refined_in_ordinary_closure final=ACCEPT today=ACCEPT stable=yes unlocks=verification
   The same annotation inside an ORDINARY (non-total) closure is also
   accepted: with no totality requirement on the closure, the predicate's
   comparison is irrelevant to capture analysis. *)
let ordinary = fun () -> (2 : int{ _ > 0 })
[%%expect {|
val ordinary : unit -> int{ (app[Stdlib!.>] _ 0) } = <fun>
|}]

(* @acc id=refined_in_total_closure final=ACCEPT today=REJECT stable=no unlocks=modes
   THE INTERACTION: the same refined annotation inside a closure that is
   required to be [total].  FINAL: accepts -- the predicate is checked in
   a logical context, isolated from the closure's capture analysis by the
   logical closure lock (the second VOX2_MODES_TODO in
   [with_refinement_typing_frame]).  TODAY: over-rejects, because that lock
   is not yet wired, so the predicate's [>] is seen as a partial value
   captured by the total closure.  Sound (conservative); flips to ACCEPT
   when modes integration lands. *)
let refined_in_total = fun () -> (2 : int{ _ > 0 })
let () = ignore (expects_total refined_in_total)
[%%expect {|
val refined_in_total : unit -> int{ (app[Stdlib!.>] _ 0) } = <fun>
Line 2, characters 31-47:
2 | let () = ignore (expects_total refined_in_total)
                                   ^^^^^^^^^^^^^^^^
Error: This value is "partial" but is expected to be "total".
|}]

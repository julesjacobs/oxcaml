(* TEST
 expect;
*)

(* ============================================================= *)
(* ACCEPTANCE CORPUS: scope and mutation guards                   *)
(*                                                                *)
(* plan.html "How checking works" (afterwards pass): "Facts that  *)
(* mention out-of-scope variables are dropped (which only weakens *)
(* conditions), mutable state is versioned and havocked on writes" *)
(* and (typechecker) "mutable binders are exempt" from becoming    *)
(* facts, because "a persistent fact about a mutable would survive *)
(* assignment".                                                    *)
(*                                                                *)
(* Marker legend: see binder_facts.ml.                            *)
(* ============================================================= *)

(* @acc id=scope_fact_in_scope final=ACCEPT today=ACCEPT stable=no unlocks=integration+verification
   A fact is available where its binder is in scope: the inner
   annotation [(x : int{ _ = 7 })] is proved from the fact [x = 7]
   recorded at the inner binder. The outer result just forwards it at
   the skeleton.
   FINAL and TODAY: accepts. *)
let scope_fact_in_scope () =
  let outer =
    let x = (7 : int{ _ = 7 }) in
    (x : int{ _ = 7 })
  in
  outer
[%%expect {|
val scope_fact_in_scope : unit -> int = <fun>
|}]

(* @acc id=scope_fact_dropped final=REJECT today=REJECT stable=no unlocks=integration+verification
   A fact goes OUT OF SCOPE: [x = 7] holds only inside the inner let.
   Once [r] escapes, the fact mentioning [x] is dropped, so the
   obligation [r = 7] is NOT provable (dropping only weakens; it
   never lets an unprovable condition through).
   FINAL: rejected with a verification error (unprovable VC).
   TODAY: rejected with the final verification error. *)
let scope_fact_dropped () =
  let r =
    let x = (7 : int{ _ = 7 }) in
    x + 0
  in
  (r : int{ _ = 7 })
[%%expect {|
Line 6, characters 2-20:
6 |   (r : int{ _ = 7 })
      ^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

(* @acc id=mut_binder_exempt final=ACCEPT today=ACCEPT stable=no unlocks=integration+verification
   A mutable refined binder: the initializer obligation ([1 = 1]) is
   discharged, but the binder contributes NO persistent fact. Merely
   declaring and reading it is fine.
   FINAL and TODAY: accepts. *)
let mut_binder_exempt () =
  let mutable x : int{ _ = 1 } = 1 in
  x
[%%expect {|
Line 2, characters 14-15:
2 |   let mutable x : int{ _ = 1 } = 1 in
                  ^
Warning 186 [unmutated-mutable]: mutable variable "x" was never mutated.

val mut_binder_exempt : unit -> int{ (app[Stdlib!.=] _ 1) } = <fun>
|}]

(* @acc id=mut_no_persistent_fact final=REJECT today=REJECT stable=yes unlocks=integration+verification
   A mutable refined cell contributes no persistent fact, so after the
   write there is no standing [x = 1] and re-imposing [int{ _ = 1 }] on
   the read is unprovable.  The write of a CONCRETE constant is caught
   earlier still, rigidly: the cell keeps its refined type [int{ _ = 1 }]
   and the constant [2 : int] clashes with it (this is a rigid-typing
   rejection, NOT a havoc of a fact -- there is no mutable fact to
   havoc).  An unconstrained [Obj.magic] on the right of [<-] is the
   accepted [Obj.magic] hole (imposition_channels.ml, imp_magic_mutassign),
   not a rigid clash.
   FINAL and TODAY: rejected. *)
let mut_no_persistent_fact () =
  let mutable x : int{ _ = 1 } = 1 in
  x <- 2;
  (x : int{ _ = 1 })
[%%expect {|
Line 3, characters 7-8:
3 |   x <- 2;
           ^
Error: The constant "2" has type "int" but an expression was expected of type
         "int{ (app[Stdlib!.=] _ 1) }"
|}]

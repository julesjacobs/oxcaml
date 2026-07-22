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

(* @acc id=scope_fact_dropped final=ACCEPT today=ACCEPT stable=no unlocks=integration+verification
   The inner fact [x = 7] is dropped when [x] leaves scope.  The stable
   result is summarized separately by the closed fact
   [r = (let x = 7 in x + 0)], which proves [r = 7] without exporting [x].
   FINAL and TODAY: accepts. *)
let scope_fact_dropped () =
  let r =
    let x = (7 : int{ _ = 7 }) in
    x + 0
  in
  (r : int{ _ = 7 })
[%%expect {|
val scope_fact_dropped : unit -> int{ _ = 7 } = <fun>
|}]

(* A closed result summary preserves the actual arithmetic; it cannot turn
   [x + 1] into [7]. *)
let scope_closed_summary_wrong () =
  let r =
    let x = (7 : int{ _ = 7 }) in
    x + 1
  in
  (r : int{ _ = 7 })
[%%expect {|
Line 6, characters 2-20:
6 |   (r : int{ _ = 7 })
      ^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (disproved)
|}]

(* An unstable local computation gets no result summary.  The local [x = 7]
   fact still does not escape on its own. *)
let scope_local_fact_not_exported () =
  let r =
    let x = (7 : int{ _ = 7 }) in
    ignore (read_int ());
    x + 0
  in
  (r : int{ _ = 7 })
[%%expect {|
Line 7, characters 2-20:
7 |   (r : int{ _ = 7 })
      ^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

(* Nested aliases and a same-named outer binder do not capture the local
   binder in the closed summary. *)
let scope_alias_shadow_ok () =
  let r =
    let x = (7 : int{ _ = 7 }) in
    let y = x in
    y + 0
  in
  let x = (8 : int{ _ = 8 }) in
  ignore x;
  (r : int{ _ = 7 })
[%%expect {|
val scope_alias_shadow_ok : unit -> int{ _ = 7 } = <fun>
|}]

let scope_alias_shadow_wrong () =
  let r =
    let x = (7 : int{ _ = 7 }) in
    let y = x in
    y + 0
  in
  let x = (8 : int{ _ = 8 }) in
  ignore x;
  (r : int{ _ = 8 })
[%%expect {|
Line 9, characters 2-20:
9 |   (r : int{ _ = 8 })
      ^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (disproved)
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

val mut_binder_exempt : unit -> int{ _ = 1 } = <fun>
|}]

(* A valid write preserves the mutable cell's declared refinement. *)
let mut_refined_write_valid () =
  let mutable x : int{ _ = 1 } = 1 in
  x <- 1;
  (x : int{ _ = 1 })
[%%expect {|
val mut_refined_write_valid : unit -> int{ _ = 1 } = <fun>
|}]

(* @acc id=mut_no_persistent_fact final=REJECT today=REJECT stable=yes unlocks=integration+verification
   A mutable refined cell contributes no persistent fact.  Its declared
   refined type is instead maintained as an invariant: AUTO turns each write
   into an obligation for the assigned value.  Here that obligation is
   [2 = 1], so the write is disproved at its source location.
   FINAL and TODAY: rejected. *)
let mut_no_persistent_fact () =
  let mutable x : int{ _ = 1 } = 1 in
  x <- 2;
  (x : int{ _ = 1 })
[%%expect {|
Line 3, characters 7-8:
3 |   x <- 2;
           ^
Error: Refinement verification failed (disproved)
|}]

(* A plain mutable binder acquires no invariant and no persistent initializer
   fact.  Reading it into an immutable name after a write cannot prove
   [y = 1]. *)
let mut_plain_no_persistent_fact () =
  let mutable x = 1 in
  x <- 2;
  let y = x in
  (y : int{ _ = 1 })
[%%expect {|
Line 5, characters 2-20:
5 |   (y : int{ _ = 1 })
      ^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

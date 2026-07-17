(* TEST
 expect;
*)

(* ============================================================= *)
(* ACCEPTANCE CORPUS: contract obligations                        *)
(*                                                                *)
(* plan.html "How checking works": arguments to a refined         *)
(* parameter are typed at the skeleton with the predicate marked  *)
(* as a CONTRACT obligation, discharged at the call site against  *)
(* the facts in scope. A refined parameter binds at its skeleton  *)
(* inside the body while the arrow domain keeps the refined type. *)
(*                                                                *)
(* Marker legend: see binder_facts.ml.                            *)
(* ============================================================= *)

(* The contract functions themselves define fine today: a refined
   parameter is legal in a signature. Only the CALL SITES carry the
   obligation. *)
let c_eq1 (x : int{ _ = 1 }) = x
[%%expect {|
val c_eq1 : int{ _ = 1 } -> int = <fun>
|}]

(* @acc id=co_provable final=ACCEPT today=ACCEPT stable=no unlocks=integration+verification
   Provable argument to a refined parameter: [1 = 1].
   FINAL: accepts (contract VC [1 = 1] discharged).
   TODAY: accepts via contract verification. *)
let co_provable = c_eq1 1
[%%expect {|
val co_provable : int = 1
|}]

(* @acc id=co_unprovable final=REJECT today=REJECT stable=no unlocks=integration+verification
   Unprovable argument: [2 = 1] is false.
   FINAL: rejected with a clean contract-VERIFICATION error.
   TODAY: rejected with the final verification error. *)
let co_unprovable = c_eq1 2
[%%expect {|
Line 1, characters 26-27:
1 | let co_unprovable = c_eq1 2
                              ^
Error: Refinement verification failed (disproved)
|}]

(* @acc id=co_dependent final=ACCEPT today=ACCEPT stable=no unlocks=integration+verification
   Dependent-ish contract: the second parameter's refinement mentions
   the first parameter ([a : int{ _ = n }]). The definition already
   elaborates today (the predicate resolves [n] as a value reference);
   the obligation lives at the call.
   FINAL: [dep 3 3] accepts (VC [3 = 3] discharged).
   TODAY: accepts. *)
let dep (n : int) (a : int{ _ = n }) = a
[%%expect {|
val dep : int -> int{ _ = n } -> int = <fun>
|}]

let co_dependent = dep 3 3
[%%expect {|
val co_dependent : int = 3
|}]

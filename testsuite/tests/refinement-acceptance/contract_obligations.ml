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
val c_eq1 : int{ (app[Stdlib!.=] _ 1) } -> int{ (app[Stdlib!.=] _ 1) } =
  <fun>
|}]

(* @acc id=co_provable final=ACCEPT today=REJECT stable=no unlocks=integration+verification
   Provable argument to a refined parameter: [1 = 1].
   FINAL: accepts (contract VC [1 = 1] discharged).
   TODAY: rejected -- bare [1] rigidly clashes with the refined domain. *)
let co_provable = c_eq1 1
[%%expect {|
Line 1, characters 24-25:
1 | let co_provable = c_eq1 1
                            ^
Error: The constant "1" has type "int" but an expression was expected of type
         "int{ (app[Stdlib!.=] _ 1) }"
|}]

(* @acc id=co_unprovable final=REJECT today=REJECT stable=no unlocks=integration+verification
   Unprovable argument: [2 = 1] is false.
   FINAL: rejected with a clean contract-VERIFICATION error.
   TODAY: rejected with a rigid type-clash error -- same outcome, the
   message tightens to a VC failure when verification lands. *)
let co_unprovable = c_eq1 2
[%%expect {|
Line 1, characters 26-27:
1 | let co_unprovable = c_eq1 2
                              ^
Error: The constant "2" has type "int" but an expression was expected of type
         "int{ (app[Stdlib!.=] _ 1) }"
|}]

(* @acc id=co_dependent final=ACCEPT today=REJECT stable=no unlocks=integration+verification
   Dependent-ish contract: the second parameter's refinement mentions
   the first parameter ([a : int{ _ = n }]). The definition already
   elaborates today (the predicate resolves [n] as a value reference);
   the obligation lives at the call.
   FINAL: [dep 3 3] accepts (VC [3 = 3] discharged).
   TODAY: rejected at the second argument. *)
let dep (n : int) (a : int{ _ = n }) = a
[%%expect {|
val dep :
  int ->
  int{ (app[Stdlib!.=] _ global[n/297]) } ->
  int{ (app[Stdlib!.=] _ global[n/297]) } = <fun>
|}]

let co_dependent = dep 3 3
[%%expect {|
Line 1, characters 25-26:
1 | let co_dependent = dep 3 3
                             ^
Error: The constant "3" has type "int" but an expression was expected of type
         "int{ (app[Stdlib!.=] _ global[n/297]) }"
|}]

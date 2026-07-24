(* TEST
 flags = "-vox-backend lean";
 expect;
*)

(* Parameter-dependent result refinements: a result predicate that mentions an
   enclosing function parameter.  The subject (the returned expression) and the
   predicate's mention of the parameter must lower to the SAME symbol; otherwise
   the subject occurrence (a bound variable) and the predicate occurrence (an
   opaque reference) become distinct Lean symbols and even [_ = x] with body [x]
   is unprovable.  Under partial correctness the identity claim must verify. *)

let identity (x : int) : int{ _ = x } = x
[%%expect {|
val identity : (x : int) -> int{ _ = x } = <fun>
|}]

(* An inequality result predicate mentioning the parameter also verifies. *)
let at_least (x : int) : int{ _ >= x } = x
[%%expect {|
val at_least : (x : int) -> int{ _ >= x } = <fun>
|}]

(* Contract-argument variant: a parameter's refinement mentions another
   parameter.  The obligation is discharged at the call site, so a matching
   argument verifies and a mismatching one is disproved. *)
let dep (n : int) (a : int{ _ = n }) = a
[%%expect {|
val dep : (n : int) -> int{ _ = n } -> int = <fun>
|}]

let matching = dep 3 3
[%%expect {|
val matching : int = 3
|}]

let mismatching = dep 3 4
[%%expect {|
Line 1, characters 24-25:
1 | let mismatching = dep 3 4
                            ^
Error: Refinement verification failed (disproved)
|}]

(* NEGATIVE: a false parameter-dependent result claim stays disproved.  The fix
   connects identical references; it does not launder distinct values, so [x + 1]
   against predicate [_ = x] yields [v + 1 = v] and is correctly rejected. *)
let wrong (x : int) : int{ _ = x } = x + 1
[%%expect {|
Line 1, characters 37-42:
1 | let wrong (x : int) : int{ _ = x } = x + 1
                                         ^^^^^
Error: Refinement verification failed (disproved)
|}]

(* Dependent-hypothesis (binder-fact) witness.  A parameter [y] whose refinement
   [_ > x] mentions an earlier parameter [x] records a binder fact [y > x] at
   [enter_pattern]; that fact must bind the SAME [x] as the result goal [_ > x].
   This exercises the binder-fact half of the reconciliation specifically.
   Returning [y] verifies (goal [y > x] is exactly the hypothesis). *)
let dependent_ok (x : int) (y : int{ _ > x }) : int{ _ > x } = y
[%%expect {|
val dependent_ok : (x : int) -> int{ _ > x } -> int{ _ > x } = <fun>
|}]

(* Returning [x] is disproved: the goal becomes [x > x], which does not follow
   from the hypothesis [y > x] -- the hypothesis and goal share the same [x], so
   the fix does not spuriously discharge it. *)
let dependent_bad (x : int) (y : int{ _ > x }) : int{ _ > x } = x
[%%expect {|
Line 1, characters 64-65:
1 | let dependent_bad (x : int) (y : int{ _ > x }) : int{ _ > x } = x
                                                                    ^
Error: Refinement verification failed (disproved)
|}]

(* Distinct parameters must NOT be unified: [two] returns [y] but claims its
   result equals [x].  Disproved (they are independent). *)
let two (x : int) (y : int) : int{ _ = x } = y
[%%expect {|
Line 1, characters 45-46:
1 | let two (x : int) (y : int) : int{ _ = x } = y
                                                 ^
Error: Refinement verification failed (not-proved)
|}]

(* A call that CONSUMES a parameter-dependent result fact (exercises the
   check_application result-fact reconciliation for a local callee): [id] has a
   dependent result, and [use] returns [id z] while claiming [_ = z]; the
   recorded fact [id z = z] discharges the goal. *)
let id (x : int) : int{ _ = x } = x
[%%expect {|
val id : (x : int) -> int{ _ = x } = <fun>
|}]

let use (z : int) : int{ _ = z } = id z
[%%expect {|
val use : (z : int) -> int{ _ = z } = <fun>
|}]

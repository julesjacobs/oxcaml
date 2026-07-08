(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* vox: nested refined expressions verified end to end by the solver -- the
   logical-ANF name for a non-nameable argument threads the argument's own
   result refinement, so [f (g x)] proves exactly what [let n = g x in f n]
   does.  Positives verify silently; the negative controls fail closed. *)

let g (x : int) : int{ _ <= x } = refine_ x
let h (x : int) : int{ _ = x + 1 } = refine_ (x + 1)
let consume (y : int{ y <= 10 }) : int = (y :> int)
let fd (y : int) : int{ _ = y + 1 } = refine_ (y + 1)
[%%expect{|
val g : (x : int) -> int{ _ <= x } = <fun>
val h : (x : int) -> int{ _ = x + 1 } = <fun>
val consume : int{ _ <= 10 } -> int = <fun>
val fd : (y : int) -> int{ _ = y + 1 } = <fun>
|}]

(* POSITIVE: nested call into a precondition-only refined parameter. *)
let ok_precond () : int = consume (g 10)
[%%expect{|
val ok_precond : unit -> int = <fun>
|}]

(* POSITIVE: nested call into a dependent-result parameter. *)
let ok_dep () : int{ _ <= 11 } = fd (g 10)
[%%expect{|
val ok_dep : unit -> int{ _ <= 11 } = <fun>
|}]

(* POSITIVE: a chain [g (g 10)] closes by transitivity of the two names. *)
let ok_chain () : int = consume (g (g 10))
[%%expect{|
val ok_chain : unit -> int = <fun>
|}]

(* NEGATIVE: the callee's postcondition does not imply the precondition
   ([g 20 <= 20] does not give [<= 10]); must fail closed. *)
let bad_precond () : int = consume (g 20)
[%%expect{|
Line 1, characters 35-41:
1 | let bad_precond () : int = consume (g 20)
                                       ^^^^^^
Error: vox: verification failed -- goal DISPROVED (a counterexample was validated).
       Goal: *arg* <= 10
Hypotheses:
  *arg* <= 20
Counterexample (validated -- every hypothesis holds and the goal fails here):
  *arg* = 11
|}]

(* NEGATIVE: a wrong exact claim on the dependent result must fail closed
   ([fd (g 10) = 11] would need [g 10 = 10], only [<= 10] is known). *)
let bad_dep () : int{ _ = 11 } = fd (g 10)
[%%expect{|
Line 1, characters 33-42:
1 | let bad_dep () : int{ _ = 11 } = fd (g 10)
                                     ^^^^^^^^^
Error: vox: verification failed -- goal DISPROVED (a counterexample was validated).
       Goal: *unknown10* = 11
Hypotheses:
  *arg* <= 10
  *unknown10* = *arg* + 1
Counterexample (validated -- every hypothesis holds and the goal fails here):
  *unknown10* = 10
  *arg* = 9
|}]

(* POSITIVE: a depth-3 chain [g (g (g 10))] proves end to end -- the innermost
   fact is threaded out of the nested walk (regression for the depth-3
   boundary). *)
let ok_chain3 () : int = consume (g (g (g 10)))
[%%expect{|
val ok_chain3 : unit -> int = <fun>
|}]

(* POSITIVE: a variable-terminated depth-3 chain, whose innermost fact mentions
   a real parameter [n]. *)
let bump (x : int) : int{ _ >= x + 1 } = refine_ (x + 1)
let atleast (y : int{ y >= 0 }) : int = (y :> int)
let ok_var_chain3 (n : int{ n >= 0 }) : int = atleast (bump (bump (bump n)))
[%%expect{|
val bump : (x : int) -> int{ _ >= x + 1 } = <fun>
val atleast : int{ _ >= 0 } -> int = <fun>
val ok_var_chain3 : int{ _ >= 0 } -> int = <fun>
|}]

(* NEGATIVE: a depth-3 chain gives only [<= 10]; a [<= 5] precondition must
   still fail closed (the extra threaded hypotheses are true, so a genuinely
   false goal is unaffected). *)
let bad_chain3 () : int =
  let consume5 (y : int{ y <= 5 }) : int = (y :> int) in
  consume5 (g (g (g 10)))
[%%expect{|
Line 3, characters 11-25:
3 |   consume5 (g (g (g 10)))
               ^^^^^^^^^^^^^^
Error: vox: verification failed -- goal DISPROVED (a counterexample was validated).
       Goal: *arg* <= 5
Hypotheses:
  *arg* <= *arg*#2
  *arg*#2 <= *arg*#3
  *arg*#3 <= 10
Counterexample (validated -- every hypothesis holds and the goal fails here):
  *arg* = 6
  *arg*#2 = 6
  *arg*#3 = 6
|}]

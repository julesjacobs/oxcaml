(* TEST
 flags = "-vox-backend oxsmt";
 expect;
*)

(* What the in-process backend needs of a divisor, and what refusing costs.

   It builds bitvector terms rather than printing them, and its blaster is
   QF_BV: it has no uninterpreted function over bitvectors to send a zero
   divisor to, and a signed division circuit over a symbolic 63-bit divisor
   is beyond what it finishes.  So it needs the divisor's value while the
   term is built, and refuses a division whose divisor is not a constant.

   The cost is a real disagreement with z3, which proves every shape below
   that this file records as refused.  It is fail-closed -- nothing is proved
   that another backend refutes -- but a divisor a refinement shows non-zero
   is the shape the division model exists to serve, and this backend does not
   serve it.  Bounding the circuit rather than refusing it is separate work.

   The verdicts here are this backend's.  Under [-vox-backend z3] all three
   refusals below prove.  Under [-vox-backend lean] none of them does, for a
   different reason: Lean is given [if decide (d <> 0) then d1 / d2 else ...],
   and with a symbolic divisor the [decide] does not reduce and nothing
   instantiates the case split, so the quotient stays behind a branch the
   proof cannot enter.  So z3 is the only backend that serves a symbolic
   divisor at all, and this file's subject is the in-process one. *)

let literal = ((7 / 2) : int{ _ = 3 })
[%%expect {|
val literal : int{ _ = 3 } = 3
|}]

let literal_is_not_vacuous = ((7 / 2) : int{ _ = 4 })
[%%expect {|
Line 1, characters 29-53:
1 | let literal_is_not_vacuous = ((7 / 2) : int{ _ = 4 })
                                 ^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (disproved)
|}]

(* A name bound to the divisor is not the divisor's value: the term the
   verifier sees is a variable with an equation beside it, and an equation is
   not something this backend can build a division circuit from. *)
let through_a_let =
  let divisor = 2 in
  ((7 / divisor) : int{ _ = 3 })
[%%expect {|
Line 3, characters 2-32:
3 |   ((7 / divisor) : int{ _ = 3 })
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

(* Neither is a parameter a refinement pins to a value. *)
let through_a_pinned_parameter (divisor : int{ _ = 2 }) =
  ((7 / divisor) : int{ _ = 3 })
[%%expect {|
Line 2, characters 2-32:
2 |   ((7 / divisor) : int{ _ = 3 })
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

(* And neither is the shape the model exists for: a divisor known only to be
   non-zero.  z3 proves this one. *)
let through_a_non_zero_parameter (divisor : int{ _ <> 0 }) =
  ((divisor / divisor) : int{ _ = 1 })
[%%expect {|
Line 2, characters 2-38:
2 |   ((divisor / divisor) : int{ _ = 1 })
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

(* A zero divisor has no value on any backend, and none of the answers a
   bitvector theory would hand out there is provable. *)
let quotient_at_zero = ((1 / 0) : int{ _ = (-1) })
[%%expect {|
Line 1, characters 23-50:
1 | let quotient_at_zero = ((1 / 0) : int{ _ = (-1) })
                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

let remainder_at_zero = ((5 mod 0) : int{ _ = 5 })
[%%expect {|
Line 1, characters 24-50:
1 | let remainder_at_zero = ((5 mod 0) : int{ _ = 5 })
                            ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

(* Where a zero divisor goes on this backend, and the one property that
   choice rests on.

   There is no value to hand out -- the program raises -- so the term is an
   unconstrained constant of the right sort.  The solver's authors give the
   rule as one constant per call site; this keys it on the operation and the
   two operand terms instead, which is weaker in one direction and stronger
   in another, and both directions are pinned below rather than argued.

   Two occurrences of the SAME division are one expression and have to agree,
   so a constant per occurrence would take [same_division] to not-proved.
   Two occurrences of DIFFERENT divisions must not be made to agree, so one
   constant per operation would take [different_dividends] to proved -- a
   proof relating two programs that both raise.  The wrong answer beside the
   first is the third leg: it fails if the constant never reaches the solver
   at all, which is a state the other two cannot distinguish from
   working, because an obligation nothing decides and an obligation about a
   value nothing constrains both come back not-proved.

   The bodies sit under a parameter so that the toplevel does not evaluate
   them; every one of them raises. *)
let same_division (_unused : int) =
  (((1 / 0) - (1 / 0)) : int{ _ = 0 })
[%%expect {|
val same_division : int -> int{ _ = 0 } = <fun>
|}]

let same_division_is_not_vacuous (_unused : int) =
  (((1 / 0) - (1 / 0)) : int{ _ = 1 })
[%%expect {|
Line 2, characters 2-38:
2 |   (((1 / 0) - (1 / 0)) : int{ _ = 1 })
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (disproved)
|}]

let different_dividends (_unused : int) =
  (((1 / 0) - (2 / 0)) : int{ _ = 0 })
[%%expect {|
Line 2, characters 2-38:
2 |   (((1 / 0) - (2 / 0)) : int{ _ = 0 })
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

let quotient_is_not_remainder (_unused : int) =
  (((1 / 0) - (1 mod 0)) : int{ _ = 0 })
[%%expect {|
Line 2, characters 2-40:
2 |   (((1 / 0) - (1 mod 0)) : int{ _ = 0 })
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

(* The table these constants live in is per-obligation state, rebuilt with
   the session.  Nothing above could observe it being shared across
   obligations directly, but a term carried out of a finished session would
   name a symbol the next one never declared, and the three cases after the
   first would not answer as they do. *)

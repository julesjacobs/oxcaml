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

   The verdicts here are this backend's.  The same file under [-vox-backend
   z3] would prove the three refusals below. *)

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

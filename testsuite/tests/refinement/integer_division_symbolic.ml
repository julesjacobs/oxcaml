(* TEST
 flags = "-vox-backend oxsmt";
 expect;
*)

(* A divisor the verifier cannot see is a literal.

   The in-process backend has no uninterpreted function over bitvectors, so
   it cannot say what the other two say about a division whose divisor might
   be zero: that the result is some arbitrary but fixed value.  It can say
   something weaker that is enough -- an unconstrained constant of the same
   sort -- because only an unsatisfiable query is ever read as an answer,
   and replacing a fixed unknown value by an unconstrained one only admits
   more models.

   The constant is shared by the operands rather than minted per occurrence.
   Two occurrences of the same division are the same expression and a term
   has to equal itself; two divisions with different operands must not be
   related, and are not. *)

let pinned (b : int{ _ = 5 }) = ((10 / b) : int{ _ = 2 })
[%%expect {|
val pinned : int{ _ = 5 } -> int{ _ = 2 } = <fun>
|}]

(* Reaching the divisor is only worth anything if the answer is the
   machine's. *)
let pinned_is_not_vacuous (b : int{ _ = 5 }) = ((10 / b) : int{ _ = 3 })
[%%expect {|
Line 1, characters 47-72:
1 | let pinned_is_not_vacuous (b : int{ _ = 5 }) = ((10 / b) : int{ _ = 3 })
                                                   ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (disproved)
|}]

let pinned_negative (b : int{ _ = (-2) }) = ((7 / b) : int{ _ = (-3) })
[%%expect {|
val pinned_negative : int{ _ = (-2) } -> int{ _ = (-3) } = <fun>
|}]

let pinned_remainder (b : int{ _ = (-2) }) = ((7 mod b) : int{ _ = 1 })
[%%expect {|
val pinned_remainder : int{ _ = (-2) } -> int{ _ = 1 } = <fun>
|}]

(* A divisor that could be anything, including zero, tells us nothing. *)
let unconstrained (b : int) = ((10 / b) : int{ _ = 10 })
[%%expect {|
Line 1, characters 30-56:
1 | let unconstrained (b : int) = ((10 / b) : int{ _ = 10 })
                                  ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

(* Not even the answers a bitvector theory would hand out at a zero
   divisor. *)
let pinned_at_zero (b : int{ _ = 0 }) = ((10 / b) : int{ _ = (-1) })
[%%expect {|
Line 1, characters 40-68:
1 | let pinned_at_zero (b : int{ _ = 0 }) = ((10 / b) : int{ _ = (-1) })
                                            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

let pinned_at_zero_remainder (b : int{ _ = 0 }) = ((10 mod b) : int{ _ = 10 })
[%%expect {|
Line 1, characters 50-78:
1 | let pinned_at_zero_remainder (b : int{ _ = 0 }) = ((10 mod b) : int{ _ = 10 })
                                                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

(* The same division twice is one value, whatever that value is. *)
let one_value (a : int) (b : int) =
  let first = a / b in
  let second = a / b in
  let _ = ((first - second) : int{ _ = 0 }) in
  ()
[%%expect {|
val one_value : int -> int -> unit = <fun>
|}]

let one_value_is_not_vacuous (a : int) (b : int) =
  let first = a / b in
  let second = a / b in
  let _ = ((first - second) : int{ _ = 1 }) in
  ()
[%%expect {|
Line 4, characters 10-43:
4 |   let _ = ((first - second) : int{ _ = 1 }) in
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (disproved)
|}]

(* Two divisions that differ in an operand are two values. *)
let different_divisors (a : int) (b : int) (c : int) =
  let first = a / b in
  let second = a / c in
  let _ = ((first - second) : int{ _ = 0 }) in
  ()
[%%expect {|
Line 4, characters 10-43:
4 |   let _ = ((first - second) : int{ _ = 0 }) in
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

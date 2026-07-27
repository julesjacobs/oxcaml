(* TEST
 flags = "-vox-backend oxsmt";
 expect;
*)

(* Which spellings of a division reach the model, and which do not.

   A call gets a structural image only when the verifier can see that its
   head is the operation it models.  For division that means the modelled
   primitive at its canonical path, saturated, over two integer operands;
   anything else keeps the opaque result a partial call gets, which proves
   nothing rather than proving something about a different operation.  The
   comparison admission beside it was defective twice in exactly these
   shapes, so they are pinned here rather than left to reading. *)

let plain = ((6 / 2) : int{ _ = 3 })
[%%expect {|
val plain : int{ _ = 3 } = 3
|}]

let plain_remainder = ((7 mod 2) : int{ _ = 1 })
[%%expect {|
val plain_remainder : int{ _ = 1 } = 1
|}]

let through_int_module = ((Int.div 6 2) : int{ _ = 3 })
[%%expect {|
val through_int_module : int{ _ = 3 } = 3
|}]

let through_int_rem = ((Int.rem 7 2) : int{ _ = 1 })
[%%expect {|
val through_int_rem : int{ _ = 1 } = 1
|}]

let through_the_qualified_path = ((Stdlib.( / ) 6 2) : int{ _ = 3 })
[%%expect {|
val through_the_qualified_path : int{ _ = 3 } = 3
|}]

(* An ordinary alias is a local value, not the primitive. *)
let quotient = ( / )
let through_a_value = ((quotient 6 2) : int{ _ = 3 })
[%%expect {|
val quotient : int -> int -> int = <fun>
Line 2, characters 22-53:
2 | let through_a_value = ((quotient 6 2) : int{ _ = 3 })
                          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

(* A partial application completed later is not a saturated primitive
   application at the point the verifier looks at it. *)
let halve = ( / ) 6
let through_a_partial_application = ((halve 2) : int{ _ = 3 })
[%%expect {|
val halve : int -> int = <fun>
Line 2, characters 36-62:
2 | let through_a_partial_application = ((halve 2) : int{ _ = 3 })
                                        ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

(* A shadowed operator is whatever it was rebound to.  Reading it as
   division would be the verifier proving something the program does not
   compute. *)
module Shadowed_as_division = struct
  let ( / ) a b = a + b
  let read_as_division = ((6 / 2) : int{ _ = 3 })
end
[%%expect {|
Line 3, characters 25-49:
3 |   let read_as_division = ((6 / 2) : int{ _ = 3 })
                             ^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

(* And the answer it does compute is not provable either: the rebound
   function is an ordinary local value, so nothing is known about it. *)
module Shadowed_as_itself = struct
  let ( / ) a b = a + b
  let read_as_itself = ((6 / 2) : int{ _ = 8 })
end
[%%expect {|
Line 3, characters 23-47:
3 |   let read_as_itself = ((6 / 2) : int{ _ = 8 })
                           ^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

(* An external declared elsewhere with the same primitive name is a
   different operation, whatever it was given for a type. *)
external elsewhere : int -> int -> int = "%divint"
let through_an_alias = ((elsewhere 6 2) : int{ _ = 3 })
[%%expect {|
external elsewhere : int -> int -> int = "%divint"
Line 2, characters 23-55:
2 | let through_an_alias = ((elsewhere 6 2) : int{ _ = 3 })
                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

(* The same, declared total by its author.  A user saying an operation is
   total does not make it the one the model describes. *)
external declared_total : int -> int -> int @@ total = "%divint"
let through_a_total_alias = ((declared_total 6 2) : int{ _ = 3 })
[%%expect {|
external declared_total : int -> int -> int = "%divint"
Line 2, characters 28-65:
2 | let through_a_total_alias = ((declared_total 6 2) : int{ _ = 3 })
                                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

(* A carrier the bitvector model does not describe.  Reading a float as a
   bitvector quotient is the shape of the float-comparison defect already on
   record.  What is pinned here is that nothing is proved; which refusal it
   is comes from the float carrier having no model at all, and may change
   without this shape becoming provable. *)
external float_quotient : float -> float -> float = "%divint"
let through_a_float_carrier =
  ((float_quotient 6.0 2.0) : float{ _ = 3.0 })
[%%expect {|
external float_quotient : float -> float -> float = "%divint"
Line 3, characters 2-47:
3 |   ((float_quotient 6.0 2.0) : float{ _ = 3.0 })
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (solver-error)
|}]

(* Labelled arguments, applied in the order they were declared and in the
   other order. *)
external labelled : dividend:int -> divisor:int -> int = "%divint"
[%%expect {|
external labelled : dividend:int -> divisor:int -> int = "%divint"
|}]

let through_labels = ((labelled ~dividend:6 ~divisor:2) : int{ _ = 3 })
[%%expect {|
Line 1, characters 21-71:
1 | let through_labels = ((labelled ~dividend:6 ~divisor:2) : int{ _ = 3 })
                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

let through_reordered_labels =
  ((labelled ~divisor:2 ~dividend:6) : int{ _ = 3 })
[%%expect {|
Line 2, characters 2-52:
2 |   ((labelled ~divisor:2 ~dividend:6) : int{ _ = 3 })
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

(* Application written through [@@] and [|>].  Whatever these become after
   typing, the answer must be the machine's or nothing. *)
let through_at = (((/) 6 @@ 2) : int{ _ = 3 })
let through_pipe = ((2 |> (/) 6) : int{ _ = 3 })
[%%expect {|
val through_at : int{ _ = 3 } = 3
val through_pipe : int{ _ = 3 } = 3
|}]

(* Reaching the model is only worth anything if it is the machine's answer
   that comes out of it. *)
let through_at_is_not_vacuous = (((/) 6 @@ 2) : int{ _ = 4 })
[%%expect {|
Line 1, characters 32-61:
1 | let through_at_is_not_vacuous = (((/) 6 @@ 2) : int{ _ = 4 })
                                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (disproved)
|}]

(* An operand that is not a value.  The call is replaced by an opaque result
   before it becomes an operand, so the quotient is not a constant. *)
let counter = ref 0
let effectful () = incr counter; 6
let through_an_effectful_operand = ((effectful () / 2) : int{ _ = 3 })
[%%expect {|
val counter : int ref = {contents = 0}
val effectful : unit -> int = <fun>
Line 3, characters 35-70:
3 | let through_an_effectful_operand = ((effectful () / 2) : int{ _ = 3 })
                                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

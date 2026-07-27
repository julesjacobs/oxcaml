(* TEST
 expect;
*)

(* Termination by a [@vox.decreases] measure: a lexicographically ordered
   tuple of integer expressions over a recursive function's parameters.

   Ordinary [int] is a signed 63-bit bitvector, so a value getting smaller is
   not by itself progress -- one less than the minimum is the maximum.  The
   obligation at each recursive call therefore asks, of the position that
   descends, both that it is strictly smaller in the same machine arithmetic
   the program runs in, and that it is at or above zero.  Each expectation
   below is written from what that obligation has to mean, not from what the
   compiler happened to print. *)

(* Expect: accepted.  [n >= 0] and [n <> 0] give [n >= 1], so [n - 1] does
   not wrap and lands in the naturals. *)
let[@vox.decreases n] rec countdown (n : int{ _ >= 0 }) : int =
  if n = 0 then 0 else countdown (n - 1)

[%%expect {|
val countdown : int{ _ >= 0 } -> int = <fun>
|}]

(* That the measure is what pays for totality cannot be witnessed here: in
   this harness even an ordinary [fun x -> x + 1] is reported partial where a
   total value is wanted, so a toplevel test would say nothing either way.
   It is witnessed in batch by [decreases_totality_batch.ml], which compiles
   the binding above against a total-argument function and compiles the same
   body without the measure beside it. *)

(* Expect: refused.  The same body without the lower bound on [n]: at a
   negative [n] the measure leaves the naturals, so nothing stops it
   descending forever. *)
module Unbounded_below : sig end = struct
  let[@vox.decreases n] rec countdown (n : int) : int =
    if n = 0 then 0 else countdown (n - 1)
end

[%%expect {|
Line 3, characters 25-42:
3 |     if n = 0 then 0 else countdown (n - 1)
                             ^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

(* Expect: refused.  The measure is bounded but the call does not move it. *)
module Does_not_decrease : sig end = struct
  let[@vox.decreases n] rec spin (n : int{ _ >= 0 }) : int =
    if n = 0 then 0 else spin n
end

[%%expect {|
Line 3, characters 25-31:
3 |     if n = 0 then 0 else spin n
                             ^^^^^^
Error: Refinement verification failed (disproved)
|}]

(* Expect: refused.  [0 - n] descends as mathematics but not as machine
   arithmetic: at the minimum, negation returns the minimum, so the caller's
   measure is the smallest value there is and the callee's is the largest. *)
module Subtraction_wraps : sig end = struct
  let[@vox.decreases 0 - n] rec up (n : int{ _ <= 0 }) : int =
    if n = 0 then 0 else up (n + 1)
end

[%%expect {|
Line 3, characters 25-35:
3 |     if n = 0 then 0 else up (n + 1)
                             ^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

(* Expect: accepted.  The same measure, once the parameter is held away from
   the machine minimum so the negation cannot wrap. *)
let[@vox.decreases 0 - n] rec up (n : int{ _ <= 0 && _ > min_int }) : int =
  if n = 0 then 0 else up (n + 1)

[%%expect {|
val up : int{ _ <= 0 && _ > min_int } -> int = <fun>
|}]

(* Expect: accepted.  A lexicographic pair whose first component stands still
   while the second descends, and whose first component descends when the
   second is reset upwards. *)
let[@vox.decreases m, n] rec walk (m : int{ _ >= 0 }) (n : int{ _ >= 0 })
    : int =
  if n = 0 then (if m = 0 then 0 else walk (m - 1) 5)
  else walk m (n - 1)

[%%expect {|
val walk : int{ _ >= 0 } -> int{ _ >= 0 } -> int = <fun>
|}]

(* Expect: refused.  The second component descends but the first one grows,
   and a lexicographic tuple is read from the front. *)
module Leading_component_grows : sig end = struct
  let[@vox.decreases m, n] rec walk (m : int) (n : int{ _ >= 0 }) : int =
    if n = 0 then 0 else walk (m + 1) (n - 1)
end

[%%expect {|
Line 3, characters 25-45:
3 |     if n = 0 then 0 else walk (m + 1) (n - 1)
                             ^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (disproved)
|}]

(* Expect: accepted.  Mutual recursion descends on the measure the whole
   group shares; each call compares the callee's measure at its arguments
   against the caller's at its parameters. *)
let[@vox.decreases n] rec even (n : int{ _ >= 0 }) : bool =
  if n = 0 then true else odd (n - 1)
and[@vox.decreases n] odd (n : int{ _ >= 0 }) : bool =
  if n = 0 then false else even (n - 1)

[%%expect {|
val even : int{ _ >= 0 } -> bool = <fun>
val odd : int{ _ >= 0 } -> bool = <fun>
|}]

(* Expect: refused.  One direction of the mutual group stands still. *)
module Mutual_does_not_decrease : sig end = struct
  let[@vox.decreases n] rec even (n : int{ _ >= 0 }) : bool =
    if n = 0 then true else odd n
  and[@vox.decreases n] odd (n : int{ _ >= 0 }) : bool =
    if n = 0 then false else even (n - 1)
end

[%%expect {|
Line 3, characters 28-33:
3 |     if n = 0 then true else odd n
                                ^^^^^
Error: Refinement verification failed (disproved)
|}]

(* Expect: accepted.  A mutual group whose members take different numbers of
   parameters, and whose lower bound comes from the guard on the path to the
   call rather than from a refinement on the parameter. *)
let[@vox.decreases n] rec one (n : int) (k : int) : int =
  if n <= 0 then k else two (n - 1) k 7
and[@vox.decreases a] two (a : int) (b : int) (c : int) : int =
  if a <= 0 then b + c else one (a - 1) (b + c)

[%%expect {|
val one : int -> int -> int = <fun>
val two : int -> int -> int -> int = <fun>
|}]

(* Expect: refused, and this is the case that fixes where the obligation
   sits.  A result refinement of [false] is exactly what a call that never
   returns establishes, and the body below is accepted without a measure for
   that reason.  If the call's own result fact were in scope while proving
   that the call descends, the measure would be proved from the assumption
   that the call terminates, and this binding would be accepted as total.
   The obligation is emitted first, so it is refused. *)
module Diverging_result_contract : sig end = struct
  let rec accepted_without_a_measure (n : int) : int{ false } =
    accepted_without_a_measure (n + 1)

  let[@vox.decreases n] rec bogus (n : int) : int{ false } = bogus (n + 1)
end

[%%expect {|
Line 5, characters 61-74:
5 |   let[@vox.decreases n] rec bogus (n : int) : int{ false } = bogus (n + 1)
                                                                 ^^^^^^^^^^^^^
Error: Refinement verification failed (disproved)
|}]

(* Expect: accepted.  A recursive call under a nested lambda is still a call
   whose arguments the measure can be stated over; termination is an
   induction on the argument, so where the call is written does not matter as
   long as its measure descends. *)
let[@vox.decreases n] rec nested (n : int{ _ >= 0 }) : int =
  if n = 0 then 0 else (let step () = nested (n - 1) in step ())

[%%expect {|
val nested : int{ _ >= 0 } -> int = <fun>
|}]

(* Expect: accepted.  A recursive call in a guard. *)
let[@vox.decreases n] rec guarded (n : int{ _ >= 0 }) : int =
  match n with
  | m when m > 0 && guarded (m - 1) >= 0 -> 1
  | _ -> 0

[%%expect {|
val guarded : int{ _ >= 0 } -> int = <fun>
|}]

(* Expect: accepted.  Labelled arguments are supplied in an order other than
   the one the parameters are written in.  The obligation is stated over the
   parameter the measure names, not over whichever argument comes first at
   the call, so this descends. *)
let[@vox.decreases n] rec labelled ~(n : int{ _ >= 0 }) ~(k : int) : int =
  if n = 0 then k else labelled ~k ~n:(n - 1)

[%%expect {|
val labelled : n:int{ _ >= 0 } -> k:int -> int = <fun>
|}]

(* Expect: refused.  The same call measured on the parameter that stands
   still.  Together with the previous case this pins the argument-to-
   parameter correspondence in both directions. *)
module Labelled_wrong_position : sig end = struct
  let[@vox.decreases k] rec labelled ~(n : int{ _ >= 0 }) ~(k : int{ _ >= 0 })
      : int =
    if n = 0 then k else labelled ~k ~n:(n - 1)
end

[%%expect {|
Line 4, characters 25-47:
4 |     if n = 0 then k else labelled ~k ~n:(n - 1)
                             ^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (disproved)
|}]

(* Expect: refused.  A measure has to be an expression the verifier can
   evaluate at every activation, so it may not call a partial function. *)
module Partial_measure : sig end = struct
  let half (x : int) = x / 2

  let[@vox.decreases half n] rec f (n : int{ _ >= 0 }) : int =
    if n = 0 then 0 else f (n - 2)
end

[%%expect {|
Line 4, characters 21-25:
4 |   let[@vox.decreases half n] rec f (n : int{ _ >= 0 }) : int =
                         ^^^^
Error: The value "half" is "partial"
       but is expected to be "total"
         because it is used in an expression (at line 4, characters 21-27).
|}]

(* Expect: accepted.  A partial operation in the body is no obstacle to
   stating a measure; it only stops the function being total, which
   [decreases_totality_batch.ml] witnesses. *)
let[@vox.decreases n] rec halves (n : int{ _ >= 0 }) : int =
  if n = 0 then 0 else (n / 2) + halves (n - 1)

[%%expect {|
val halves : int{ _ >= 0 } -> int = <fun>
|}]

(* Expect: accepted.  A measured group written in expression position rather
   than at the top of a structure. *)
let local_descends () =
  let[@vox.decreases n] rec countdown (n : int{ _ >= 0 }) : int =
    if n = 0 then 0 else countdown (n - 1)
  in
  countdown 3

[%%expect {|
val local_descends : unit -> int = <fun>
|}]

(* Expect: refused.  The same position, with a call that does not descend.
   Obligations for a group in expression position come from a different walk
   site than one at the top of a structure, and during development that site
   did not install the measure, so a group written here was granted totality
   with nothing proved about it. *)
module Local_group_does_not_decrease : sig end = struct
  let outer () =
    let[@vox.decreases n] rec spin (n : int{ _ >= 0 }) : int =
      if n = 0 then 0 else spin n
    in
    spin 3
end

[%%expect {|
Line 4, characters 27-33:
4 |       if n = 0 then 0 else spin n
                               ^^^^^^
Error: Refinement verification failed (disproved)
|}]

(* Eligibility.  The checks below decide whether a measure can be stated at
   all; they run in the typing phase, so they hold under [-vox-type-only] and
   on the editor's typing path as well as in a full compile. *)

(* Expect: refused.  Nothing recurses, so there is nothing to measure. *)
module Not_recursive : sig end = struct
  let[@vox.decreases n] plain (n : int) : int = n
end

[%%expect {|
Line 2, characters 5-23:
2 |   let[@vox.decreases n] plain (n : int) : int = n
         ^^^^^^^^^^^^^^^^^^
Error: vox: [@vox.decreases] applies to a recursive binding; this one is not recursive
|}]

(* Expect: refused.  A group descends on one order, so a measure on one half
   of it says nothing about the other. *)
module Half_a_group : sig end = struct
  let[@vox.decreases n] rec even (n : int) : bool =
    if n = 0 then true else odd (n - 1)
  and odd (n : int) : bool =
    if n = 0 then false else even (n - 1)
end

[%%expect {|
Line 2, characters 5-23:
2 |   let[@vox.decreases n] rec even (n : int) : bool =
         ^^^^^^^^^^^^^^^^^^
Error: vox: [@vox.decreases] must be given on every binding of a mutually recursive group
|}]

(* Expect: refused.  Two tuples of different lengths have no lexicographic
   order between them. *)
module Ragged_group : sig end = struct
  let[@vox.decreases n, n] rec even (n : int) : bool =
    if n = 0 then true else odd (n - 1)
  and[@vox.decreases n] odd (n : int) : bool =
    if n = 0 then false else even (n - 1)
end

[%%expect {|
Line 2, characters 5-26:
2 |   let[@vox.decreases n, n] rec even (n : int) : bool =
         ^^^^^^^^^^^^^^^^^^^^^
Error: vox: [@vox.decreases] must give the same number of components on every binding of a mutually recursive group
|}]

(* Expect: refused.  No measure was written. *)
module Empty_measure : sig end = struct
  let[@vox.decreases] rec loop (n : int) : int =
    if n = 0 then 0 else loop (n - 1)
end

[%%expect {|
Line 2, characters 5-21:
2 |   let[@vox.decreases] rec loop (n : int) : int =
         ^^^^^^^^^^^^^^^^
Error: vox: [@vox.decreases] expects a measure expression, or a comma-separated tuple of them
|}]

(* Expect: refused.  A measure has to be an integer. *)
module Not_an_integer : sig end = struct
  let[@vox.decreases f] rec loop (f : int -> int) (n : int) : int =
    if n = 0 then 0 else loop f (n - 1)
end

[%%expect {|
Line 2, characters 21-22:
2 |   let[@vox.decreases f] rec loop (f : int -> int) (n : int) : int =
                         ^
Error: The value "f" has type "int -> int" but an expression was expected of type
         "int"
|}]

(* Expect: refused.  A recursive name reached other than as a saturated call
   has no argument tuple for the obligation to compare against the
   parameters. *)
module Passed_as_a_value : sig end = struct
  let[@vox.decreases n] rec loop (n : int{ _ >= 0 }) : int =
    if n = 0 then 0
    else List.fold_left (fun a b -> a + b) 0 (List.map loop [n - 1])
end

[%%expect {|
Line 2, characters 5-23:
2 |   let[@vox.decreases n] rec loop (n : int{ _ >= 0 }) : int =
         ^^^^^^^^^^^^^^^^^^
Error: vox: [@vox.decreases] requires every occurrence of a name in the recursive group to be a direct call supplying all of its parameters
|}]

(* Expect: refused.  Likewise for a partial application: the closure it
   builds is called somewhere the obligation cannot see. *)
module Partial_application : sig end = struct
  let[@vox.decreases n] rec loop (n : int{ _ >= 0 }) (k : int) : int =
    if n = 0 then k else (loop (n - 1)) k
end

[%%expect {|
Line 2, characters 5-23:
2 |   let[@vox.decreases n] rec loop (n : int{ _ >= 0 }) (k : int) : int =
         ^^^^^^^^^^^^^^^^^^
Error: vox: [@vox.decreases] requires every occurrence of a name in the recursive group to be a direct call supplying all of its parameters
|}]

(* Expect: refused.  [@vox.def] generates a trusted equation from the body it
   reads, and reads it on the strength of structural recursion; a measure is
   deliberately not accepted in its place. *)
module Def_stays_structural : sig end = struct
  let[@vox.def] [@vox.decreases n] rec dbl (n : int{ _ >= 0 }) : int =
    if n = 0 then 0 else 2 + dbl (n - 1)
end

[%%expect {|
Lines 2-3, characters 2-40:
2 | ..let[@vox.def] [@vox.decreases n] rec dbl (n : int{ _ >= 0 }) : int =
3 |     if n = 0 then 0 else 2 + dbl (n - 1)
Error: vox: [@vox.def] cannot be used on this recursive binding: its recursive group is not structurally total
|}]

(* Expect: accepted.  A local binding that shadows a group name is not a
   recursive occurrence. *)
let[@vox.decreases n] rec shadowed (n : int{ _ >= 0 }) : int =
  if n = 0 then 0
  else
    let shadowed = 3 in
    shadowed + 0

[%%expect {|
val shadowed : int{ _ >= 0 } -> int = <fun>
|}]

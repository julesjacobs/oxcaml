(* TEST
 expect;
*)

(* Ackermann's function, whose recursion no structural argument reaches: the
   first argument falls but the second is reset upwards, and one of the calls
   passes a recursive result as an argument.  A lexicographic pair of the two
   arguments is the measure that fits, and it is the smallest example in
   which the lexicographic reading does real work.

   Two things about machine integers show up here.  Both parameters are held
   at or above zero, because a descent from [m] to [m - 1] is only a descent
   while [m] is positive: at the machine minimum, subtracting one gives the
   maximum.  And the base case saturates, returning the maximum unchanged
   rather than wrapping past it, so the result really is non-negative for
   every input rather than only for the small ones.

   The outer call is the interesting obligation.  Its second argument is
   [ack m (n - 1)], a value this function computes, but the pair descends on
   its first component, so nothing has to be known about the second.  That is
   why the lower bound is asked of the component that descends rather than of
   the whole tuple: asking it of the whole tuple would need [ack]'s result to
   be bounded by something, and Ackermann's result is bounded by nothing that
   fits in a machine integer. *)

let[@vox.decreases m, n] rec ack (m : int{ _ >= 0 }) (n : int{ _ >= 0 })
    : int{ _ >= 0 } =
  if m = 0 then (if n = max_int then n else n + 1)
  else if n = 0 then ack (m - 1) 1
  else ack (m - 1) (ack m (n - 1))

[%%expect {|
val ack : int{ _ >= 0 } -> int{ _ >= 0 } -> int{ _ >= 0 } = <fun>
|}]

(* The measure is what makes it total: an integer recursion is not structural,
   so without one this binding could not be passed where a total function is
   wanted.  That is witnessed in batch, by
   [refinement/decreases_totality_batch.ml]; this harness reports even an
   ordinary [fun x -> x + 1] as partial where a total value is wanted, so a
   toplevel witness would say nothing either way. *)

(* Small values, computed rather than asserted: [ack 2 3] is 9 and
   [ack 3 3] is 61. *)
let () =
  if ack 0 0 <> 1 then failwith "ack 0 0";
  if ack 1 1 <> 3 then failwith "ack 1 1";
  if ack 2 3 <> 9 then failwith "ack 2 3";
  if ack 3 3 <> 61 then failwith "ack 3 3"

[%%expect {|
|}]

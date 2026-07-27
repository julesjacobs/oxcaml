(* TEST
 expect;
*)

(* Ackermann's function, whose recursion no structural argument reaches: the
   first argument falls but the second is reset upwards, and one of the calls
   passes a recursive result as an argument.  A lexicographic pair of the two
   arguments is the measure that fits, and it is the smallest example in which
   the lexicographic reading does real work.

   It is written on [Bigint], the mathematical integers, because Ackermann's
   values leave the machine integers almost immediately -- A(4, 2) has 19729
   digits -- and an example titled Ackermann's function should compute
   Ackermann's function.

   The measure descends in the same arithmetic.  Both arguments are held at or
   above zero, and on [Bigint] that lower bound is there for the oldest reason
   there is: the naturals are well-founded and the integers are not.  Nothing
   about machine widths enters into it.  Take the bound away and the recursion
   is refused, because [n - 1] below zero descends forever.

   The outer call is the interesting obligation.  Its second argument is
   [ack m (n - 1)], a value this function computes, but the pair descends on
   its first component, so nothing has to be known about the second.  That is
   why the lower bound is asked of the component that descends rather than of
   the whole tuple: asking it of the whole tuple would need [ack]'s result to
   be bounded, and Ackermann's result is bounded by nothing. *)

let[@vox.decreases m, n] rec ack
    (m : Bigint.t{ Bigint.ge _ Bigint.zero })
    (n : Bigint.t{ Bigint.ge _ Bigint.zero })
  : Bigint.t{ Bigint.ge _ Bigint.zero } =
  if Bigint.is_zero m then Bigint.add n Bigint.one
  else if Bigint.is_zero n then ack (Bigint.sub m Bigint.one) Bigint.one
  else ack (Bigint.sub m Bigint.one) (ack m (Bigint.sub n Bigint.one))

[%%expect {|
val ack :
  Bigint.t{ Bigint.ge _ Bigint.zero } ->
  Bigint.t{ Bigint.ge _ Bigint.zero } -> Bigint.t{ Bigint.ge _ Bigint.zero } =
  <fun>
|}]

(* Computed rather than asserted: A(0,0) is 1, A(1,1) is 3, A(2,3) is 9 and
   A(3,3) is 61.  These are the real values, not values that happen to agree
   with the real ones below some threshold. *)
let () =
  let check (a : int{ _ >= 0 }) (b : int{ _ >= 0 }) expected =
    if
      not
        (Bigint.equal
           (ack (Bigint.of_int a) (Bigint.of_int b))
           (Bigint.of_int expected))
    then failwith (Printf.sprintf "ack %d %d" a b)
  in
  check 0 0 1;
  check 1 1 3;
  check 2 3 9;
  check 3 3 61

[%%expect {|
|}]

(* The same recursion on machine integers, kept because it is where the other
   half of the obligation does visible work, and named for what it actually
   computes.  It is not Ackermann's function: past roughly A(3, .) the values
   are wrong, because the base case saturates at the maximum rather than
   wrapping past it.  What it demonstrates is that the recursion terminates
   and returns a non-negative number in an arithmetic where one less than the
   minimum is the maximum.

   Remove the saturation and the compiler refuses at [n + 1], correctly: at
   the maximum it wraps negative and the declared result stops holding.  That
   refusal is the machine-integer half of the same lower bound the Bigint
   version above needs for a different reason. *)
let[@vox.decreases m, n] rec saturating_ackermann_shape
    (m : int{ _ >= 0 }) (n : int{ _ >= 0 }) : int{ _ >= 0 } =
  if m = 0 then (if n = max_int then n else n + 1)
  else if n = 0 then saturating_ackermann_shape (m - 1) 1
  else
    saturating_ackermann_shape (m - 1)
      (saturating_ackermann_shape m (n - 1))

[%%expect {|
val saturating_ackermann_shape :
  int{ _ >= 0 } -> int{ _ >= 0 } -> int{ _ >= 0 } = <fun>
|}]

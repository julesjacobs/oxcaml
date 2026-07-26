(* Recursion, proved by induction.

   A recursive call's refined result is the induction hypothesis.  In
   [fib] each recursive result is [_ >= 0], and the sum of two
   nonnegatives is nonnegative, so the result contract holds.  The
   argument contract [n >= 0] rides each recursive call. *)

let rec fib (n : int{ _ >= 0 }) : int{ _ >= 0 } =
  if n <= 1 then n else fib (n - 1) + fib (n - 2)

(* Triangular sum: the same induction, a cleaner witness. *)
let rec sum_to (n : int{ _ >= 0 }) : int{ _ >= 0 } =
  if n = 0 then 0 else n + sum_to (n - 1)

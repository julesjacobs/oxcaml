(* A total function must terminate. A recursive call on a structurally
   smaller argument needs no annotation; otherwise the function states a
   measure with [@@decreases]: an expression that stays nonnegative and
   decreases at every recursive call. *)

(* Euclid's algorithm: [b] decreases, since [a mod b < b]. *)
let rec (gcd @ total) (a : {a : int | a >= 0}) (b : {b : int | b >= 0}) :
    {g : int | g >= 0} =
  if b = 0 then a else gcd b Int.Refined.(a mod b)
[@@decreases b]

(* Binary search for the first index in [lo, hi) where [f] holds, assuming
   [f] is monotone: the width [hi - lo] decreases. *)
let rec (search @ total) (f : int -> bool) (lo : {l : int | l >= 0})
    (hi : {h : int | lo <= h}) : {r : int | lo <= r && r <= hi} =
  if lo = hi then lo
  else
    let mid = lo + Int.Refined.((hi - lo) / 2) in
    if f mid then search f lo mid else search f (mid + 1) hi
[@@decreases hi - lo]

(* Rejected: nobody knows a measure for the Collatz iteration, and [n] is
   not one. *)
let rec (collatz_steps @ total) (n : {n : int | n >= 1}) : int =
  if n = 1 then 0
  else if Int.Refined.(n mod 2) = 0 then 1 + collatz_steps Int.Refined.(n / 2)
  else 1 + collatz_steps (3 * n + 1)
[@@decreases n]

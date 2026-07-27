(* TEST
 setup-ocamlc.byte-build-env;
 flags = "-vox-backend z3 -c";
 ocamlc.byte;
 flags = "-vox-backend oxsmt -c";
 ocamlc.byte;
 flags = "-vox-backend lean -c";
 ocamlc.byte;
*)

(* What the machine computes for a division, asked of each backend in turn.
   Asking all three is the point: the SMT-LIB text, the in-process oxsmt
   terms and the Lean term are three separate spellings of the same
   operation, and a change to one does not reach the others.

   OCaml truncates towards zero, so a negative dividend rounds up, not down,
   and the remainder carries the dividend's sign rather than the divisor's.
   [min_int / (-1)] does not raise: the quotient is out of range and wraps
   back to [min_int], and the remainder is zero. *)

let exact = ((6 / 2) : int{ _ = 3 })
let truncates_towards_zero = (((-7) / 2) : int{ _ = (-3) })
let truncates_the_other_way = ((7 / (-2)) : int{ _ = (-3) })
let two_negatives = (((-7) / (-2)) : int{ _ = 3 })

let remainder_follows_the_dividend = (((-7) mod 2) : int{ _ = (-1) })
let remainder_ignores_the_divisor = ((7 mod (-2)) : int{ _ = 1 })

let quotient_wraps = ((min_int / (-1)) : int{ _ = min_int })
let remainder_at_the_wrap = ((min_int mod (-1)) : int{ _ = 0 })

let at_the_top = ((max_int / 2) : int{ _ = 2305843009213693951 })
let at_the_bottom = ((min_int / 2) : int{ _ = (-2305843009213693952) })

(* The same operation reached through [Int]. *)
let through_int_div = ((Int.div 6 2) : int{ _ = 3 })
let through_int_rem = ((Int.rem 7 2) : int{ _ = 1 })

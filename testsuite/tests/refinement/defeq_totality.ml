(* TEST
 expect;
*)

(* Part 1 of definitional equations: integer [/] and [mod] are PARTIAL in the
   totality mode, because they trap on a zero divisor, mirroring how [raise],
   [List.hd] and array indexing are already partial.  A function using [/] or
   [mod] therefore cannot be [@ total], and, since a refinement predicate is
   checked at [total], cannot appear in a predicate either.

   The zero divisor is now the whole of the reason.  Away from it the logic's
   division is the machine's: the backends model the truncating quotient and
   the remainder that carries the dividend's sign, so a division in subject
   position can be proved about even though it cannot be written in a
   predicate.  See [integer_division.ml]. *)

let expects_total (f @ total) = f
[%%expect {|
val expects_total : 'a @ total -> 'a = <fun>
|}]

(* Rejected: integer division is partial. *)
let div_is_partial = expects_total (fun x -> 100 / x)
[%%expect {|
Line 1, characters 49-50:
1 | let div_is_partial = expects_total (fun x -> 100 / x)
                                                     ^
Error: The value "(/)" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 35-53
         which is expected to be "total".
|}]

(* Rejected: integer modulus is partial. *)
let mod_is_partial = expects_total (fun x -> 100 mod x)
[%%expect {|
Line 1, characters 49-52:
1 | let mod_is_partial = expects_total (fun x -> 100 mod x)
                                                     ^^^
Error: The value "\#mod" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 35-55
         which is expected to be "total".
|}]

(* Accepted: addition and multiplication are total. *)
let arith_is_total = expects_total (fun x -> (x + x) * 2)
[%%expect {|
val arith_is_total : int -> int = <fun>
|}]

(* Accepted: the bitwise operators remain total. *)
let bitand_is_total = expects_total (fun x -> x land 1)
[%%expect {|
val bitand_is_total : int -> int = <fun>
|}]

(* Contrast: [raise] and [List.hd] were ALREADY partial (unchanged). *)
let raise_is_partial = expects_total (fun () -> raise Not_found)
[%%expect {|
Line 1, characters 48-53:
1 | let raise_is_partial = expects_total (fun () -> raise Not_found)
                                                    ^^^^^
Error: The value "raise" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 37-64
         which is expected to be "total".
|}]

let hd_is_partial = expects_total (fun l -> List.hd l)
[%%expect {|
Line 1, characters 44-51:
1 | let hd_is_partial = expects_total (fun l -> List.hd l)
                                                ^^^^^^^
Error: The value "List.hd" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 34-54
         which is expected to be "total".
|}]

(* A refinement predicate using [mod] is now rejected at totality. *)
type even = int{ _ mod 2 = 0 }
[%%expect {|
Line 1, characters 19-22:
1 | type even = int{ _ mod 2 = 0 }
                       ^^^
Error: The value "\#mod" is "partial"
       but is expected to be "total"
         because it is used in an expression (at line 1, characters 12-30).
|}]

(* A refinement predicate using [/] is now rejected at totality. *)
type third = int{ _ / 3 = 1 }
[%%expect {|
Line 1, characters 20-21:
1 | type third = int{ _ / 3 = 1 }
                        ^
Error: The value "(/)" is "partial"
       but is expected to be "total"
         because it is used in an expression (at line 1, characters 13-29).
|}]

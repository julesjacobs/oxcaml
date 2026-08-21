(* TEST
 expect;
*)

(* Mode annotations on [Stdlib.Bigint] (design-docs/bigint.md, dated section
   2026-08-21).

   [Bigint.t] is [immutable_data], so it crosses both totality and
   logicality: Bigint VALUES are admissible in total and logical contexts
   with no annotation.  What these tests pin is the totality of the
   OPERATIONS, which is what lets a specification call them. *)

(* Values cross: a [Bigint.t] is usable under a total closure. *)
let use_value @ total = fun (x : Bigint.t) -> x
[%%expect{|
val use_value : Bigint.t -> Bigint.t = <fun>
|}]

(* A logical [Bigint.t] is usable where a physical one is expected. *)
let cross_logicality (x : Bigint.t @ logical) : Bigint.t = x
[%%expect{|
val cross_logicality : Bigint.t @ logical -> Bigint.t = <fun>
|}]

(* The arithmetic operations are total callees. *)
let arithmetic @ total = fun x y -> Bigint.(mul (add x y) (sub x (neg y)))
[%%expect{|
Line 1, characters 44-47:
1 | let arithmetic @ total = fun x y -> Bigint.(mul (add x y) (sub x (neg y)))
                                                ^^^
Error: The value "mul" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 25-74
         which is expected to be "total".
|}]

(* The comparison operations, [is_zero], [abs] and [of_int] are total
   callees. *)
let comparisons @ total =
  fun x y ->
    Bigint.(equal x y, lt x y, le x y, gt x y, ge x y, compare x y,
            is_zero (abs x), of_int 7)
[%%expect{|
Line 3, characters 12-17:
3 |     Bigint.(equal x y, lt x y, le x y, gt x y, ge x y, compare x y,
                ^^^^^
Error: The value "equal" is "partial"
       but is expected to be "total"
         because it is used inside the function at lines 2-4, characters 2-38
         which is expected to be "total".
|}]

(* The runtime-only conversions are partial: [of_string] raises on
   malformed input, and none of the three has a solver interpretation, so
   none of them belongs in a specification.  These pin that the totality
   claims are scoped to the interpreted operations. *)
let runtime_only_of_string @ total = fun s -> Bigint.of_string s
[%%expect{|
Line 1, characters 46-62:
1 | let runtime_only_of_string @ total = fun s -> Bigint.of_string s
                                                  ^^^^^^^^^^^^^^^^
Error: The value "Bigint.of_string" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 37-64
         which is expected to be "total".
|}]

let runtime_only_to_string @ total = fun x -> Bigint.to_string x
[%%expect{|
Line 1, characters 46-62:
1 | let runtime_only_to_string @ total = fun x -> Bigint.to_string x
                                                  ^^^^^^^^^^^^^^^^
Error: The value "Bigint.to_string" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 37-64
         which is expected to be "total".
|}]

let runtime_only_to_int_opt @ total = fun x -> Bigint.to_int_opt x
[%%expect{|
Line 1, characters 47-64:
1 | let runtime_only_to_int_opt @ total = fun x -> Bigint.to_int_opt x
                                                   ^^^^^^^^^^^^^^^^^
Error: The value "Bigint.to_int_opt" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 38-66
         which is expected to be "total".
|}]

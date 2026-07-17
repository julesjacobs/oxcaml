(* TEST
 expect;
*)

(* Part 2 of definitional equations.  [let[@vox.def] f p ... = rhs] forces
   [f @ total] and generates a companion trusted-lemma binding [f_def] whose
   refinement asserts [f]'s definitional equation [f p ... = rhs].  [f] itself
   stays an uninterpreted solver symbol -- nothing about its body reaches the
   solver except through [f_def].  Writing [let () = f_def a ...] deposits the
   ground equation [f a ... = rhs[a,...]] as a fact (the existing
   refined-application-becomes-a-fact mechanism). *)

let[@vox.def] double x = x + x
[%%expect {|
val double : int -> int = <fun>
val double_def : int @ total -> unit{ double x = x + x } = <fun>
|}]

(* Opaque: with [double] uninterpreted, [double 5 = 10] is NOT provable. *)
let opaque_is_unproved = (double 5 : int{ _ = 10 })
[%%expect {|
Line 1, characters 25-51:
1 | let opaque_is_unproved = (double 5 : int{ _ = 10 })
                             ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

(* Depositing [double 5 = 5 + 5] (via [double_def 5]) makes the same goal
   provable. *)
let () = double_def 5
let after_def_is_proved = (double 5 : int{ _ = 10 })
[%%expect {|
val after_def_is_proved : int{ _ = 10 } = 10
|}]

(* A false consequence of the deposited equation is disproved. *)
let () = double_def 5
let false_consequence = (double 5 : int{ _ = 11 })
[%%expect {|
Line 2, characters 24-50:
2 | let false_consequence = (double 5 : int{ _ = 11 })
                            ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (disproved)
|}]

(* A two-argument definitional equation. *)
let[@vox.def] add3 x y = x + y + 3
[%%expect {|
val add3 : int @ total logical -> int -> int = <fun>
val add3_def : int @ total -> int @ total -> unit{ add3 x y = x + y + 3 } =
  <fun>
|}]

let () = add3_def 10 20
let add3_used = (add3 10 20 : int{ _ = 33 })
[%%expect {|
val add3_used : int{ _ = 33 } = 33
|}]

(* Fail-closed: a body using integer division is partial, so it cannot be
   [@vox.def]. *)
let[@vox.def] bad_div x = 100 / x
[%%expect {|
Line 1, characters 30-31:
1 | let[@vox.def] bad_div x = 100 / x
                                  ^
Error: The value "(/)" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 22-33
         which is expected to be "total".
|}]

(* Fail-closed: [raise] is partial. *)
let[@vox.def] bad_raise b = if b then raise Not_found else 0
[%%expect {|
Line 1, characters 38-43:
1 | let[@vox.def] bad_raise b = if b then raise Not_found else 0
                                          ^^^^^
Error: The value "raise" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 24-60
         which is expected to be "total".
|}]

(* Fail-closed: recursion is not total, so it cannot be reflected. *)
let[@vox.def] rec bad_rec x = bad_rec x
[%%expect {|
Line 1, characters 0-39:
1 | let[@vox.def] rec bad_rec x = bad_rec x
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: vox: [@vox.def] cannot be used on a recursive binding (recursion is not total, so it cannot be reflected)
|}]

(* Fail-closed: a [@vox.def] binding must be a function with parameters. *)
let[@vox.def] not_a_function = 42
[%%expect {|
Line 1, characters 0-33:
1 | let[@vox.def] not_a_function = 42
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: vox: [@vox.def] requires a function binding with explicit parameters
|}]

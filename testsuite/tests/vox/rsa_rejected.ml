(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "vox_rsa_spec.mli vox_rsa_spec.ml vox_rsa_arithmetic.ml";
 all_modules += " vox_rsa_number_theory.ml";
 all_modules += " vox_rsa_fermat.ml vox_rsa.mli vox_rsa.ml";
 readonly_files = "rsa_rejected.ml";
 compile_only = "true";
 {
   setup-ocamlc.opt-build-env;
   ocamlc.opt;
   run-expect;
   check-program-output;
 }
*)

(* Load the implementation so that the accepted phrases below can be
   evaluated. *)
#load "vox_rsa_spec.cmo";;
#load "vox_rsa_arithmetic.cmo";;
#load "vox_rsa_number_theory.cmo";;
#load "vox_rsa_fermat.cmo";;
#load "vox_rsa.cmo";;

let negative_exponent () =
  let e = -1Z in let n = 35Z in
  Vox_rsa.modexp 2Z e n;;
[%%expect{|
Line 3, characters 20-21:
3 |   Vox_rsa.modexp 2Z e n;;
                        ^
Error: Refinement could not be proved (counterexample)
File "vox_rsa.mli", line 6, characters 45-52:
  The refinement is stated here.
|}]

let zero_modulus () =
  let e = 3Z in let n = 0Z in
  Vox_rsa.modexp 2Z e n;;
[%%expect{|
Line 3, characters 22-23:
3 |   Vox_rsa.modexp 2Z e n;;
                          ^
Error: Refinement could not be proved (counterexample)
File "vox_rsa.mli", line 7, characters 22-28:
  The refinement is stated here.
|}]

(* The key cases below unfold every definition a valid key needs: [prime]
   for 5, 7 and 9, and [lambda] (through [lcm] and [gcd]) for the three
   prime pairs used.  Each rejected case is followed by an accepted one that
   differs only in the bad input, so the rejection is caused by that input
   and not by a missing unfolding. *)
module Facts : sig
  val small_primes : unit ->
    {u : unit | Vox_rsa.Spec.prime 5Z && Vox_rsa.Spec.prime 7Z} @ ghost
    @@ total
  val nine_composite : unit ->
    {u : unit | not (Vox_rsa.Spec.prime 9Z)} @ ghost @@ total
  val small_lambdas : unit ->
    {u : unit | Vox_rsa.Spec.lambda 5Z 5Z = 4Z
      && Vox_rsa.Spec.lambda 5Z 7Z = 12Z
      && Vox_rsa.Spec.lambda 9Z 7Z = 24Z} @ ghost @@ total
end = struct
  let (small_primes @ total) () :
      {u : unit | Vox_rsa.Spec.prime 5Z && Vox_rsa.Spec.prime 7Z} @ ghost =
    ghost_ (
    Vox_rsa.Spec.prime_def 5Z;
    Vox_rsa.Spec.prime_def 7Z;
    Vox_rsa.Spec.no_divisors_def 5Z 4Z;
    Vox_rsa.Spec.no_divisors_def 5Z 3Z;
    Vox_rsa.Spec.no_divisors_def 5Z 2Z;
    Vox_rsa.Spec.no_divisors_def 5Z 1Z;
    Vox_rsa.Spec.no_divisors_def 7Z 6Z;
    Vox_rsa.Spec.no_divisors_def 7Z 5Z;
    Vox_rsa.Spec.no_divisors_def 7Z 4Z;
    Vox_rsa.Spec.no_divisors_def 7Z 3Z;
    Vox_rsa.Spec.no_divisors_def 7Z 2Z;
    Vox_rsa.Spec.no_divisors_def 7Z 1Z;
    ())

  let (nine_composite @ total) () :
      {u : unit | not (Vox_rsa.Spec.prime 9Z)} @ ghost = ghost_ (
    Vox_rsa.Spec.prime_def 9Z;
    Vox_rsa.Spec.no_divisors_def 9Z 8Z;
    Vox_rsa.Spec.no_divisors_def 9Z 7Z;
    Vox_rsa.Spec.no_divisors_def 9Z 6Z;
    Vox_rsa.Spec.no_divisors_def 9Z 5Z;
    Vox_rsa.Spec.no_divisors_def 9Z 4Z;
    Vox_rsa.Spec.no_divisors_def 9Z 3Z;
    ())

  let (small_lambdas @ total) () :
      {u : unit | Vox_rsa.Spec.lambda 5Z 5Z = 4Z
        && Vox_rsa.Spec.lambda 5Z 7Z = 12Z
        && Vox_rsa.Spec.lambda 9Z 7Z = 24Z} @ ghost =
    ghost_ (
    Vox_rsa.Spec.lambda_def 5Z 5Z;
    Vox_rsa.Spec.lambda_def 5Z 7Z;
    Vox_rsa.Spec.lambda_def 9Z 7Z;
    Vox_rsa.Spec.lcm_def 4Z 4Z;
    Vox_rsa.Spec.lcm_def 4Z 6Z;
    Vox_rsa.Spec.lcm_def 8Z 6Z;
    Vox_rsa.Spec.gcd_def 4Z 4Z;
    Vox_rsa.Spec.gcd_def 4Z 0Z;
    Vox_rsa.Spec.gcd_def 4Z 6Z;
    Vox_rsa.Spec.gcd_def 6Z 4Z;
    Vox_rsa.Spec.gcd_def 4Z 2Z;
    Vox_rsa.Spec.gcd_def 8Z 6Z;
    Vox_rsa.Spec.gcd_def 6Z 2Z;
    Vox_rsa.Spec.gcd_def 2Z 0Z;
    ())
end;;
[%%expect{|
module Facts :
  sig
    val small_primes :
      unit ->
      {u : unit
        | (Vox_rsa.Spec.prime (Bigint.of_int 5)) &&
            (Vox_rsa.Spec.prime (Bigint.of_int 7))} @ ghost
      @@ total
    val nine_composite :
      unit -> {u : unit | not (Vox_rsa.Spec.prime (Bigint.of_int 9))} @ ghost
      @@ total
    val small_lambdas :
      unit ->
      {u : unit
        | ((Vox_rsa.Spec.lambda (Bigint.of_int 5) (Bigint.of_int 5)) =
             (Bigint.of_int 4))
            &&
            (((Vox_rsa.Spec.lambda (Bigint.of_int 5) (Bigint.of_int 7)) =
                (Bigint.of_int 12))
               &&
               ((Vox_rsa.Spec.lambda (Bigint.of_int 9) (Bigint.of_int 7)) =
                  (Bigint.of_int 24)))} @ ghost
      @@ total
  end
|}]

(* p = q: every other conjunct of [valid_key] holds (lambda 5 5 = 4 divides
   5 * 5 - 1). *)
let repeated_prime () =
  let p = 5Z in let q = 5Z in let e = 5Z in let d = 5Z in let m = 2Z in
  ghost_ (Facts.small_primes ()); ghost_ (Facts.small_lambdas ());
  ghost_ (Vox_rsa.Spec.valid_key_def p q e d);
  Vox_rsa.roundtrip p q e d m;;
[%%expect{|
Line 5, characters 28-29:
5 |   Vox_rsa.roundtrip p q e d m;;
                                ^
Error: Refinement could not be proved (counterexample)
File "vox_rsa.mli", line 25, characters 22-63:
  The refinement is stated here.
|}]

let distinct_primes () =
  let p = 5Z in let q = 7Z in let e = 5Z in let d = 5Z in let m = 2Z in
  ghost_ (Facts.small_primes ()); ghost_ (Facts.small_lambdas ());
  ghost_ (Vox_rsa.Spec.valid_key_def p q e d);
  Vox_rsa.roundtrip p q e d m;;
[%%expect{|
val distinct_primes : unit -> Bigint.t = <fun>
|}]

(* 5 * 7 - 1 = 34 is not a multiple of lambda 5 7 = 12. *)
let invalid_inverse () =
  let p = 5Z in let q = 7Z in let e = 5Z in let d = 7Z in let m = 2Z in
  ghost_ (Facts.small_primes ()); ghost_ (Facts.small_lambdas ());
  ghost_ (Vox_rsa.Spec.valid_key_def p q e d);
  Vox_rsa.roundtrip p q e d m;;
[%%expect{|
Line 5, characters 28-29:
5 |   Vox_rsa.roundtrip p q e d m;;
                                ^
Error: Refinement could not be proved (counterexample)
File "vox_rsa.mli", line 25, characters 22-63:
  The refinement is stated here.
|}]

let valid_inverse () =
  let p = 5Z in let q = 7Z in let e = 5Z in let d = 5Z in let m = 2Z in
  ghost_ (Facts.small_primes ()); ghost_ (Facts.small_lambdas ());
  ghost_ (Vox_rsa.Spec.valid_key_def p q e d);
  Vox_rsa.roundtrip p q e d m;;
[%%expect{|
val valid_inverse : unit -> Bigint.t = <fun>
|}]

(* A valid key, but the message is not below n = 35. *)
let message_too_large () =
  let p = 5Z in let q = 7Z in let e = 5Z in let d = 5Z in let m = 35Z in
  ghost_ (Facts.small_primes ()); ghost_ (Facts.small_lambdas ());
  ghost_ (Vox_rsa.Spec.valid_key_def p q e d);
  Vox_rsa.roundtrip p q e d m;;
[%%expect{|
Line 5, characters 28-29:
5 |   Vox_rsa.roundtrip p q e d m;;
                                ^
Error: Refinement could not be proved (counterexample)
File "vox_rsa.mli", line 25, characters 22-63:
  The refinement is stated here.
|}]

let largest_message () =
  let p = 5Z in let q = 7Z in let e = 5Z in let d = 5Z in let m = 34Z in
  ghost_ (Facts.small_primes ()); ghost_ (Facts.small_lambdas ());
  ghost_ (Vox_rsa.Spec.valid_key_def p q e d);
  Vox_rsa.roundtrip p q e d m;;
[%%expect{|
val largest_message : unit -> Bigint.t = <fun>
|}]

(* 9 = 3 * 3; with q = 7 and e = d = 5, 24 divides 5 * 5 - 1. *)
let composite_prime () =
  let p = 9Z in let q = 7Z in let e = 5Z in let d = 5Z in let m = 2Z in
  ghost_ (Facts.small_primes ()); ghost_ (Facts.nine_composite ());
  ghost_ (Facts.small_lambdas ());
  ghost_ (Vox_rsa.Spec.valid_key_def p q e d);
  Vox_rsa.roundtrip p q e d m;;
[%%expect{|
Line 6, characters 28-29:
6 |   Vox_rsa.roundtrip p q e d m;;
                                ^
Error: Refinement could not be proved (counterexample)
File "vox_rsa.mli", line 25, characters 22-63:
  The refinement is stated here.
|}]

let prime_instead () =
  let p = 5Z in let q = 7Z in let e = 5Z in let d = 5Z in let m = 2Z in
  ghost_ (Facts.small_primes ()); ghost_ (Facts.nine_composite ());
  ghost_ (Facts.small_lambdas ());
  ghost_ (Vox_rsa.Spec.valid_key_def p q e d);
  Vox_rsa.roundtrip p q e d m;;
[%%expect{|
val prime_instead : unit -> Bigint.t = <fun>
|}]

module Hidden_proof = Vox_rsa.Proof;;
[%%expect{|
Line 1, characters 22-35:
1 | module Hidden_proof = Vox_rsa.Proof;;
                          ^^^^^^^^^^^^^
Error: Unbound module "Vox_rsa.Proof"
|}]

let hidden_helper = Vox_rsa.lambda_lcm;;
[%%expect{|
Line 1, characters 20-38:
1 | let hidden_helper = Vox_rsa.lambda_lcm;;
                        ^^^^^^^^^^^^^^^^^^
Error: Unbound value "Vox_rsa.lambda_lcm"
|}]

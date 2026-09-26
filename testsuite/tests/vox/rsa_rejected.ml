(* TEST
 has-z3;
 flags = "-extension refinement_types -smt-timeout 10000";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "vox_rsa_spec.mli vox_rsa_spec.ml vox_rsa_arithmetic.ml";
 all_modules += " vox_rsa_number_theory.ml";
 all_modules += " vox_rsa_fermat.ml vox_rsa.mli vox_rsa.ml";
 readonly_files = "rsa_rejected.ml";
 compile_only = "true";
 {
   setup-ocamlc.byte-build-env;
   ocamlc.byte;
   run-expect;
   check-program-output;
 }
*)

let negative_exponent () =
  let e = -1Z in let n = 35Z in
  Vox_rsa.modexp 2Z e n;;
[%%expect{|
Line 3, characters 20-21:
3 |   Vox_rsa.modexp 2Z e n;;
                        ^
Error: Refinement could not be proved (counterexample)
|}]

let zero_modulus () =
  let e = 3Z in let n = 0Z in
  Vox_rsa.modexp 2Z e n;;
[%%expect{|
Line 3, characters 22-23:
3 |   Vox_rsa.modexp 2Z e n;;
                          ^
Error: Refinement could not be proved (counterexample)
|}]

let repeated_prime () =
  let p = 5Z in let e = 3Z in let d = 3Z in let m = 5Z in
  Vox_rsa.roundtrip p p e d m;;
[%%expect{|
Line 3, characters 28-29:
3 |   Vox_rsa.roundtrip p p e d m;;
                                ^
Error: Refinement could not be proved (counterexample)
|}]

let invalid_inverse () =
  let p = 5Z in let q = 7Z in let e = 2Z in let d = 2Z in let m = 2Z in
  ghost_ (Vox_rsa.Spec.prime_def p);
  ghost_ (Vox_rsa.Spec.prime_def q);
  ghost_ (Vox_rsa.Spec.lambda_def p q);
  Vox_rsa.roundtrip p q e d m;;
[%%expect{|
Line 6, characters 28-29:
6 |   Vox_rsa.roundtrip p q e d m;;
                                ^
Error: Refinement could not be proved (counterexample)
|}]

let message_too_large () =
  let p = 5Z in let q = 7Z in let e = 5Z in let d = 5Z in let m = 35Z in
  ghost_ (Vox_rsa.Spec.prime_def p);
  ghost_ (Vox_rsa.Spec.prime_def q);
  ghost_ (Vox_rsa.Spec.lambda_def p q);
  Vox_rsa.roundtrip p q e d m;;
[%%expect{|
Line 6, characters 28-29:
6 |   Vox_rsa.roundtrip p q e d m;;
                                ^
Error: Refinement could not be proved (counterexample)
|}]

let composite_prime () =
  let p = 9Z in let q = 7Z in let e = 1Z in let m = 1Z in
  ghost_ (Vox_rsa.Spec.valid_key_def p q e e);
  ghost_ (Vox_rsa.Spec.prime_def p);
  ghost_ (Vox_rsa.Spec.no_divisors_def p 8Z);
  ghost_ (Vox_rsa.Spec.no_divisors_def p 7Z);
  ghost_ (Vox_rsa.Spec.no_divisors_def p 6Z);
  ghost_ (Vox_rsa.Spec.no_divisors_def p 5Z);
  ghost_ (Vox_rsa.Spec.no_divisors_def p 4Z);
  ghost_ (Vox_rsa.Spec.no_divisors_def p 3Z);
  Vox_rsa.roundtrip p q e e m;;
[%%expect{|
Line 11, characters 28-29:
11 |   Vox_rsa.roundtrip p q e e m;;
                                 ^
Error: Refinement could not be proved (counterexample)
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

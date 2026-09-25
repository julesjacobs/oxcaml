(* TEST
 has-z3;
 flags = "-extension refinement_types -smt-timeout 10000";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "vox_rsa_arithmetic.ml vox_rsa_number_theory.ml";
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
  Vox_rsa.modexp 2Z (refine_ e) (refine_ n);;
[%%expect{|
Line 3, characters 20-31:
3 |   Vox_rsa.modexp 2Z (refine_ e) (refine_ n);;
                        ^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let zero_modulus () =
  let e = 3Z in let n = 0Z in
  Vox_rsa.modexp 2Z (refine_ e) (refine_ n);;
[%%expect{|
Line 3, characters 32-43:
3 |   Vox_rsa.modexp 2Z (refine_ e) (refine_ n);;
                                    ^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let repeated_prime () =
  let p = 5Z in let e = 3Z in let d = 3Z in let m = 5Z in
  Vox_rsa.roundtrip p p e d (refine_ m);;
[%%expect{|
Line 3, characters 28-39:
3 |   Vox_rsa.roundtrip p p e d (refine_ m);;
                                ^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let invalid_inverse () =
  let p = 5Z in let q = 7Z in let e = 2Z in let d = 2Z in let m = 2Z in
  ghost_ (Vox_rsa.Number_theory.prime_def p);
  ghost_ (Vox_rsa.Number_theory.prime_def q);
  ghost_ (Vox_rsa.lambda_def p q);
  Vox_rsa.roundtrip p q e d (refine_ m);;
[%%expect{|
Line 6, characters 28-39:
6 |   Vox_rsa.roundtrip p q e d (refine_ m);;
                                ^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let message_too_large () =
  let p = 5Z in let q = 7Z in let e = 5Z in let d = 5Z in let m = 35Z in
  ghost_ (Vox_rsa.Number_theory.prime_def p);
  ghost_ (Vox_rsa.Number_theory.prime_def q);
  ghost_ (Vox_rsa.lambda_def p q);
  Vox_rsa.roundtrip p q e d (refine_ m);;
[%%expect{|
Line 6, characters 28-39:
6 |   Vox_rsa.roundtrip p q e d (refine_ m);;
                                ^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let composite_prime () =
  let p = 9Z in let q = 7Z in let e = 1Z in let m = 1Z in
  ghost_ (Vox_rsa.Number_theory.prime_divisors p 3Z);
  Vox_rsa.roundtrip p q e e (refine_ m);;
[%%expect{|
Line 4, characters 28-39:
4 |   Vox_rsa.roundtrip p q e e (refine_ m);;
                                ^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 prebuilt_modules = "vox_rsa_spec.mli vox_rsa_spec.ml vox_rsa_arithmetic.ml vox_rsa_number_theory.ml vox_rsa_fermat.ml vox_rsa.mli vox_rsa.ml";
 { bytecode; }
 { native; }
*)

open Bigint
module Spec = Vox_rsa.Spec

let (roundtrip @ total) p q e d
    (message : {m : t | Spec.valid_key p q e d && 0Z <= m && m < p * q}) :
    {r : t | r = message} =
  let m = message in
  ghost_ (Spec.valid_key_def p q e d);
  ghost_ (Spec.prime_def p); ghost_ (Spec.prime_def q);
  let n = p * q in
  let c = Vox_rsa.encrypt m e n in
  let r = Vox_rsa.decrypt c d n in
  ghost_ (Vox_rsa.roundtrip_correct p q e d m);
  r

let (crt_equivalence @ total) c (exponent : {d : t | d >= 0Z}) p
    (other_prime : {q : t | Spec.prime p && Spec.prime q && p <> q}) :
    {r : t | let d = exponent in let q = other_prime in
      r = Spec.power c d mod (p * q)} =
  let d = exponent in let q = other_prime in
  ghost_ (Spec.prime_def p); ghost_ (Spec.prime_def q);
  let n = p * q in
  let ordinary = Vox_rsa.decrypt c d n in
  let crt = Vox_rsa.decrypt_crt c d p q in
  ghost_ ((() : {u : unit | ordinary = crt}));
  crt

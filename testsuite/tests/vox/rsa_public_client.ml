(* TEST
 has-z3;
 flags = "-extension refinement_types -smt-timeout 10000";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "vox_rsa_spec.mli vox_rsa_spec.ml vox_rsa_arithmetic.ml";
 all_modules += " vox_rsa_number_theory.ml vox_rsa_fermat.ml";
 all_modules += " vox_rsa.mli vox_rsa.ml rsa_public_client.ml";
 { bytecode; }
 { native; }
*)

open Bigint
module Spec = Vox_rsa.Spec

let (roundtrip @ total) p q e d
    (message : {m : t | Spec.valid_key p q e d && 0Z <= m && m < p * q}) :
    {r : t | let refine_ m = message in r = m} =
  let refine_ m = message in
  ghost_ (Spec.valid_key_def p q e d);
  ghost_ (Spec.prime_def p); ghost_ (Spec.prime_def q);
  let n = p * q in
  let refine_ c = Vox_rsa.encrypt m (refine_ e) (refine_ n) in
  let refine_ r = Vox_rsa.decrypt c (refine_ d) (refine_ n) in
  ghost_ (Vox_rsa.roundtrip_correct p q e d m);
  refine_ r

let (crt_equivalence @ total) c (exponent : {d : t | d >= 0Z}) p
    (other_prime : {q : t | Spec.prime p && Spec.prime q && p <> q}) :
    {r : t | let refine_ d = exponent in let refine_ q = other_prime in
      r = Spec.power c d mod (p * q)} =
  let refine_ d = exponent in let refine_ q = other_prime in
  ghost_ (Spec.prime_def p); ghost_ (Spec.prime_def q);
  let n = p * q in
  let refine_ ordinary = Vox_rsa.decrypt c (refine_ d) (refine_ n) in
  let refine_ crt = Vox_rsa.decrypt_crt c (refine_ d) p (refine_ q) in
  ghost_ (let u = () in (refine_ u : {u : unit | ordinary = crt}));
  refine_ crt

(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "vox_rsa_spec.mli vox_rsa_spec.ml vox_rsa_arithmetic.ml";
 all_modules += " vox_rsa_number_theory.ml";
 all_modules += " vox_rsa_fermat.ml vox_rsa.mli vox_rsa.ml rsa.ml";
 { native; }
*)

module Number_theory = Vox_rsa.Spec

let (example_primes @ total) () :
    {u : unit | Number_theory.prime 5Z && Number_theory.prime 7Z} =
  Number_theory.prime_def 5Z; Number_theory.prime_def 7Z;
  Number_theory.no_divisors_def 5Z 4Z; Number_theory.no_divisors_def 5Z 3Z;
  Number_theory.no_divisors_def 5Z 2Z; Number_theory.no_divisors_def 5Z 1Z;
  Number_theory.no_divisors_def 7Z 6Z; Number_theory.no_divisors_def 7Z 5Z;
  Number_theory.no_divisors_def 7Z 4Z; Number_theory.no_divisors_def 7Z 3Z;
  Number_theory.no_divisors_def 7Z 2Z; Number_theory.no_divisors_def 7Z 1Z;
  ()

let (static_example @ total) () : {r : Bigint.t | r = 5Z} =
  let p = 5Z in let q = 7Z in let e = 5Z in let m = 5Z in
  ghost_ (example_primes ());
  ghost_ (Vox_rsa.Spec.gcd_def 4Z 6Z);
  ghost_ (Vox_rsa.Spec.gcd_def 6Z 4Z);
  ghost_ (Vox_rsa.Spec.gcd_def 4Z 2Z);
  ghost_ (Vox_rsa.Spec.gcd_def 2Z 0Z);
  ghost_ (Vox_rsa.Spec.lcm_def 4Z 6Z);
  ghost_ (Vox_rsa.Spec.lambda_def p q);
  ghost_ (Vox_rsa.Spec.valid_key_def p q e e);
  let r = Vox_rsa.roundtrip p q e e m in
  r

let () =
  let example = static_example () in
  assert (example = 5Z);
  let e = 13Z in
  let n = 497Z in
  let r = Vox_rsa.modexp 4Z e n in
  assert (r = 445Z)

let reference a e n =
  let rec loop acc e =
    if e = 0 then acc
    else loop Bigint.(acc * a mod n) (e - 1)
  in
  loop Bigint.(1Z mod n) e

let () =
  List.iter (fun a ->
    for e = 0 to 32 do
      for n = 1 to 39 do
        let exponent = Bigint.of_int e in
        let modulus = Bigint.of_int n in
        let r = Vox_rsa.modexp a
          (assume_ exponent : {e : Bigint.t | e >= 0Z})
          (assume_ modulus : {n : Bigint.t | n > 0Z}) in
        assert (r = reference a e modulus)
      done
    done)
    [-1234567890123456789012345678901234567890Z; -7Z; 0Z; 1Z; 9Z;
      1234567890123456789012345678901234567890Z];
  let primes = [2; 3; 5; 7; 11; 13] in
  List.iter (fun pi -> List.iter (fun qi ->
    if pi <> qi then begin
      let p = Bigint.of_int pi in
      let q = Bigint.of_int qi in
      for ei = 1 to 24 do
        for di = 1 to 24 do
          let e = Bigint.of_int ei in
          let d = Bigint.of_int di in
          if Bigint.((e * d - 1Z) mod Vox_rsa.Spec.lambda p q = 0Z) then
            for mi = 0 to pi * qi - 1 do
              let m = Bigint.of_int mi in
              let result = Vox_rsa.roundtrip p q e d
                (assume_ m : {m : Bigint.t |
                  Vox_rsa.Spec.valid_key p q e d
                  && 0Z <= m && m < Bigint.mul p q}) in
              assert (result = m)
            done
        done
      done
    end) primes) primes;
  let p = 5Z in
  let q = 7Z in
  let e = Bigint.(12Z * Vox_rsa.Spec.power 10Z 100Z + 1Z) in
  List.iter (fun mi ->
    let m = Bigint.of_int mi in
    let result = Vox_rsa.roundtrip p q e e
      (assume_ m : {m : Bigint.t |
        Vox_rsa.Spec.valid_key p q e e
        && 0Z <= m && m < Bigint.mul p q}) in
    assert (result = m)) [0; 5; 7; 34];
  List.iter (fun pi -> List.iter (fun qi ->
    if pi <> qi then
      for di = 0 to 16 do
        for ci = -2 to pi * qi do
          let p = Bigint.of_int pi in let q = Bigint.of_int qi in
          let d = Bigint.of_int di in let c = Bigint.of_int ci in
          let exponent = (assume_ d : {d : Bigint.t | d >= 0Z}) in
          let other_prime = (assume_ q : {q : Bigint.t |
            Number_theory.prime p && Number_theory.prime q && p <> q}) in
          let n = Bigint.mul p q in
          let ordinary = Vox_rsa.decrypt c exponent
            (assume_ n : {n : Bigint.t | n > 0Z}) in
          let crt = Vox_rsa.decrypt_crt c exponent p other_prime in
          ghost_ ((() : {u : unit | ordinary = crt}));
          assert (ordinary = crt)
        done
      done) primes) primes;
  let p = 53Z in let q = 61Z in let e = 17Z in let d = 2753Z in
  let p = (assume_ p : {p : Bigint.t | Number_theory.prime p}) in
  let q = (assume_ q : {q : Bigint.t |
    Number_theory.prime q && p <> q}) in
  let d = (assume_ d : {d : Bigint.t | d > 0Z &&
    Bigint.modulo (Bigint.sub (Bigint.mul e d) 1Z)
      (Vox_rsa.Spec.lambda p q) = 0Z}) in
  ghost_ (Vox_rsa.Spec.valid_key_def p q e d);
  for mi = 0 to 3232 do
    let m = Bigint.of_int mi in
    let m = (assume_ m : {m : Bigint.t |
      0Z <= m && m < Bigint.mul p q}) in
    let r = Vox_rsa.roundtrip p q e d m in
    assert (r = m)
  done;
  let exponent = 17Z in
  let modulus = 3233Z in
  let encrypted =
    Vox_rsa.encrypt 65Z exponent modulus in
  assert (encrypted = 2790Z);
  let exponent = 2753Z in
  let decrypted =
    Vox_rsa.decrypt encrypted exponent modulus in
  assert (decrypted = 65Z);
  let p = 65537Z in let q = 257Z in let e = 17Z in let d = 61681Z in
  List.iter (fun mi ->
    let m = Bigint.of_int mi in
    let r = Vox_rsa.roundtrip p q e d
      (assume_ m : {m : Bigint.t |
        Vox_rsa.Spec.valid_key p q e d
        && 0Z <= m && m < Bigint.mul p q}) in
    assert (r = m)) [0; 257; 65537; 65538; 16843008];
  print_endline
    "RSA: modular powers, exhaustive keys, large exponents, and CRT passed"

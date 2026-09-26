open Bigint
module Spec = Vox_rsa_spec
open Spec
open Vox_rsa_arithmetic
open Vox_rsa_number_theory
open Vox_rsa_fermat

let modexp = Vox_rsa_arithmetic.modexp

let (lambda_lcm @ total) (p : t) (q : t) (multiple : t) :
    {u : unit | if prime p && prime q && p <> q then
      lambda p q > 0Z
      && lambda p q mod (p - 1Z) = 0Z
      && lambda p q mod (q - 1Z) = 0Z
      && (if multiple > 0Z && multiple mod (p - 1Z) = 0Z
            && multiple mod (q - 1Z) = 0Z
          then multiple mod lambda p q = 0Z && multiple >= lambda p q
          else true)
      else true} =
  prime_def p; prime_def q; lambda_def p q;
  lcm_properties (p - 1Z) (q - 1Z) multiple;
  ()

let (reduce_divisor @ total) (a : t) (p : t) (q : t) :
    {u : unit | if p > 0Z && q > 0Z then
      (a mod (p * q)) mod p = a mod p else true} =
  if p <= 0Z || q <= 0Z then ()
  else begin
    let r = a mod (p * q) in
    remainder_unique a p (q * (a / (p * q)) + r / p) (r mod p);
    ()
  end


let (rsa_power @ total) (p : t) (q : t) (e : t) (d : t) (m : t) :
    {u : unit | if prime p && prime q && p <> q
      && e > 0Z && d > 0Z && (e * d - 1Z) mod lambda p q = 0Z
      && 0Z <= m && m < p * q
      then power m (e * d) mod (p * q) = m else true} =
  prime_def p; prime_def q; lambda_def p q;
  let u = () in
  if not (prime p && prime q && p <> q
      && e > 0Z && d > 0Z && (e * d - 1Z) mod lambda p q = 0Z
      && 0Z <= m && m < p * q) then u
  else begin
    let k = e * d - 1Z in
    lambda_lcm p q k;
    divides_transitive (p - 1Z) (lambda p q) k;
    divides_transitive (q - 1Z) (lambda p q) k;
    let kp = k / (p - 1Z) in
    let kq = k / (q - 1Z) in
    fermat_period m p kp;
    fermat_period m q kq;
    let a = power m (e * d) in
    reduce_divisor a p q;
    reduce_divisor a q p;
    crt_unique p q (a mod (p * q)) m;
    u
  end

let encrypt = modexp
let decrypt = modexp

let (roundtrip_correct @ total) (p : t) (q : t) (e : t) (d : t) (m : t) :
    {u : unit | if valid_key p q e d && 0Z <= m && m < p * q then
      power (power m e mod (p * q)) d mod (p * q) = m else true} =
  valid_key_def p q e d;
  prime_def p; prime_def q;
  let n = p * q in
  reduce_power (power m e) d n;
  power_multiply m e d;
  rsa_power p q e d m;
  ()

let (roundtrip @ total) : (p : t) -> (q : t) -> (e : t) -> (d : t) ->
    (message : {m : t | valid_key p q e d && 0Z <= m && m < p * q}) ->
    {r : t | r = message} =
  fun p q e d message ->
  let m = message in
  ghost_ (valid_key_def p q e d);
  ghost_ (prime_def p);
  ghost_ (prime_def q);
  let n = p * q in
  let ciphertext = encrypt m e n in
  let plaintext = decrypt ciphertext d n in
  ghost_ (roundtrip_correct p q e d m);
  plaintext

let (prime_inverse @ total) (p : t) (q : t) :
    {u : unit | if prime p && prime q && p <> q then
      (p * power p (q - 2Z)) mod q = 1Z else true} =
  distinct_prime_nondivisor p q;
  fermat_little p q;
  power_def p (q - 1Z);
  ()

(* Nonlinear integer arithmetic (the CRT recombination); Z3 needs about
   0.7 s (warning 222). *)
let[@warning "-222"] (recombine_correct @ total)
    (p : t) (q : t) (rp : t) (rq : t) (inverse : t) :
    {u : unit | let r = rp + p * (((rq - rp) * inverse) mod q) in
      if p > 0Z && q > 1Z && 0Z <= rp && rp < p
        && 0Z <= rq && rq < q && (p * inverse) mod q = 1Z then
        0Z <= r && r < p * q && r mod p = rp && r mod q = rq
      else true} =
  let v = (rq - rp) * inverse in
  let h = v mod q in
  let r = rp + p * h in
  if not (p > 0Z && q > 1Z && 0Z <= rp && rp < p
    && 0Z <= rq && rq < q && (p * inverse) mod q = 1Z) then ()
  else begin
    let quotient = (rq - rp) * ((p * inverse) / q) - p * (v / q) in
    (() : {u : unit | r = q * quotient + rq});
    remainder_unique r p h rp;
    remainder_unique r q quotient rq;
    ()
  end

let (computed_inverse_correct @ total) (p : t) (q : t) (inverse : t) :
    {u : unit | if prime p && prime q && p <> q
      && inverse = power p (q - 2Z) mod q then
      (p * inverse) mod q = 1Z else true} =
  prime_def q;
  prime_inverse p q;
  reduce_left (power p (q - 2Z)) p q;
  ()

let (decrypt_crt_correct @ total)
    (ciphertext : t) (d : t) (p : t) (q : t) (r : t) :
    {u : unit | if prime p && prime q && p <> q && d >= 0Z
      && 0Z <= r && r < p * q
      && r mod p = power ciphertext d mod p
      && r mod q = power ciphertext d mod q then
      r = power ciphertext d mod (p * q) else true} =
  prime_def p; prime_def q;
  let a = power ciphertext d in
  reduce_divisor a p q;
  reduce_divisor a q p;
  crt_unique p q r (a mod (p * q));
  ()

let (recombine @ total) (p : t) (q : t) (rp : t) (rq : t) (inverse : t) :
    {r : t | if p > 0Z && q > 1Z && 0Z <= rp && rp < p
      && 0Z <= rq && rq < q && (p * inverse) mod q = 1Z then
      0Z <= r && r < p * q && r mod p = rp && r mod q = rq else true} =
  let v = (rq - rp) * inverse in
  let h = v mod q in
  let r = rp + p * h in
  ghost_ (recombine_correct p q rp rq inverse);
  r

let (decrypt_crt @ total) : (ciphertext : t) ->
    (exponent : {d : t | d >= 0Z}) ->
    (p : t) -> (other_prime : {q : t | prime p && prime q && p <> q}) ->
    {r : t | let d = exponent in let q = other_prime in
      0Z <= r && r < p * q && r = power ciphertext d mod (p * q)} =
  fun ciphertext exponent p other_prime ->
  let d = exponent in
  let q = other_prime in
  ghost_ (prime_def p);
  ghost_ (prime_def q);
  let rp = modexp ciphertext exponent p in
  let rq = modexp ciphertext exponent q in
  let inverse_exponent = q - 2Z in
  let inverse = modexp p inverse_exponent q in
  ghost_ (computed_inverse_correct p q inverse);
  let r = recombine p q rp rq inverse in
  ghost_ (decrypt_crt_correct ciphertext d p q r);
  r

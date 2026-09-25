open Bigint
open Vox_rsa_spec
open Vox_rsa_arithmetic

type bezout = { g : t; x : t; y : t }

let rec (extended_gcd @ total) : (a : t) -> (b : t) ->
    {r : bezout | if a >= 0Z && b >= 0Z then
      r.g = gcd a b && r.g >= 0Z && r.g = a * r.x + b * r.y
      && (if a > 0Z || b > 0Z then
        r.g > 0Z && a mod r.g = 0Z && b mod r.g = 0Z else true)
      else true} = fun a b ->
  if a < 0Z || b < 0Z then
    {g = 0Z; x = 0Z; y = 0Z}
  else if b = 0Z then begin
    gcd_def a b;
    {g = a; x = 1Z; y = 0Z}
  end else begin
    let previous = extended_gcd b (a mod b) in
    gcd_def a b;
    let quotient = a / b in
    divides_sum previous.g b (a mod b) quotient 1Z;
    {g = previous.g; x = previous.y;
      y = previous.x - quotient * previous.y}
  end
[@@decreases b]

let rec (no_divisors_at @ total) : (p : t) -> (k : t) -> (d : t) ->
    {u : unit | if no_divisors p k && 2Z <= d && d <= k
      then p mod d <> 0Z else true} = fun p k d ->
  no_divisors_def p k;
  if k < 2Z || d > k || d < 2Z then ()
  else if k = d then ()
  else begin no_divisors_at p (k - 1Z) d; () end
[@@decreases k]

let (prime_divisors @ total) (p : t) (d : t) :
    {u : unit | if prime p && d > 0Z && p mod d = 0Z
      then p > 1Z && (d = 1Z || d = p) else true} =
  prime_def p;
  no_divisors_at p (p - 1Z) d;
  ()

let (prime_coprime @ total) (p : t) (a : t) :
    {r : bezout | if prime p && a >= 0Z && a mod p <> 0Z then
      r.g = 1Z && 1Z = a * r.x + p * r.y else true} =
  prime_def p;
  let r = extended_gcd a p in
  prime_divisors p r.g;
  r

let (prime_cancel @ total) (p : t) (a : t) (b : t) :
    {u : unit | if prime p && a >= 0Z && a mod p <> 0Z
      && (a * b) mod p = 0Z then b mod p = 0Z else true} =
  prime_def p;
  let r = prime_coprime p a in
  divides_sum p (a * b) p r.x (r.y * b);
  ()

let (lcm_properties @ total) (a : t) (b : t) (multiple : t) :
    {u : unit | if a > 0Z && b > 0Z then
      lcm a b > 0Z && lcm a b mod a = 0Z && lcm a b mod b = 0Z
      && (if multiple mod a = 0Z && multiple mod b = 0Z then
        multiple mod lcm a b = 0Z else true)
      && (if multiple > 0Z && multiple mod a = 0Z && multiple mod b = 0Z
        then multiple >= lcm a b else true)
      else true} =
  lcm_def a b;
  if a <= 0Z || b <= 0Z then ()
  else begin
    let r = extended_gcd a b in
    let l = lcm a b in
    (() : {u : unit | l > 0Z && l = a * (b / r.g)});
    remainder_unique l a (b / r.g) 0Z;
    remainder_unique l b (a / r.g) 0Z;
    if multiple mod a = 0Z && multiple mod b = 0Z then begin
      let quotient = (multiple / b) * r.x + (multiple / a) * r.y in
      (() : {u : unit | multiple * r.g = l * quotient * r.g});
      (() : {u : unit | multiple = l * quotient});
      remainder_unique multiple l quotient 0Z;
      ()
    end else ()
  end

let (distinct_prime_nondivisor @ total) (p : t) (q : t) :
    {u : unit | if prime p && prime q && p <> q then
      p > 1Z && q > 1Z && p mod q <> 0Z && q mod p <> 0Z else true} =
  prime_def p; prime_def q;
  prime_divisors p q; prime_divisors q p;
  ()

let (crt_unique @ total) (p : t) (q : t) (a : t) (b : t) :
    {u : unit | if prime p && prime q && p <> q
      && 0Z <= a && a < p * q && 0Z <= b && b < p * q
      && a mod p = b mod p && a mod q = b mod q
      then a = b else true} =
  distinct_prime_nondivisor p q;
  if not (prime p && prime q && p <> q
      && 0Z <= a && a < p * q && 0Z <= b && b < p * q
      && a mod p = b mod p && a mod q = b mod q) then ()
  else begin
    equal_remainders a b p;
    equal_remainders a b q;
    let difference = a - b in
    let factor = difference / q in
    prime_cancel p q factor;
    remainder_unique difference (p * q) (factor / p) 0Z;
    ()
  end

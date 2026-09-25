open Bigint

module Modular = Vox_rsa_arithmetic
module Number_theory = Vox_rsa_number_theory
module Proof = Vox_rsa_fermat
open Modular
open Number_theory

val lambda : t -> t -> t @@ total
val lambda_def : (p : t) -> (q : t) ->
  {u : unit | lambda p q === lcm (p - 1Z) (q - 1Z)} @@ total
val lambda_lcm : (p : t) -> (q : t) -> (multiple : t) ->
  {u : unit | if prime p && prime q && p <> q then
    lambda p q > 0Z
    && lambda p q mod (p - 1Z) = 0Z
    && lambda p q mod (q - 1Z) = 0Z
    && (if multiple > 0Z && multiple mod (p - 1Z) = 0Z
          && multiple mod (q - 1Z) = 0Z
        then multiple mod lambda p q = 0Z && multiple >= lambda p q
        else true)
    else true} @@ total

val modexp : (a : t) -> (exponent : {e : t | e >= 0Z}) ->
  (modulus : {n : t | n > 0Z}) ->
  {r : t | let refine_ e = exponent in let refine_ n = modulus in
    0Z <= r && r < n && r = power a e mod n} @@ total
val encrypt : (a : t) -> (exponent : {e : t | e >= 0Z}) ->
  (modulus : {n : t | n > 0Z}) ->
  {r : t | let refine_ e = exponent in let refine_ n = modulus in
    0Z <= r && r < n && r = power a e mod n} @@ total
val decrypt : (a : t) -> (exponent : {e : t | e >= 0Z}) ->
  (modulus : {n : t | n > 0Z}) ->
  {r : t | let refine_ e = exponent in let refine_ n = modulus in
    0Z <= r && r < n && r = power a e mod n} @@ total

val fermat : (a : t) -> (p : t) ->
  {u : unit | if prime p then power a p mod p = a mod p else true}
  @@ total
val fermat_period : (a : t) -> (p : t) -> (k : t) ->
  {u : unit | if prime p && k >= 0Z then
    power a (1Z + k * (p - 1Z)) mod p = a mod p else true} @@ total
val crt_unique : (p : t) -> (q : t) -> (a : t) -> (b : t) ->
  {u : unit | if prime p && prime q && p <> q
    && 0Z <= a && a < p * q && 0Z <= b && b < p * q
    && a mod p = b mod p && a mod q = b mod q
    then a = b else true} @@ total
val rsa_power : (p : t) -> (q : t) -> (e : t) -> (d : t) -> (m : t) ->
  {u : unit | if prime p && prime q && p <> q
    && e > 0Z && d > 0Z && (e * d - 1Z) mod lambda p q = 0Z
    && 0Z <= m && m < p * q
    then power m (e * d) mod (p * q) = m else true} @@ total
val roundtrip : (p : t) -> (q : t) -> (e : t) -> (d : t) ->
  (message : {m : t | prime p && prime q && p <> q
    && e > 0Z && d > 0Z && (e * d - 1Z) mod lambda p q = 0Z
    && 0Z <= m && m < p * q}) ->
  {r : t | let refine_ m = message in r = m} @@ total

val decrypt_crt : (ciphertext : t) ->
  (exponent : {d : t | d >= 0Z}) ->
  (p : t) -> (other_prime : {q : t | prime p && prime q && p <> q}) ->
  {r : t | let refine_ d = exponent in let refine_ q = other_prime in
    0Z <= r && r < p * q && r = power ciphertext d mod (p * q)} @@ total

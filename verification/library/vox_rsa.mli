open Bigint

module Spec = Vox_rsa_spec
open Spec

val modexp : (a : t) -> (exponent : {e : t | e >= 0Z}) ->
  (modulus : {n : t | n > 0Z}) ->
  {r : t | let e = exponent in let n = modulus in
    0Z <= r && r < n && r = power a e mod n} @@ total
val encrypt : (a : t) -> (exponent : {e : t | e >= 0Z}) ->
  (modulus : {n : t | n > 0Z}) ->
  {r : t | let e = exponent in let n = modulus in
    0Z <= r && r < n && r = power a e mod n} @@ total
val decrypt : (a : t) -> (exponent : {e : t | e >= 0Z}) ->
  (modulus : {n : t | n > 0Z}) ->
  {r : t | let e = exponent in let n = modulus in
    0Z <= r && r < n && r = power a e mod n} @@ total

val roundtrip_correct :
  (p : t) -> (q : t) -> (e : t) -> (d : t) -> (m : t) ->
  {u : unit | if valid_key p q e d && 0Z <= m && m < p * q then
    power (power m e mod (p * q)) d mod (p * q) = m else true} @@ total

val roundtrip : (p : t) -> (q : t) -> (e : t) -> (d : t) ->
  (message : {m : t | valid_key p q e d && 0Z <= m && m < p * q}) ->
  {r : t | r = message} @@ total

val decrypt_crt : (ciphertext : t) ->
  (exponent : {d : t | d >= 0Z}) ->
  (p : t) -> (other_prime : {q : t | prime p && prime q && p <> q}) ->
  {r : t | let d = exponent in let q = other_prime in
    0Z <= r && r < p * q && r = power ciphertext d mod (p * q)} @@ total

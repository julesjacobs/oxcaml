open Bigint

val power : t -> t -> t @@ total
val power_def : (a : t) -> (e : t) ->
  {u : unit | power a e ===
    (if e <= 0Z then 1Z else a * power a (e - 1Z))} @@ total

val no_divisors : t -> t -> bool @@ total
val no_divisors_def : (p : t) -> (k : t) ->
  {u : unit | no_divisors p k ===
    (if k < 2Z then true else p mod k <> 0Z && no_divisors p (k - 1Z))}
  @@ total
val prime : t -> bool @@ total
val prime_def : (p : t) ->
  {u : unit | prime p === (p > 1Z && no_divisors p (p - 1Z))} @@ total

val gcd : t -> t -> t @@ total
val gcd_def : (a : t) -> (b : t) ->
  {u : unit | gcd a b ===
    (if a < 0Z || b < 0Z then 0Z
     else if b = 0Z then a else gcd b (a mod b))} @@ total
val lcm : t -> t -> t @@ total
val lcm_def : (a : t) -> (b : t) ->
  {u : unit | lcm a b ===
    (if a <= 0Z || b <= 0Z then 0Z else (a / gcd a b) * b)} @@ total
val lambda : t -> t -> t @@ total
val lambda_def : (p : t) -> (q : t) ->
  {u : unit | lambda p q === lcm (p - 1Z) (q - 1Z)} @@ total

val valid_key : t -> t -> t -> t -> bool @@ total
val valid_key_def : (p : t) -> (q : t) -> (e : t) -> (d : t) ->
  {u : unit | valid_key p q e d ===
    (prime p && prime q && p <> q && e > 0Z && d > 0Z
     && (e * d - 1Z) mod lambda p q = 0Z)} @@ total

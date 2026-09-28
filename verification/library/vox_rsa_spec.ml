open Bigint

let[@def] rec power a e =
  if e <= 0Z then 1Z else a * power a (e - 1Z)
[@@decreases e]

let[@def] rec no_divisors p k =
  if k < 2Z then true else p mod k <> 0Z && no_divisors p (k - 1Z)
[@@decreases k]

let[@def] prime p = p > 1Z && no_divisors p (p - 1Z)

let[@def] rec gcd a b =
  if a < 0Z || b < 0Z then 0Z
  else if b = 0Z then a else gcd b (a mod b)
[@@decreases b]

let[@def] lcm a b =
  if a <= 0Z || b <= 0Z then 0Z else (a / gcd a b) * b

let[@def] lambda p q = lcm (p - 1Z) (q - 1Z)

let[@def] valid_key p q e d =
  prime p && prime q && p <> q && e > 0Z && d > 0Z
  && (e * d - 1Z) mod lambda p q = 0Z

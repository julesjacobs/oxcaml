open Bigint
open Vox_rsa_spec
let rec (power_add @ total) : (a : t) -> (e : t) -> (f : t) ->
    {u : unit | if e >= 0Z && f >= 0Z then
      power a (e + f) = power a e * power a f else true} = fun a e f ->
  if e < 0Z || f < 0Z then ()
  else if e = 0Z then (power_def a e; ())
  else begin
    power_add a (e - 1Z) f;
    power_def a e;
    power_def a (e + f);
    ()
  end
[@@decreases e]

let (remainder_unique @ total) (a : t) (n : t) (q : t) (r : t) :
    {u : unit | if n > 0Z && 0Z <= r && r < n && a = n * q + r
      then a mod n = r else true} =
  if q < a / n then ()
  else if q > a / n then ()
  else ()

let (reduce_product @ total) (a : t) (b : t) (n : t) :
    {u : unit | if n > 0Z then
      (a * b) mod n = ((a mod n) * (b mod n)) mod n else true} =
  if n <= 0Z then ()
  else begin
    let x = a mod n in
    let y = b mod n in
    let q = (a / n) * b + x * (b / n) + (x * y) / n in
    remainder_unique (a * b) n q ((x * y) mod n);
    ()
  end

let rec (modexp @ total) : (a : t) ->
    (exponent : {e : t | e >= 0Z}) ->
    (modulus : {n : t | n > 0Z}) ->
    {r : t | let e = exponent in let n = modulus in
      0Z <= r && r < n && r = power a e mod n} =
  fun a exponent modulus ->
  let e = exponent in
  let n = modulus in
  if e = 0Z then begin
    ghost_ (power_def a e);
    1Z mod n
  end else begin
    let k = e / 2Z in
    let half = modexp a k modulus in
    let square = (half * half) mod n in
    ghost_ begin
      power_add a k k;
      reduce_product (power a k) (power a k) n;
      (() : {u : unit | square = power a (2Z * k) mod n})
    end;
    if e mod 2Z = 0Z then square
    else begin
      let r = (square * (a mod n)) mod n in
      ghost_ begin
        power_def a e;
        reduce_product (power a (2Z * k)) a n;
        (() : {u : unit | r = power a e mod n})
      end;
      r
    end
  end
[@@decreases exponent]

let rec (power_multiply @ total) : (a : t) -> (e : t) -> (f : t) ->
    {u : unit | if e >= 0Z && f >= 0Z then
      power (power a e) f = power a (e * f) else true} = fun a e f ->
  if e < 0Z || f < 0Z then ()
  else if f = 0Z then begin
    power_def (power a e) f;
    power_def a 0Z;
    ()
  end else begin
    power_multiply a e (f - 1Z);
    power_def (power a e) f;
    power_add a e (e * (f - 1Z));
    ()
  end
[@@decreases f]

let rec (reduce_power @ total) : (a : t) -> (e : t) -> (n : t) ->
    {u : unit | if e >= 0Z && n > 0Z then
      power (a mod n) e mod n = power a e mod n else true} = fun a e n ->
  if e < 0Z || n <= 0Z then ()
  else begin
    power_def a e;
    power_def (a mod n) e;
    if e = 0Z then ()
    else begin
      reduce_power a (e - 1Z) n;
      reduce_product a (power a (e - 1Z)) n;
      reduce_product (a mod n) (power (a mod n) (e - 1Z)) n;
      ()
    end
  end
[@@decreases e]


let (divides_factor @ total) (n : t) (k : t) :
    {u : unit | if n > 0Z then (n * k) mod n = 0Z else true} =
  remainder_unique (n * k) n k 0Z;
  ()

let (divides_sum @ total) (n : t) (a : t) (b : t) (x : t) (y : t) :
    {u : unit | if n > 0Z && a mod n = 0Z && b mod n = 0Z then
      (a * x + b * y) mod n = 0Z else true} =
  remainder_unique (a * x + b * y) n ((a / n) * x + (b / n) * y) 0Z;
  ()

let (divides_transitive @ total) (a : t) (b : t) (c : t) :
    {u : unit | if a > 0Z && b > 0Z && b mod a = 0Z && c mod b = 0Z
      then c mod a = 0Z else true} =
  remainder_unique c a ((c / b) * (b / a)) 0Z;
  ()

let (reduce_left @ total) (a : t) (b : t) (n : t) :
    {u : unit | if n > 0Z then
      ((a mod n) * b) mod n = (a * b) mod n else true} =
  let r = ((a mod n) * b) mod n in
  remainder_unique (a * b) n ((a / n) * b + ((a mod n) * b) / n) r;
  ()

let (multiply_congruent @ total) (factor : t) (x : t) (y : t) (n : t) :
    {u : unit | if n > 0Z && x mod n = y mod n then
      (factor * x) mod n = (factor * y) mod n else true} =
  reduce_product factor x n;
  reduce_product factor y n;
  ()

let (multiply_four @ total) (a : t) (b : t) (c : t) (d : t) :
    {u : unit | (a * b) * (c * d) = (a * c) * (b * d)} =
  ()

let (equal_remainders @ total) (a : t) (b : t) (n : t) :
    {u : unit | if n > 0Z then
      (a mod n = b mod n) === ((a - b) mod n = 0Z) else true} =
  if n <= 0Z then ()
  else if a mod n = b mod n then begin
    remainder_unique (a - b) n (a / n - b / n) 0Z;
    ()
  end else begin
    remainder_unique a n ((a - b) / n + b / n) (b mod n);
    ()
  end

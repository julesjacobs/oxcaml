open Bigint
open Vox_rsa_spec
open Vox_rsa_arithmetic
open Vox_rsa_number_theory

(* Nonlinear integer arithmetic: about 1.6M solver resource units, over the
   1M slow-refinement threshold. *)
let[@warning "-slow-refinement"] (inverse_action @ total) (a : t) (inverse : t) (p : t) (x : t) :
    {u : unit | if p > 1Z && (a * inverse) mod p = 1Z then
      (((a * x) mod p) * inverse) mod p = x mod p else true} =
  reduce_left (a * x) inverse p;
  reduce_left (a * inverse) x p;
  ()

let (inverse_index @ total) (a : t) (inverse : t) (p : t) (i : t) (x : t) :
    {u : unit | if p > 1Z && (a * inverse) mod p = 1Z
      && 1Z <= i && i < p then
      1Z <= (a * i) mod p && (a * i) mod p < p
      && ((a * i) mod p = x) ===
        (1Z <= x && x < p && i = (inverse * x) mod p)
      else true} =
  inverse_action a inverse p i;
  inverse_action inverse a p x;
  ()

let[@def] rec count x xs = match xs with
  | [] -> 0Z
  | h :: tail -> (if x = h then 1Z else 0Z) + count x tail

let[@def] rec product xs = match xs with
  | [] -> 1Z
  | h :: tail -> h * product tail

let[@def] rec remove x xs = match xs with
  | [] -> []
  | h :: tail -> if x = h then tail else h :: remove x tail

let rec (count_nonnegative @ total) : (x : t) -> (xs : t list) ->
    {u : unit | count x xs >= 0Z} = fun x xs ->
  count_def x xs;
  (match xs with [] -> () | _ :: tail -> count_nonnegative x tail);
  ()

let rec (remove_count @ total) : (x : t) -> (y : t) -> (xs : t list) ->
    {u : unit | count x (remove y xs) = count x xs -
      (if x = y && count y xs > 0Z then 1Z else 0Z)} = fun x y xs ->
  count_def x xs; count_def y xs; remove_def y xs;
  match xs with
  | [] -> count_def x []; ()
  | h :: tail ->
    count_nonnegative y tail;
    if y = h then ()
    else begin
      remove_count x y tail;
      count_def x (h :: remove y tail);
      ()
    end

let rec (remove_product @ total) : (x : t) -> (xs : t list) ->
    {u : unit | if count x xs > 0Z then
      product xs = x * product (remove x xs) else true} = fun x xs ->
  count_def x xs; product_def xs; remove_def x xs;
  match xs with
  | [] -> ()
  | h :: tail ->
    if x = h then ()
    else begin
      remove_product x tail;
      product_def (h :: remove x tail);
      ()
    end

let rec (product_extensional @ total) :
    (xs : t list) @ immutable -> (ys : t list) @ immutable ->
    ((x : t) -> {u : unit | count x xs = count x ys}) @ total ->
    {u : unit | product xs = product ys} = fun xs ys equal_counts ->
  product_def xs;
  match xs with
  | [] ->
    (match ys with
     | [] -> ()
     | h :: tail ->
       equal_counts h;
       count_def h xs; count_def h ys; count_nonnegative h tail;
       ())
  | h :: tail ->
    equal_counts h;
    count_def h xs; count_nonnegative h tail;
    remove_product h ys;
    let remaining = remove h ys in
    let same (x : t) : {u : unit | count x tail = count x remaining} =
      equal_counts x;
      count_def x xs;
      remove_count x h ys;
      ()
    in
    product_extensional tail remaining same;
    ()

let[@def] rec interval k =
  if k <= 0Z then [] else k :: interval (k - 1Z)
[@@decreases k]

let[@def] rec multiples a p k =
  if k <= 0Z then [] else ((a * k) mod p) :: multiples a p (k - 1Z)
[@@decreases k]

let rec (interval_count @ total) : (x : t) -> (k : t) ->
    {u : unit | count x (interval k) =
      (if 1Z <= x && x <= k then 1Z else 0Z)} = fun x k ->
  interval_def k;
  if k <= 0Z then begin count_def x []; () end
  else begin
    interval_count x (k - 1Z);
    count_def x (k :: interval (k - 1Z));
    ()
  end
[@@decreases k]

let rec (multiples_count @ total) :
    (a : t) -> (inverse : t) -> (p : t) -> (k : t) -> (x : t) ->
    {u : unit | if p > 1Z && (a * inverse) mod p = 1Z && 0Z <= k && k < p then
      count x (multiples a p k) =
        (if 1Z <= x && x < p && 1Z <= (inverse * x) mod p
            && (inverse * x) mod p <= k then 1Z else 0Z)
      else true} = fun a inverse p k x ->
  if not (p > 1Z && (a * inverse) mod p = 1Z && 0Z <= k && k < p) then
    ()
  else begin
    multiples_def a p k;
    if k = 0Z then begin count_def x []; () end
    else begin
      multiples_count a inverse p (k - 1Z) x;
      inverse_index a inverse p k x;
      count_def x (((a * k) mod p) :: multiples a p (k - 1Z));
      ()
    end
  end
[@@decreases k]

let (multiples_permute @ total) (a : t) (inverse : t) (p : t) :
    {u : unit | if p > 1Z && (a * inverse) mod p = 1Z then
      product (multiples a p (p - 1Z)) = product (interval (p - 1Z))
      else true} =
  if not (p > 1Z && (a * inverse) mod p = 1Z) then ()
  else begin
    let xs = multiples a p (p - 1Z) in
    let ys = interval (p - 1Z) in
    let same (x : t) : {u : unit | count x xs = count x ys} =
      multiples_count a inverse p (p - 1Z) x;
      interval_count x (p - 1Z);
      inverse_action inverse a p x;
      ()
    in
    product_extensional xs ys same;
    ()
  end

let rec (interval_product_positive @ total) : (k : t) ->
    {u : unit | product (interval k) > 0Z} = fun k ->
  interval_def k;
  if k <= 0Z then begin product_def []; () end
  else begin
    interval_product_positive (k - 1Z);
    product_def (k :: interval (k - 1Z));
    ()
  end
[@@decreases k]

let rec (interval_product_nonzero @ total) : (p : t) -> (k : t) ->
    {u : unit | if prime p && 0Z <= k && k < p then
      product (interval k) mod p <> 0Z else true} = fun p k ->
  prime_def p;
  if not (prime p && 0Z <= k && k < p) then ()
  else begin
    interval_def k;
    if k = 0Z then begin product_def []; () end
    else begin
      interval_product_nonzero p (k - 1Z);
      product_def (k :: interval (k - 1Z));
      prime_cancel p k (product (interval (k - 1Z)));
      ()
    end
  end
[@@decreases k]

let rec (multiples_product @ total) : (a : t) -> (p : t) -> (k : t) ->
    {u : unit | if p > 0Z && k >= 0Z then
      product (multiples a p k) mod p =
        (power a k * product (interval k)) mod p else true} = fun a p k ->
  if p <= 0Z || k < 0Z then ()
  else begin
    multiples_def a p k; interval_def k; power_def a k;
    if k = 0Z then begin product_def []; () end
    else begin
      multiples_product a p (k - 1Z);
      product_def (((a * k) mod p) :: multiples a p (k - 1Z));
      product_def (k :: interval (k - 1Z));
      reduce_left (a * k) (product (multiples a p (k - 1Z))) p;
      multiply_congruent (a * k) (product (multiples a p (k - 1Z)))
        (power a (k - 1Z) * product (interval (k - 1Z))) p;
      multiply_four a (power a (k - 1Z)) k (product (interval (k - 1Z)));
      ()
    end
  end
[@@decreases k]

let (fermat_little @ total) (a : t) (p : t) :
    {u : unit | if prime p && a >= 0Z && a mod p <> 0Z then
      power a (p - 1Z) mod p = 1Z else true} =
  prime_def p;
  if not (prime p && a >= 0Z && a mod p <> 0Z) then ()
  else begin
    let bezout = prime_coprime p a in
    remainder_unique (a * bezout.x) p (-bezout.y) 1Z;
    multiples_permute a bezout.x p;
    multiples_product a p (p - 1Z);
    interval_product_positive (p - 1Z);
    interval_product_nonzero p (p - 1Z);
    let f = product (interval (p - 1Z)) in
    let b = power a (p - 1Z) in
    equal_remainders (b * f) f p;
    prime_cancel p f (b - 1Z);
    equal_remainders b 1Z p;
    ()
  end

let (fermat @ total) (a : t) (p : t) :
    {u : unit | if prime p then power a p mod p = a mod p else true} =
  prime_def p;
  if not (prime p) then ()
  else begin
    let r = a mod p in
    reduce_power a p p;
    power_def r p;
    reduce_product r (power r (p - 1Z)) p;
    if r = 0Z then ()
    else begin fermat_little r p; () end
  end

let (power_one @ total) (a : t) : {u : unit | power a 1Z = a} =
  power_def a 0Z; power_def a 1Z;
  ()

let (period_step @ total) (a : t) (p : t) (t : t) :
    {u : unit | if p > 0Z && t >= 0Z && power a p mod p = a mod p then
      power a (t + p) mod p = power a (t + 1Z) mod p else true} =
  power_add a t p;
  power_add a t 1Z;
  power_one a;
  reduce_product (power a t) (power a p) p;
  reduce_product (power a t) a p;
  ()

let rec (fermat_period @ total) : (a : t) -> (p : t) -> (k : t) ->
    {u : unit | if prime p && k >= 0Z then
      power a (1Z + k * (p - 1Z)) mod p = a mod p else true} = fun a p k ->
  prime_def p;
  if not (prime p) || k < 0Z then ()
  else if k = 0Z then begin power_one a; () end
  else begin
    fermat_period a p (k - 1Z);
    fermat a p;
    let t = (k - 1Z) * (p - 1Z) in
    period_step a p t;
    ()
  end
[@@decreases k]

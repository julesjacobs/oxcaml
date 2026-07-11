(* Exact overflow-guarded rationals. See rational.mli.

   The soundness-critical property: no operation ever wraps silently. Every native-int
   add/sub/mul/neg goes through a guarded primitive that raises [Overflow] rather than
   return a wrapped value; all public operations are built from these, so a normalized [t]
   never encodes a value it does not mathematically equal. *)

exception Overflow

(* ---- guarded native-int primitives ---- *)

let add_int a b =
  let r = a + b in
  if (b > 0 && r < a) || (b < 0 && r > a) then raise Overflow else r
;;

let sub_int a b =
  let r = a - b in
  if (b < 0 && r < a) || (b > 0 && r > a) then raise Overflow else r
;;

let neg_int a = if a = min_int then raise Overflow else -a

let mul_int a b =
  let r = a * b in
  if a <> 0 && (r / a <> b || (a = -1 && b = min_int) || (b = -1 && a = min_int))
  then raise Overflow
  else r
;;

let abs_int a = if a = min_int then raise Overflow else abs a
let rec gcd_pos a b = if b = 0 then a else gcd_pos b (a mod b)

(* gcd of magnitudes; result is >= 0 and 0 only if both are 0. *)
let gcd a b = gcd_pos (abs_int a) (abs_int b)

(* ---- the type ---- *)

type t =
  { num : int
  ; den : int (* invariant: den > 0, gcd(|num|,den) = 1 *)
  }

let num t = t.num
let den t = t.den

(* [make] normalizes: den forced positive, then divided through by the gcd. Inputs come
   from guarded arithmetic, but sign-flipping and gcd division are themselves guarded (neg
   min_int, abs min_int). *)
let make num den =
  if den = 0 then invalid_arg "Rational: zero denominator";
  let num, den = if den < 0 then neg_int num, neg_int den else num, den in
  let g = gcd num den in
  let g = if g = 0 then 1 else g in
  { num = num / g; den = den / g }
;;

let zero = { num = 0; den = 1 }
let one = { num = 1; den = 1 }
let of_int n = { num = n; den = 1 }
let of_frac num den = make num den
let is_zero t = t.num = 0
let is_int t = t.den = 1
let sign t = compare t.num 0

(* a/b + c/d = (a*d + c*b) / (b*d) *)
let add x y =
  let n = add_int (mul_int x.num y.den) (mul_int y.num x.den) in
  let d = mul_int x.den y.den in
  make n d
;;

let sub x y =
  let n = sub_int (mul_int x.num y.den) (mul_int y.num x.den) in
  let d = mul_int x.den y.den in
  make n d
;;

let mul x y = make (mul_int x.num y.num) (mul_int x.den y.den)

let div x y =
  if y.num = 0 then invalid_arg "Rational.div: division by zero";
  make (mul_int x.num y.den) (mul_int x.den y.num)
;;

let neg t = { num = neg_int t.num; den = t.den }
let abs t = { num = abs_int t.num; den = t.den }

(* compare a/b to c/d, b,d>0: sign of a*d - c*b. Cross-multiplication is guarded, so a
   comparison of astronomically large rationals raises rather than mis-orders. *)
let compare x y =
  let l = mul_int x.num y.den in
  let r = mul_int y.num x.den in
  Int.compare l r
;;

let equal x y = x.num = y.num && x.den = y.den
let min x y = if compare x y <= 0 then x else y
let max x y = if compare x y >= 0 then x else y

(* den > 0, so [num mod den] carries the sign of num. *)
let floor t =
  let q = t.num / t.den
  and r = t.num mod t.den in
  if r < 0 then sub_int q 1 else q
;;

let ceil t =
  let q = t.num / t.den
  and r = t.num mod t.den in
  if r > 0 then add_int q 1 else q
;;

let to_string t =
  if t.den = 1 then string_of_int t.num else Printf.sprintf "%d/%d" t.num t.den
;;

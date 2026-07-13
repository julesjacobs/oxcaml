(* Exact rationals, two-tier (core-bignum W2). See rational.mli.

   Soundness-critical property: no operation ever wraps silently and no value is ever
   mis-represented. Values are one of:
   - [Small { num; den }] — the pre-W2 native-int representation (den>0,
     gcd(|num|,den)=1), whose arithmetic is byte-for-byte the old guarded-int code (the
     FAST PATH); or
   - [Big { num; den }] — arbitrary-precision {!Bigint} numerator/denominator (den>0,
     gcd=1), used only when the native path would overflow int63.

   Tier discipline (core-bignum-review.md R1/R5):
   - INTERNAL arithmetic ([add]/[sub]/[mul]/[div]/[neg]/[abs]/[compare]) NEVER raises: a
     [Small ⊗ Small] op runs the guarded int primitives and, on [Overflow], PROMOTES both
     operands to [Big] and redoes the WHOLE op in [Bigint] (no partial-Small intermediate
     leaks), then normalizes and DEMOTES back to [Small] iff the result fits int63.
   - CANONICAL-DEMOTE invariant: fits-int63 ⟺ [Small]. A [Big] never holds a value that
     fits int63, so a value has ONE representation — [compare]/[equal] are value-correct.
   - The only ops that return a native [int] — [num]/[floor]/[ceil] (and [den]) — are the
     OUTPUT-PROJECTION boundary (R1). They raise [Overflow] iff the (integer) value does
     not fit int63. Callers at model-extraction / B&B branch-bound sinks keep degrading
     that to [unknown] (retain the poison exactly there); they must NEVER truncate. *)

(* [Bigint] now lives in [oxsmt_core] (it also backs core term coefficients); name it
   unqualified here as before. *)
module Bigint = Oxsmt_core.Bigint

exception Overflow

(* ---- guarded native-int primitives (unchanged; the promotion TRIGGER) ---- *)

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

(* gcd of magnitudes; result is >= 0 and 0 only if both are 0. Guarded (abs min_int). *)
let gcd a b = gcd_pos (abs_int a) (abs_int b)

(* ---- the type ---- *)

type small =
  { num : int
  ; den : int (* invariant: den > 0, gcd(|num|,den) = 1 *)
  }

type t =
  | Small of small
  | Big of
      { num : Bigint.t
      ; den : Bigint.t (* invariant: den > 0, gcd = 1, NOT both fitting int63 *)
      }

(* ---- constructors / normalization ---- *)

(* Today's int normalize, RAISING [Overflow] on any int boundary (den<0 flip on min_int,
   gcd abs on min_int). Wrapped as [Small]; the caller's try/promote arm handles the
   raise. *)
let small_make_raise num den =
  let num, den = if den < 0 then neg_int num, neg_int den else num, den in
  let g = gcd num den in
  let g = if g = 0 then 1 else g in
  Small { num = num / g; den = den / g }
;;

(* Normalize a Bigint fraction (force den>0, divide by gcd) then DEMOTE to [Small] iff
   both fit int63. [d <> 0] required. This is the sole producer of [Big] values, and the
   place the canonical-demote invariant (fits ⟺ Small) is established. *)
let bnorm_demote num den =
  let num, den =
    if Bigint.sign den < 0 then Bigint.neg num, Bigint.neg den else num, den
  in
  let g = Bigint.gcd num den in
  let num = fst (Bigint.divmod num g) in
  let den = fst (Bigint.divmod den g) in
  match Bigint.to_int_opt num, Bigint.to_int_opt den with
  | Some n, Some d -> Small { num = n; den = d }
  | _ -> Big { num; den }
;;

let to_big = function
  | Small a -> Bigint.of_int a.num, Bigint.of_int a.den
  | Big b -> b.num, b.den
;;

let zero = Small { num = 0; den = 1 }
let one = Small { num = 1; den = 1 }
let of_int n = Small { num = n; den = 1 }

(* Integer from an arbitrary-precision [Bigint] (den = 1); demotes to [Small] iff it fits
   int63 (canonical-demote invariant). The ingestion path for core term coefficients that
   exceed int63. *)
let of_bigint n = bnorm_demote n Bigint.one

let make num den =
  if den = 0 then invalid_arg "Rational: zero denominator";
  try small_make_raise num den with
  | Overflow -> bnorm_demote (Bigint.of_int num) (Bigint.of_int den)
;;

let of_frac num den = make num den

let num = function
  | Small a -> a.num
  | Big b ->
    (match Bigint.to_int_opt b.num with
     | Some n -> n
     | None -> raise Overflow)
;;

let den = function
  | Small a -> a.den
  | Big b ->
    (match Bigint.to_int_opt b.den with
     | Some d -> d
     | None -> raise Overflow)
;;

let is_zero = function
  | Small a -> a.num = 0
  | Big b -> Bigint.is_zero b.num (* unreachable: zero fits, so it is always Small *)
;;

let is_int = function
  | Small a -> a.den = 1
  | Big b -> Bigint.equal b.den Bigint.one
;;

let sign = function
  | Small a -> compare a.num 0
  | Big b -> Bigint.sign b.num
;;

(* ---- Big-tier arithmetic: cross-multiply then normalize+demote. NOTE: an earlier
   revision cross-cancelled gcd(den,den) before the cross-multiply (review R9, to bound
   intermediate size). Measurement (logs/core-bignum-measurement.md) shows Big values on
   the bucket-1 population stay <= 5 limbs (~155 bits) — the demote-back keeps them small
   — so intermediates never balloon, and the extra gcd(s) per op that cross-cancel adds
   dominated the cost (coef-size-100 spent 6.1M gcds). Removed; [bnorm_demote]'s single
   normalization keeps every result canonical, and Bigint's native-int gcd/divmod fast
   path (<= 2 limbs) makes it cheap. R9's huge-intermediate premise is not borne out on
   this population; revisit only if a future file shows a real growth tail. ---- *)

let big_add (an, ad) (bn, bd) =
  bnorm_demote (Bigint.add (Bigint.mul an bd) (Bigint.mul bn ad)) (Bigint.mul ad bd)
;;

let big_sub (an, ad) (bn, bd) =
  bnorm_demote (Bigint.sub (Bigint.mul an bd) (Bigint.mul bn ad)) (Bigint.mul ad bd)
;;

let big_mul (an, ad) (bn, bd) = bnorm_demote (Bigint.mul an bn) (Bigint.mul ad bd)

let big_div (an, ad) (bn, bd) =
  (* (an/ad) / (bn/bd) = (an*bd) / (ad*bn) *)
  bnorm_demote (Bigint.mul an bd) (Bigint.mul ad bn)
;;

let big_compare (an, ad) (bn, bd) =
  (* ad, bd > 0, so sign is preserved by the cross-multiply. *)
  Bigint.compare (Bigint.mul an bd) (Bigint.mul bn ad)
;;

(* ---- public arithmetic: Small fast path + whole-op promotion ---- *)

(* Integer fast path (both operands den=1): the result denominator is 1 and gcd(n,1)=1, so
   the cross-multiply and the [small_make_raise] gcd normalization are both unnecessary —
   the value is already canonical. This is the dominant LIA operand shape (integer
   coefficients, bounds, δ-constants); it is a pure special case of the general formula,
   so the produced [Small] is bit-identical to the general path (guarded by the property /
   brute-force / differential oracles). Overflow still promotes to [Big] exactly. *)

let add x y =
  match x, y with
  | Small a, Small b when a.den = 1 && b.den = 1 ->
    (try Small { num = add_int a.num b.num; den = 1 } with
     | Overflow -> big_add (to_big x) (to_big y))
  | Small a, Small b ->
    (try
       let n = add_int (mul_int a.num b.den) (mul_int b.num a.den) in
       small_make_raise n (mul_int a.den b.den)
     with
     | Overflow -> big_add (to_big x) (to_big y))
  | _ -> big_add (to_big x) (to_big y)
;;

let sub x y =
  match x, y with
  | Small a, Small b when a.den = 1 && b.den = 1 ->
    (try Small { num = sub_int a.num b.num; den = 1 } with
     | Overflow -> big_sub (to_big x) (to_big y))
  | Small a, Small b ->
    (try
       let n = sub_int (mul_int a.num b.den) (mul_int b.num a.den) in
       small_make_raise n (mul_int a.den b.den)
     with
     | Overflow -> big_sub (to_big x) (to_big y))
  | _ -> big_sub (to_big x) (to_big y)
;;

let mul x y =
  match x, y with
  | Small a, Small b when a.den = 1 && b.den = 1 ->
    (try Small { num = mul_int a.num b.num; den = 1 } with
     | Overflow -> big_mul (to_big x) (to_big y))
  | Small a, Small b ->
    (try small_make_raise (mul_int a.num b.num) (mul_int a.den b.den) with
     | Overflow -> big_mul (to_big x) (to_big y))
  | _ -> big_mul (to_big x) (to_big y)
;;

let div x y =
  if is_zero y then invalid_arg "Rational.div: division by zero";
  match x, y with
  | Small a, Small b ->
    (try small_make_raise (mul_int a.num b.den) (mul_int a.den b.num) with
     | Overflow -> big_div (to_big x) (to_big y))
  | _ -> big_div (to_big x) (to_big y)
;;

let neg = function
  | Small a as x ->
    (try Small { num = neg_int a.num; den = a.den } with
     | Overflow ->
       let n, d = to_big x in
       bnorm_demote (Bigint.neg n) d)
  | Big b -> Big { num = Bigint.neg b.num; den = b.den }
;;

let abs = function
  | Small a as x ->
    (try Small { num = abs_int a.num; den = a.den } with
     | Overflow ->
       let n, d = to_big x in
       bnorm_demote (Bigint.abs n) d)
  | Big b -> Big { num = Bigint.abs b.num; den = b.den }
;;

(* Value-based (R5/R6): never raises; promotes to a common tier on Small overflow. Integer
   fast path (both den=1): a direct [Int.compare a.num b.num] — no cross-multiply, so no
   overflow possible and no trap frame. Identical result to the general path
   ([Int.compare (a.num*1) (b.num*1)]). *)
let compare x y =
  match x, y with
  | Small a, Small b when a.den = 1 && b.den = 1 -> Int.compare a.num b.num
  | Small a, Small b ->
    (try Int.compare (mul_int a.num b.den) (mul_int b.num a.den) with
     | Overflow -> big_compare (to_big x) (to_big y))
  | _ -> big_compare (to_big x) (to_big y)
;;

(* Value-based equality. Small/Small is a fast structural compare (canonical ⇒ structural
   = value equality); anything else routes through the value-based [compare], so a (bug-)
   missed demotion is a perf wart, never a wrong [is_zero]/pivot/Farkas result. *)
let equal x y =
  match x, y with
  | Small a, Small b -> a.num = b.num && a.den = b.den
  | _ -> compare x y = 0
;;

let min x y = if compare x y <= 0 then x else y
let max x y = if compare x y >= 0 then x else y

(* ---- output projection to native int (R1): raise [Overflow] iff the integer value does
   not fit int63; NEVER truncate. ---- *)

let floor = function
  | Small a ->
    let q = a.num / a.den
    and r = a.num mod a.den in
    if r < 0 then sub_int q 1 else q
  | Big b ->
    let q, r = Bigint.divmod b.num b.den in
    let q = if Bigint.sign r < 0 then Bigint.sub q Bigint.one else q in
    (match Bigint.to_int_opt q with
     | Some n -> n
     | None -> raise Overflow)
;;

let ceil = function
  | Small a ->
    let q = a.num / a.den
    and r = a.num mod a.den in
    if r > 0 then add_int q 1 else q
  | Big b ->
    let q, r = Bigint.divmod b.num b.den in
    let q = if Bigint.sign r > 0 then Bigint.add q Bigint.one else q in
    (match Bigint.to_int_opt q with
     | Some n -> n
     | None -> raise Overflow)
;;

(* ---- decimal string (R7 cert grammar): "num" when den=1, else "num/den"; num/den each
   in canonical decimal (no leading zeros, sign on numerator, den>0). Same grammar in both
   tiers. ---- *)
let to_string = function
  | Small a ->
    if a.den = 1 then string_of_int a.num else Printf.sprintf "%d/%d" a.num a.den
  | Big b ->
    if Bigint.equal b.den Bigint.one
    then Bigint.to_string b.num
    else Printf.sprintf "%s/%s" (Bigint.to_string b.num) (Bigint.to_string b.den)
;;

(* Parse the R7 grammar: "num" or "num/den" (decimal, via {!Bigint.of_string}'s strict
   parse), then normalize+demote. Lenient on non-canonical input (e.g. "6/4" → 3/2); exact
   round-trip with {!to_string} (which emits canonical). [den > 0] required. *)
let of_string s =
  match String.index_opt s '/' with
  | None -> bnorm_demote (Bigint.of_string s) Bigint.one
  | Some i ->
    if i = 0 || i = String.length s - 1
    then invalid_arg "Rational.of_string: malformed fraction";
    let n = Bigint.of_string (String.sub s 0 i) in
    let d = Bigint.of_string (String.sub s (i + 1) (String.length s - i - 1)) in
    if Bigint.sign d <= 0 then invalid_arg "Rational.of_string: nonpositive denominator";
    bnorm_demote n d
;;

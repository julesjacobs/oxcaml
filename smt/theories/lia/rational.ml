(* Exact rationals in ONE WORD (Zarith [Z]-style). See rational.mli.

   Soundness-critical property: no operation ever wraps silently and no value is ever
   mis-represented. A value ({!Rational_word.t}) is one of:
   - an IMMEDIATE tagged [int] — an integer with den = 1 that fits int63; the tagged int
     IS the numerator. This is the dominant LIA operand shape (integer coefficients,
     bounds, δ-constants) and its arithmetic runs inline and ALLOCATION-FREE (the FAST
     PATH); or
   - a POINTER to a {!Rational_word.block}: [Frac { n; d }] — a small fraction (d > 1,
     both fit int63), whose arithmetic is the native-int guarded cross-multiply (the old
     [Small]-fraction path); or [Big { num; den }] — arbitrary precision, used only when a
     component exceeds int63.

   The immediate + [Frac] pair is exactly the pre-Zarith native-int [Small] tier, split by
   denominator so the integer case is a zero-alloc immediate; [Frac]/[Big] preserve the
   old [Small]/[Big] fraction and bignum behaviour byte-for-byte (guarded overflow →
   promote).

   Tier discipline (core-bignum-review.md R1/R5):
   - INTERNAL arithmetic ([add]/[sub]/[mul]/[div]/[neg]/[abs]/[compare]) NEVER raises: a
     native op (both operands immediate or [Frac]) runs the guarded int primitives and, on
     [Overflow], PROMOTES both operands to [Bigint] and redoes the WHOLE op arbitrary-
     precision (no partial native-int intermediate leaks), then normalizes and DEMOTES
     back to the smallest form. Any op touching a [Big] routes straight to the [Bigint]
     path.
   - CANONICAL-DEMOTE invariant (fits-int63-integer ⟺ immediate; small fraction ⟺ [Frac];
     else ⟺ [Big]): a value has ONE physical form, so [compare]/[equal] are value-correct
     and [to_string] is well-defined. [bnorm_demote]/[small_make_raise] are the sole
     producers and the place this invariant is established.
   - The only ops that return a native [int] — [num]/[den]/[floor]/[ceil] — are the
     OUTPUT-PROJECTION boundary (R1). They raise [Overflow] iff the (integer) value does
     not fit int63. Callers at model-extraction / B&B branch-bound sinks keep degrading
     that to [unknown] (retain the poison exactly there); they must NEVER truncate.

   This module is the SOLE client of {!Rational_word} and the ONLY place the
   representation invariant lives; it never itself names [Obj]. Do NOT use polymorphic
   [(=)] / [Stdlib.compare] / [Hashtbl.hash] / [Marshal] on [t] — the representation is
   mixed (immediates interleaved with pointers), so structural [compare] mis-orders an
   immediate against a block (see rational_word.mli); use [compare]/[equal] (value-based,
   R5). *)

module Bigint = Oxsmt_core.Bigint
module W = Rational_word

exception Overflow

(* ---- guarded native-int primitives (the promotion TRIGGER; unchanged from the
   pre-Zarith two-tier code, so the fast path is byte-for-byte the old guarded-int
   arithmetic) ---- *)

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

type t = W.t

(* ---- constructors / normalization ---- *)

(* Normalize a Bigint fraction (force den>0, divide by gcd) then DEMOTE to the smallest
   form: a fits-int63 integer -> immediate; a fraction with both components fitting int63
   -> [Frac]; else -> [Big]. [d <> 0] required. Sole [Bigint]-side canonical producer. *)
let bnorm_demote num den =
  let num, den =
    if Bigint.sign den < 0 then Bigint.neg num, Bigint.neg den else num, den
  in
  let g = Bigint.gcd num den in
  let num = fst (Bigint.divmod num g) in
  let den = fst (Bigint.divmod den g) in
  if Bigint.equal den Bigint.one
  then (
    match Bigint.to_int_opt num with
    | Some n -> W.of_int_unchecked n
    | None -> W.of_block (W.Big { num; den }))
  else (
    match Bigint.to_int_opt num, Bigint.to_int_opt den with
    | Some n, Some d -> W.of_block (W.Frac { n; d })
    | _ -> W.of_block (W.Big { num; den }))
;;

(* The native-int normalize, RAISING [Overflow] on any int boundary (den<0 flip on
   min_int, gcd abs on min_int). den = 1 collapses to the immediate; a reduced fraction
   (den > 1, both components native) is a [Frac]. The caller's try/promote arm handles the
   raise, so e.g. a min_int numerator (gcd's [abs_int] raises) promotes to [Big] exactly
   as the pre-Zarith code did. *)
let small_make_raise num den =
  let num, den = if den < 0 then neg_int num, neg_int den else num, den in
  let g = gcd num den in
  let g = if g = 0 then 1 else g in
  let n = num / g
  and d = den / g in
  if d = 1 then W.of_int_unchecked n else W.of_block (W.Frac { n; d })
;;

(* Native (numerator, denominator) if the value is immediate or a [Frac]; [None] if [Big].
   Allocates a [Some] in the fraction/mixed path only — never on the immediate fast path,
   which never calls this. *)
let small_parts x =
  if W.is_immediate x
  then Some (W.to_int_unchecked x, 1)
  else (
    match W.to_block x with
    | W.Frac f -> Some (f.n, f.d)
    | W.Big _ -> None)
;;

let to_big x =
  if W.is_immediate x
  then Bigint.of_int (W.to_int_unchecked x), Bigint.one
  else (
    match W.to_block x with
    | W.Frac f -> Bigint.of_int f.n, Bigint.of_int f.d
    | W.Big b -> b.num, b.den)
;;

let zero = W.of_int_unchecked 0
let one = W.of_int_unchecked 1
let of_int n = W.of_int_unchecked n

(* Integer from an arbitrary-precision [Bigint] (den = 1); demotes to the immediate form
   iff it fits int63 (canonical-demote invariant). The ingestion path for core term
   coefficients that exceed int63. *)
let of_bigint n = bnorm_demote n Bigint.one

let make num den =
  if den = 0 then invalid_arg "Rational: zero denominator";
  try small_make_raise num den with
  | Overflow -> bnorm_demote (Bigint.of_int num) (Bigint.of_int den)
;;

let of_frac num den = make num den

let num x =
  if W.is_immediate x
  then W.to_int_unchecked x
  else (
    match W.to_block x with
    | W.Frac f -> f.n
    | W.Big b ->
      (match Bigint.to_int_opt b.num with
       | Some n -> n
       | None -> raise Overflow))
;;

let den x =
  if W.is_immediate x
  then 1
  else (
    match W.to_block x with
    | W.Frac f -> f.d
    | W.Big b ->
      (match Bigint.to_int_opt b.den with
       | Some d -> d
       | None -> raise Overflow))
;;

(* Zero is the fits-int63 integer 0, hence always the immediate 0; no block is ever zero
   (a [Frac] has d > 1 so a nonzero numerator, a [Big] has a >int63 component). *)
let is_zero x = W.is_immediate x && W.to_int_unchecked x = 0

let is_int x =
  if W.is_immediate x
  then true
  else (
    match W.to_block x with
    | W.Frac _ -> false
    | W.Big b -> Bigint.equal b.den Bigint.one)
;;

let sign x =
  if W.is_immediate x
  then compare (W.to_int_unchecked x) 0
  else (
    match W.to_block x with
    | W.Frac f -> compare f.n 0
    | W.Big b -> Bigint.sign b.num)
;;

(* ---- Big-tier arithmetic: cross-multiply then normalize+demote. As in the pre-Zarith
   code, [bnorm_demote]'s single normalization keeps every result canonical; Bigint's
   native-int gcd/divmod fast path (<= 2 limbs) makes the small-magnitude cases cheap, so
   no cross-cancellation before the cross-multiply is warranted on this population (review
   R9; logs/core-bignum-measurement.md). ---- *)

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

(* ---- public arithmetic: immediate (integer) zero-alloc fast path, then the native
   fraction cross-multiply, then whole-op promotion to [Bigint].

   immediate ⊗ immediate: both denominators are 1, so the result denominator is 1 and the
   value is already canonical — a single guarded native op, NO allocation.
   immediate/[Frac] ⊗ immediate/[Frac]: the guarded native cross-multiply +
   [small_make_raise] (identical to the pre-Zarith [Small] arithmetic). Overflow promotes
   to [Big] exactly; any [Big] operand routes straight to the [Bigint] path. All arms
   produce a value bit-identical to the pure-[Bigint] computation (guarded by the
   differential oracle). ---- *)

let add x y =
  if W.is_immediate x && W.is_immediate y
  then (
    try W.of_int_unchecked (add_int (W.to_int_unchecked x) (W.to_int_unchecked y)) with
    | Overflow -> big_add (to_big x) (to_big y))
  else (
    match small_parts x, small_parts y with
    | Some (an, ad), Some (bn, bd) ->
      (try small_make_raise (add_int (mul_int an bd) (mul_int bn ad)) (mul_int ad bd) with
       | Overflow -> big_add (to_big x) (to_big y))
    | _ -> big_add (to_big x) (to_big y))
;;

let sub x y =
  if W.is_immediate x && W.is_immediate y
  then (
    try W.of_int_unchecked (sub_int (W.to_int_unchecked x) (W.to_int_unchecked y)) with
    | Overflow -> big_sub (to_big x) (to_big y))
  else (
    match small_parts x, small_parts y with
    | Some (an, ad), Some (bn, bd) ->
      (try small_make_raise (sub_int (mul_int an bd) (mul_int bn ad)) (mul_int ad bd) with
       | Overflow -> big_sub (to_big x) (to_big y))
    | _ -> big_sub (to_big x) (to_big y))
;;

let mul x y =
  if W.is_immediate x && W.is_immediate y
  then (
    try W.of_int_unchecked (mul_int (W.to_int_unchecked x) (W.to_int_unchecked y)) with
    | Overflow -> big_mul (to_big x) (to_big y))
  else (
    match small_parts x, small_parts y with
    | Some (an, ad), Some (bn, bd) ->
      (try small_make_raise (mul_int an bn) (mul_int ad bd) with
       | Overflow -> big_mul (to_big x) (to_big y))
    | _ -> big_mul (to_big x) (to_big y))
;;

let div x y =
  if is_zero y then invalid_arg "Rational.div: division by zero";
  if W.is_immediate x && W.is_immediate y
  then (
    (* (a/1) / (b/1) = a/b, normalized; b <> 0. *)
    try small_make_raise (W.to_int_unchecked x) (W.to_int_unchecked y) with
    | Overflow -> big_div (to_big x) (to_big y))
  else (
    match small_parts x, small_parts y with
    | Some (an, ad), Some (bn, bd) ->
      (try small_make_raise (mul_int an bd) (mul_int ad bn) with
       | Overflow -> big_div (to_big x) (to_big y))
    | _ -> big_div (to_big x) (to_big y))
;;

let neg x =
  if W.is_immediate x
  then (
    try W.of_int_unchecked (neg_int (W.to_int_unchecked x)) with
    | Overflow ->
      let n, d = to_big x in
      bnorm_demote (Bigint.neg n) d)
  else (
    match W.to_block x with
    (* [Frac.n] <> min_int by invariant (a min_int numerator promotes to [Big]); negation
       preserves the magnitude and denominator, so a block stays the same arm. *)
    | W.Frac f -> W.of_block (W.Frac { n = -f.n; d = f.d })
    | W.Big b -> W.of_block (W.Big { num = Bigint.neg b.num; den = b.den }))
;;

let abs x =
  if W.is_immediate x
  then (
    try W.of_int_unchecked (abs_int (W.to_int_unchecked x)) with
    | Overflow ->
      let n, d = to_big x in
      bnorm_demote (Bigint.abs n) d)
  else (
    match W.to_block x with
    | W.Frac f -> W.of_block (W.Frac { n = abs f.n; d = f.d })
    | W.Big b -> W.of_block (W.Big { num = Bigint.abs b.num; den = b.den }))
;;

(* Value-based (R5/R6): never raises; promotes to a common tier on native overflow. Both
   immediate: a direct [Int.compare] — no cross-multiply, so no overflow and no trap
   frame. immediate/[Frac] pair: the native guarded cross-multiply. Anything with a [Big]
   cross-multiplies in Bigint. *)
let compare x y =
  if W.is_immediate x && W.is_immediate y
  then Int.compare (W.to_int_unchecked x) (W.to_int_unchecked y)
  else (
    match small_parts x, small_parts y with
    | Some (an, ad), Some (bn, bd) ->
      (try Int.compare (mul_int an bd) (mul_int bn ad) with
       | Overflow -> big_compare (to_big x) (to_big y))
    | _ -> big_compare (to_big x) (to_big y))
;;

(* Value-based equality. Both immediate is a direct int equality; anything else routes
   through the value-based [compare] (canonical-uniqueness ⇒ this is correct, and a
   bug-missed demotion would be a perf wart, never a wrong [is_zero]/pivot/Farkas result). *)
let equal x y =
  if W.is_immediate x && W.is_immediate y
  then W.to_int_unchecked x = W.to_int_unchecked y
  else compare x y = 0
;;

let min x y = if compare x y <= 0 then x else y
let max x y = if compare x y >= 0 then x else y

(* ---- output projection to native int (R1): raise [Overflow] iff the integer value does
   not fit int63; NEVER truncate. An immediate is a fits-int63 integer, so its floor/ceil
   is itself and never overflows; a [Frac] floors within int63 except at the min_int edge
   (guarded). ---- *)

let floor x =
  if W.is_immediate x
  then W.to_int_unchecked x
  else (
    match W.to_block x with
    | W.Frac f ->
      let q = f.n / f.d
      and r = f.n mod f.d in
      if r < 0 then sub_int q 1 else q
    | W.Big b ->
      let q, r = Bigint.divmod b.num b.den in
      let q = if Bigint.sign r < 0 then Bigint.sub q Bigint.one else q in
      (match Bigint.to_int_opt q with
       | Some n -> n
       | None -> raise Overflow))
;;

let ceil x =
  if W.is_immediate x
  then W.to_int_unchecked x
  else (
    match W.to_block x with
    | W.Frac f ->
      let q = f.n / f.d
      and r = f.n mod f.d in
      if r > 0 then add_int q 1 else q
    | W.Big b ->
      let q, r = Bigint.divmod b.num b.den in
      let q = if Bigint.sign r > 0 then Bigint.add q Bigint.one else q in
      (match Bigint.to_int_opt q with
       | Some n -> n
       | None -> raise Overflow))
;;

(* ---- decimal string (R7 cert grammar): "num" when den=1, else "num/den"; num/den each
   in canonical decimal (no leading zeros, sign on numerator, den>0). An immediate renders
   as a bare integer; a [Frac] as "n/d"; a [Big] as a bare Bigint (den=1) or "num/den".
   ---- *)
let to_string x =
  if W.is_immediate x
  then string_of_int (W.to_int_unchecked x)
  else (
    match W.to_block x with
    | W.Frac f -> Printf.sprintf "%d/%d" f.n f.d
    | W.Big b ->
      if Bigint.equal b.den Bigint.one
      then Bigint.to_string b.num
      else Printf.sprintf "%s/%s" (Bigint.to_string b.num) (Bigint.to_string b.den))
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

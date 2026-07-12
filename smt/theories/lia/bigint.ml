(* Hand-rolled arbitrary-precision signed integers. See bigint.mli.

   Stdlib-only (INVARIANTS.md I3): no Zarith, no external deps. This is the [Big]-tier
   fallback arithmetic for {!Rational} (core-bignum W2): when the guarded native-int fast
   path overflows int63, operands promote here and the op is redone exactly.

   Representation (core-bignum-review.md R3): sign-magnitude, little-endian base-2^31 limbs.
     - [radix = 2^31]; every limb is in [0, radix).
     - CANONICAL invariants, enforced by every constructor:
       * no trailing (most-significant) zero limbs ([mag] has no leading zeros);
       * a UNIQUE zero: the value 0 is exactly [{ sign = 0; mag = [||] }] — never [-0],
         never [[|0|]];
       * [sign] is [-1 | 0 | +1], and [sign = 0] iff [mag = [||]].
     These make [compare]/[equal] and the [to_string] decimal grammar well-defined and make
     a mis-canonicalized value unconstructible.

   Radix choice (R3): 2^31, NOT 2^62. OCaml's native [int] is 63-bit, so there is no
   double-width product; base-2^31 keeps every intermediate within [int]. The load-bearing
   bound is proved at [mag_mul] below: the schoolbook inner accumulator never exceeds
   [radix^2 - 1 = 2^62 - 1 = max_int]. *)

let radix_bits = 31
let radix = 1 lsl radix_bits (* 2^31 *)
let mask = radix - 1

type t =
  { sign : int (* -1 | 0 | +1 ; sign = 0 iff mag = [||] *)
  ; mag :
      int array (* little-endian base-2^31 limbs, each in [0, radix), no trailing 0s *)
  }

(* ---- magnitude helpers (unsigned; canonical = no trailing zero limbs) ---- *)

(* Drop most-significant zero limbs so the array is canonical. Returns [[||]] for zero. *)
let trim (a : int array) : int array =
  let n = ref (Array.length a) in
  while !n > 0 && a.(!n - 1) = 0 do
    decr n
  done;
  if !n = Array.length a then a else Array.sub a 0 !n
;;

(* The single constructor: canonicalize a (sign, magnitude) pair. If the magnitude is zero
   the value is the unique zero regardless of the sign intent. *)
let mk sign mag =
  let mag = trim mag in
  if Array.length mag = 0
  then { sign = 0; mag = [||] }
  else { sign = (if sign >= 0 then 1 else -1); mag }
;;

let zero = { sign = 0; mag = [||] }
let is_zero t = t.sign = 0
let sign t = t.sign

(* Unsigned comparison of canonical magnitudes: longer is larger; else most-significant
   limb down. *)
let mag_compare a b =
  let la = Array.length a
  and lb = Array.length b in
  if la <> lb
  then Int.compare la lb
  else (
    let rec go i =
      if i < 0 then 0 else if a.(i) <> b.(i) then Int.compare a.(i) b.(i) else go (i - 1)
    in
    go (la - 1))
;;

let mag_add a b =
  let la = Array.length a
  and lb = Array.length b in
  let n = max la lb in
  let r = Array.make (n + 1) 0 in
  let carry = ref 0 in
  for i = 0 to n - 1 do
    let s = (if i < la then a.(i) else 0) + (if i < lb then b.(i) else 0) + !carry in
    r.(i) <- s land mask;
    carry := s lsr radix_bits
  done;
  r.(n) <- !carry;
  trim r
;;

(* Unsigned subtract, REQUIRES a >= b (checked by callers via [mag_compare]). *)
let mag_sub a b =
  let la = Array.length a
  and lb = Array.length b in
  let r = Array.make la 0 in
  let borrow = ref 0 in
  for i = 0 to la - 1 do
    let d = a.(i) - (if i < lb then b.(i) else 0) - !borrow in
    if d < 0
    then (
      r.(i) <- d + radix;
      borrow := 1)
    else (
      r.(i) <- d;
      borrow := 0)
  done;
  trim r
;;

(* Schoolbook multiply. SAFETY (R3): the inner accumulator is t = r.(i+j) + a.(i)*b.(j) +
   carry with r.(i+j) < radix, a.(i)*b.(j) <= (radix-1)^2, and (by induction) carry <=
   radix-1. Hence t <= (radix-1) + (radix-1)^2 + (radix-1) = radix^2 - 1 = 2^62 - 1 =
   max_int, so no native overflow, and the next carry = t lsr 31 <= radix-1 preserves the
   induction. The carry-out lands in r.(i+lb), which no prior step has written (it holds
   0), so it stays < radix too. *)
let mag_mul a b =
  let la = Array.length a
  and lb = Array.length b in
  if la = 0 || lb = 0
  then [||]
  else (
    let r = Array.make (la + lb) 0 in
    for i = 0 to la - 1 do
      let carry = ref 0 in
      let ai = a.(i) in
      for j = 0 to lb - 1 do
        let t = r.(i + j) + (ai * b.(j)) + !carry in
        r.(i + j) <- t land mask;
        carry := t lsr radix_bits
      done;
      r.(i + lb) <- r.(i + lb) + !carry
    done;
    trim r)
;;

(* ---- bit access (for binary long division) ---- *)

let mag_bitlen m =
  let len = Array.length m in
  if len = 0
  then 0
  else (
    let top = m.(len - 1) in
    let rec bl v acc = if v = 0 then acc else bl (v lsr 1) (acc + 1) in
    ((len - 1) * radix_bits) + bl top 0)
;;

let mag_test_bit m i =
  let limb = i / radix_bits
  and off = i mod radix_bits in
  if limb >= Array.length m then false else (m.(limb) lsr off) land 1 = 1
;;

(* [m << 1] as a magnitude (shift the whole number left by one bit). *)
let mag_shl1 m =
  let len = Array.length m in
  if len = 0
  then [||]
  else (
    let r = Array.make (len + 1) 0 in
    let carry = ref 0 in
    for i = 0 to len - 1 do
      let v = (m.(i) lsl 1) lor !carry in
      r.(i) <- v land mask;
      carry := v lsr radix_bits
    done;
    r.(len) <- !carry;
    trim r)
;;

(* Set bit 0 of a magnitude (the number is even here, so limb 0's bit 0 is 0). *)
let mag_set_low_bit m =
  if Array.length m = 0
  then [| 1 |]
  else (
    let r = Array.copy m in
    r.(0) <- r.(0) lor 1;
    r)
;;

(* Unsigned division: [a = q*b + r], [0 <= r < b], [b] nonzero. Binary long division —
   simple and obviously correct; [Big] is rare (see logs/core-bignum-measurement.md), so
   the O(bits) loop is not on the hot path. (Knuth Algorithm D is the optimization if a
   Phase-2 residency measurement flags divmod.) *)
let mag_divmod a b =
  if Array.length b = 0 then invalid_arg "Bigint.mag_divmod: divide by zero";
  if mag_compare a b < 0
  then [||], a
  else (
    let n = mag_bitlen a in
    let q = Array.make ((n / radix_bits) + 1) 0 in
    let rem = ref [||] in
    for i = n - 1 downto 0 do
      rem := mag_shl1 !rem;
      if mag_test_bit a i then rem := mag_set_low_bit !rem;
      if mag_compare !rem b >= 0
      then (
        rem := mag_sub !rem b;
        q.(i / radix_bits) <- q.(i / radix_bits) lor (1 lsl (i mod radix_bits)))
    done;
    trim q, !rem)
;;

(* Divide a magnitude by a small positive [d] in [1, radix); returns (quotient, remainder).
   SAFETY: cur = rem*radix + m.(i) with rem <= d-1 <= radix-1 and m.(i) < radix, so
   cur <= (radix-1)*radix + (radix-1) = radix^2 - 1 = max_int. *)
let mag_divmod_small m d =
  let len = Array.length m in
  let q = Array.make len 0 in
  let rem = ref 0 in
  for i = len - 1 downto 0 do
    let cur = (!rem * radix) + m.(i) in
    q.(i) <- cur / d;
    rem := cur mod d
  done;
  trim q, !rem
;;

let rec mag_gcd a b = if Array.length b = 0 then a else mag_gcd b (snd (mag_divmod a b))

(* ---- signed API ---- *)

let compare x y =
  if x.sign <> y.sign
  then Int.compare x.sign y.sign
  else (
    match x.sign with
    | 0 -> 0
    | s ->
      let c = mag_compare x.mag y.mag in
      if s > 0 then c else -c)
;;

let equal x y = compare x y = 0
let neg x = { sign = -x.sign; mag = x.mag }
let abs x = if x.sign = 0 then zero else { sign = 1; mag = x.mag }

let add x y =
  match x.sign, y.sign with
  | 0, _ -> y
  | _, 0 -> x
  | sx, sy when sx = sy -> mk sx (mag_add x.mag y.mag)
  | sx, _ ->
    (* opposite signs: larger magnitude keeps its sign, subtract the smaller *)
    let c = mag_compare x.mag y.mag in
    if c = 0
    then zero
    else if c > 0
    then mk sx (mag_sub x.mag y.mag)
    else mk (-sx) (mag_sub y.mag x.mag)
;;

let sub x y = add x (neg y)

let mul x y =
  if x.sign = 0 || y.sign = 0 then zero else mk (x.sign * y.sign) (mag_mul x.mag y.mag)
;;

(* Signed division, TRUNCATING TOWARD ZERO with the remainder carrying the DIVIDEND's sign
   (R3; matches OCaml [/]/[mod] so Rational.floor/ceil reuse their [r<0]/[r>0]
   correction). [x = q*y + r], [q = truncate(x/y)], [sign r = sign x] (or r = 0). *)
let divmod x y =
  if y.sign = 0 then invalid_arg "Bigint.divmod: division by zero";
  if x.sign = 0
  then zero, zero
  else (
    let qm, rm = mag_divmod x.mag y.mag in
    mk (x.sign * y.sign) qm, mk x.sign rm)
;;

(* Nonnegative gcd; gcd(0,0) = 0. *)
let gcd x y = mk 1 (mag_gcd x.mag y.mag)

(* ---- native-int conversions ---- *)

let of_int n =
  if n = 0
  then zero
  else if n = min_int
  then { sign = -1; mag = [| 0; 0; 1 |] } (* |min_int| = 2^62 = radix^2 *)
  else (
    let s = if n < 0 then -1 else 1 in
    let m = Stdlib.abs n in
    let rec limbs m acc =
      if m = 0 then acc else limbs (m lsr radix_bits) ((m land mask) :: acc)
    in
    { sign = s; mag = Array.of_list (List.rev (limbs m [])) })
;;

let one = of_int 1

(* [Some v] iff the value fits native int63, else [None]. A magnitude of <= 2 limbs always
   fits (max = radix^2 - 1 = max_int); a 3-limb magnitude fits only as exactly min_int. *)
let to_int_opt t =
  match t.sign with
  | 0 -> Some 0
  | s ->
    let m = t.mag in
    let len = Array.length m in
    if len <= 2
    then (
      let v = (if len >= 1 then m.(0) else 0) + if len >= 2 then m.(1) * radix else 0 in
      Some (s * v))
    else if len = 3 && m.(0) = 0 && m.(1) = 0 && m.(2) = 1 && s = -1
    then Some min_int
    else None
;;

let fits_int : t -> bool = fun t -> to_int_opt t <> None

(* ---- decimal string (R7 grammar): sign on the number only, no leading zeros, zero
   renders exactly "0". ---- *)
let to_string x =
  if x.sign = 0
  then "0"
  else (
    let chunk = 1_000_000_000 in
    (* 10^9 < radix; one divmod_small per 9 decimal digits *)
    let rec collect m acc =
      if Array.length m = 0
      then acc
      else (
        let q, r = mag_divmod_small m chunk in
        collect q (r :: acc))
    in
    match collect x.mag [] with
    | [] -> "0"
    | hd :: tl ->
      let b = Buffer.create 32 in
      if x.sign < 0 then Buffer.add_char b '-';
      Buffer.add_string b (string_of_int hd);
      List.iter (fun c -> Buffer.add_string b (Printf.sprintf "%09d" c)) tl;
      Buffer.contents b)
;;

(* Parse the R7 grammar STRICTLY: optional leading '-', then digits with no leading zero
   (except the single literal "0"); reject "-0", empty, non-digits. *)
let of_string s =
  let n = String.length s in
  if n = 0 then invalid_arg "Bigint.of_string: empty";
  let neg_flag, start = if s.[0] = '-' then true, 1 else false, 0 in
  if start >= n then invalid_arg "Bigint.of_string: no digits";
  let digits = String.sub s start (n - start) in
  String.iter
    (fun c -> if c < '0' || c > '9' then invalid_arg "Bigint.of_string: non-digit")
    digits;
  if String.length digits > 1 && digits.[0] = '0'
  then invalid_arg "Bigint.of_string: leading zero";
  if neg_flag && digits = "0" then invalid_arg "Bigint.of_string: negative zero";
  let ten = of_int 10 in
  let acc = ref zero in
  String.iter
    (fun c -> acc := add (mul !acc ten) (of_int (Char.code c - Char.code '0')))
    digits;
  if neg_flag then neg !acc else !acc
;;

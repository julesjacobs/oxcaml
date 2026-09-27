module P = Vox_table_probe

type capacity = {c : int | 16 <= c && c <= 1073741824 &&
  c land (c - 1) = 0}

(* Z3 proves this by bit-blasting the 63-bit sum against a symbolic mask:
   about 1.8M solver resource units, over the 1M slow-refinement threshold.
   Splitting it into lemmas about masking below and above [capacity] did not
   make it cheaper. *)
let[@warning "-slow-refinement"] (wrap_add @ total) (capacity : capacity)
    (value : {v : int | 0 <= v && v < capacity})
    (delta : {d : int | 0 <= d && d <= capacity}) :
    {u : unit | (value + delta) land (capacity - 1) =
      P.addmod capacity value delta} =
  P.addmod_def capacity value delta;
  ()

let (wrap_range @ total) (capacity : capacity) (value : int) :
    {u : unit | 0 <= value land (capacity - 1) &&
      value land (capacity - 1) < capacity} = ()

(* [shifted value scale half r] states [r = value lsr k] for [scale = 2^k]
   and [half = 2^(62 - k)]: [r] is [value / 2^k] for a nonnegative [value],
   and [(value + 2^63) / 2^k = (value - min_int) / 2^k + half] for a negative
   one. The bounds on [r] keep the products from wrapping. *)
let[@def transparent] (shifted @ total) (value : int) (scale : int)
    (half : int) (r : int) =
  (value >= 0 && 0 <= r && r < half
   && 0 <= value - scale * r && value - scale * r < scale)
  || (value < 0 && half <= r && r < half + half
      && 0 <= value - min_int - scale * (r - half)
      && value - min_int - scale * (r - half) < scale)

(* Logical shifts right by the constants the table uses, for its
   specifications: [( lsr )] is not total, and a predicate cannot pass a
   constant to the refined count of [Int.Refined.( lsr )]. A predicate that
   applies one knows its value only through its [_spec] lemma. *)
let (lsr3 @ total) (value : int) :
    {r : int | shifted value 8 576460752303423488 r} =
  Int.Refined.(value lsr 3)

let (lsr3_spec @ total) (value : int) :
    {u : unit | shifted value 8 576460752303423488 (lsr3 value)} @ ghost =
  ghost_ (let _ = lsr3 value in ())

let (lsr4 @ total) (value : int) :
    {r : int | shifted value 16 288230376151711744 r} =
  Int.Refined.(value lsr 4)

let (lsr4_spec @ total) (value : int) :
    {u : unit | shifted value 16 288230376151711744 (lsr4 value)} @ ghost =
  ghost_ (let _ = lsr4 value in ())

let (lsr7 @ total) (value : int) :
    {r : int | shifted value 128 36028797018963968 r} =
  Int.Refined.(value lsr 7)

let (lsr7_spec @ total) (value : int) :
    {u : unit | shifted value 128 36028797018963968 (lsr7 value)} @ ghost =
  ghost_ (let _ = lsr7 value in ())

let[@def] (scale16 @ total) (value : int) =
  let two = value + value in
  let four = two + two in
  let eight = four + four in
  eight + eight

let (split16 @ total)
    (value : {v : int | 0 <= v && v <= 1073741824}) :
    {u : unit | value = scale16 (lsr4 value) + (value land 15) &&
      0 <= lsr4 value && lsr4 value <= 67108864 &&
      0 <= value land 15 && value land 15 < 16} =
  ghost_ (lsr4_spec value); scale16_def (lsr4 value);
  ()

let (scale_addmod @ total)
    (modulus : {m : int | 0 < m && m <= 67108864})
    (value : {v : int | 0 <= v && v < modulus})
    (delta : {d : int | 0 <= d && d <= modulus}) :
    {u : unit | scale16 (P.addmod modulus value delta) =
      P.addmod (scale16 modulus) (scale16 value) (scale16 delta)} =
  P.addmod_def modulus value delta;
  scale16_def modulus; scale16_def value; scale16_def delta;
  scale16_def (P.addmod modulus value delta);
  P.addmod_def (scale16 modulus) (scale16 value) (scale16 delta);
  ()

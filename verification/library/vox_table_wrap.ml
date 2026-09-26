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

let[@def] (scale16 @ total) (value : int) =
  let two = value + value in
  let four = two + two in
  let eight = four + four in
  eight + eight

let (split16 @ total)
    (value : {v : int | 0 <= v && v <= 1073741824}) :
    {u : unit | value = scale16 (value lsr 4) + (value land 15) &&
      0 <= value lsr 4 && value lsr 4 <= 67108864 &&
      0 <= value land 15 && value land 15 < 16} =
  scale16_def (value lsr 4);
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

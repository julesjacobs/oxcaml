(* δ-rationals. See delta.mli. Overflow propagates from [Rational]. *)

type t =
  { c : Rational.t
  ; k : Rational.t
  }

let make c k = { c; k }
let of_rat c = { c; k = Rational.zero }
let c_part t = t.c
let k_part t = t.k
let zero = { c = Rational.zero; k = Rational.zero }
let add x y = { c = Rational.add x.c y.c; k = Rational.add x.k y.k }
let sub x y = { c = Rational.sub x.c y.c; k = Rational.sub x.k y.k }
let scale r t = { c = Rational.mul r t.c; k = Rational.mul r t.k }
let neg t = { c = Rational.neg t.c; k = Rational.neg t.k }

let compare x y =
  let cc = Rational.compare x.c y.c in
  if cc <> 0 then cc else Rational.compare x.k y.k
;;

let equal x y = Rational.equal x.c y.c && Rational.equal x.k y.k
let le x y = compare x y <= 0
let lt x y = compare x y < 0
let is_rational t = Rational.is_zero t.k

let to_string t =
  if is_rational t
  then Rational.to_string t.c
  else Printf.sprintf "%s + %s·d" (Rational.to_string t.c) (Rational.to_string t.k)
;;

open Oxsmt_core

module Rational = struct
  type t =
    { num : Bigint.t
    ; den : Bigint.t
    }

  let of_big_frac ~num ~den =
    if Bigint.is_zero den then invalid_arg "Value.Rational: zero denominator";
    let num, den =
      if Bigint.sign den < 0 then Bigint.neg num, Bigint.neg den else num, den
    in
    if Bigint.is_zero num
    then { num = Bigint.zero; den = Bigint.one }
    else (
      let g = Bigint.gcd (Bigint.abs num) den in
      let num, rn = Bigint.divmod num g in
      let den, rd = Bigint.divmod den g in
      if not (Bigint.is_zero rn && Bigint.is_zero rd)
      then invalid_arg "Value.Rational: non-exact normalization";
      { num; den })
  ;;

  let zero = of_big_frac ~num:Bigint.zero ~den:Bigint.one
  let of_term (q : Term.rational) = of_big_frac ~num:q.num ~den:q.den

  let add a b =
    of_big_frac
      ~num:(Bigint.add (Bigint.mul a.num b.den) (Bigint.mul b.num a.den))
      ~den:(Bigint.mul a.den b.den)
  ;;

  let mul a b = of_big_frac ~num:(Bigint.mul a.num b.num) ~den:(Bigint.mul a.den b.den)
  let compare a b = Bigint.compare (Bigint.mul a.num b.den) (Bigint.mul b.num a.den)
  let equal a b = compare a b = 0

  let to_string q =
    if Bigint.equal q.den Bigint.one
    then Bigint.to_string q.num ^ ".0"
    else Bigint.to_string q.num ^ "/" ^ Bigint.to_string q.den
  ;;
end

type t =
  | Bool of bool
  | Int of int
  | Real of Rational.t
  | BitVec of
      { width : int
      ; bits : Bigint.t
      }
  | Uninterp of Sort.t * int

let equal (a : t) (b : t) : bool =
  match a, b with
  | Bool x, Bool y -> Bool.equal x y
  | Int x, Int y -> Int.equal x y
  | Real x, Real y -> Rational.equal x y
  | BitVec x, BitVec y -> x.width = y.width && Bigint.equal x.bits y.bits
  | Uninterp (s1, i1), Uninterp (s2, i2) -> Sort.equal s1 s2 && Int.equal i1 i2
  | (Bool _ | Int _ | Real _ | BitVec _ | Uninterp _), _ -> false
;;

let to_string = function
  | Bool b -> Bool.to_string b
  | Int n -> Int.to_string n
  | Real q -> Rational.to_string q
  | BitVec { width; bits } -> Printf.sprintf "(_ bv%s %d)" (Bigint.to_string bits) width
  | Uninterp (sort, id) ->
    let sname =
      match (sort : Sort.t) with
      | Uninterpreted sym -> Symbol.name sym
      | Datatype sym -> Symbol.name sym
      | Bool -> "Bool"
      | Int _ -> "Int"
      | Real -> "Real"
      | Array _ -> "Array"
      | BitVec w -> Printf.sprintf "(_ BitVec %d)" w
    in
    Printf.sprintf "%s@%d" sname id
;;

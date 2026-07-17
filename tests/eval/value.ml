open Oxsmt_core

type t =
  | Bool of bool
  | Int of int
  | BitVec of
      { width : int
      ; bits : Bigint.t
      }
  | Uninterp of Sort.t * int

let equal (a : t) (b : t) : bool =
  match a, b with
  | Bool x, Bool y -> Bool.equal x y
  | Int x, Int y -> Int.equal x y
  | BitVec x, BitVec y -> x.width = y.width && Bigint.equal x.bits y.bits
  | Uninterp (s1, i1), Uninterp (s2, i2) -> Sort.equal s1 s2 && Int.equal i1 i2
  | (Bool _ | Int _ | BitVec _ | Uninterp _), _ -> false
;;

let to_string = function
  | Bool b -> Bool.to_string b
  | Int n -> Int.to_string n
  | BitVec { width; bits } -> Printf.sprintf "(_ bv%s %d)" (Bigint.to_string bits) width
  | Uninterp (sort, id) ->
    let sname =
      match (sort : Sort.t) with
      | Uninterpreted sym -> Symbol.name sym
      | Datatype sym -> Symbol.name sym
      | Bool -> "Bool"
      | Int _ -> "Int"
      | Array _ -> "Array"
      | BitVec w -> Printf.sprintf "(_ BitVec %d)" w
    in
    Printf.sprintf "%s@%d" sname id
;;

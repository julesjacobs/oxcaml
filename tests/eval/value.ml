open Oxsmt_core

type t =
  | Bool of bool
  | Int of int
  | Uninterp of Sort.t * int

let equal (a : t) (b : t) : bool =
  match a, b with
  | Bool x, Bool y -> Bool.equal x y
  | Int x, Int y -> Int.equal x y
  | Uninterp (s1, i1), Uninterp (s2, i2) -> Sort.equal s1 s2 && Int.equal i1 i2
  | (Bool _ | Int _ | Uninterp _), _ -> false
;;

let to_string = function
  | Bool b -> Bool.to_string b
  | Int n -> Int.to_string n
  | Uninterp (sort, id) ->
    let sname =
      match (sort : Sort.t) with
      | Uninterpreted sym -> Symbol.name sym
      | Datatype sym -> Symbol.name sym
      | Bool -> "Bool"
      | Int _ -> "Int"
      | Array _ -> "Array"
    in
    Printf.sprintf "%s@%d" sname id
;;

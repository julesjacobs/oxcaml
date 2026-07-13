type t =
  | Bool
  | Int of int_kind
  | Uninterpreted of Symbol.t
  | Datatype of Symbol.t
  | Array of t * t

and int_kind = Mathematical

let bool = Bool
let int = Int Mathematical
let uninterpreted s = Uninterpreted s
let datatype_ s = Datatype s
let array_ ~index ~element = Array (index, element)

let rec equal a b =
  match a, b with
  | Bool, Bool -> true
  | Int Mathematical, Int Mathematical -> true
  | Uninterpreted s1, Uninterpreted s2 -> Symbol.equal s1 s2
  | Datatype s1, Datatype s2 -> Symbol.equal s1 s2
  | Array (i1, e1), Array (i2, e2) -> equal i1 i2 && equal e1 e2
  | (Bool | Int _ | Uninterpreted _ | Datatype _ | Array _), _ -> false
;;

let rec hash = function
  | Bool -> 0
  | Int Mathematical -> 1
  | Uninterpreted s -> (Symbol.hash s * 3) + 2
  | Datatype s -> (Symbol.hash s * 3) + 3
  | Array (i, e) -> (((hash i * 31) + hash e) * 3) + 4
;;

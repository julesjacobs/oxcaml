type t =
  | Bool
  | Int of int_kind
  | Uninterpreted of Symbol.t
  | Datatype of Symbol.t
  | Array of t * t
  | BitVec of int
  | Real

and int_kind = Mathematical

let bool = Bool
let int = Int Mathematical
let real = Real
let uninterpreted s = Uninterpreted s
let datatype_ s = Datatype s
let array_ ~index ~element = Array (index, element)

let bitvec width =
  if width < 1 then invalid_arg "Sort.bitvec: width must be >= 1";
  BitVec width
;;

let rec equal a b =
  match a, b with
  | Bool, Bool -> true
  | Int Mathematical, Int Mathematical -> true
  | Uninterpreted s1, Uninterpreted s2 -> Symbol.equal s1 s2
  | Datatype s1, Datatype s2 -> Symbol.equal s1 s2
  | Array (i1, e1), Array (i2, e2) -> equal i1 i2 && equal e1 e2
  | BitVec w1, BitVec w2 -> w1 = w2
  | Real, Real -> true
  | (Bool | Int _ | Uninterpreted _ | Datatype _ | Array _ | BitVec _ | Real), _ -> false
;;

let rec hash = function
  | Bool -> 0
  | Int Mathematical -> 1
  | Uninterpreted s -> (Symbol.hash s * 3) + 2
  | Datatype s -> (Symbol.hash s * 3) + 3
  | Array (i, e) -> (((hash i * 31) + hash e) * 3) + 4
  | BitVec w -> (w * 7) + 5
  | Real -> 6
;;

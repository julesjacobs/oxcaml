(* TEST
 expect;
*)

type int_list = Int_list of int list

let singleton = (Int_list [1] : int_list{ _ = Int_list [1] })

[%%expect {|
type int_list = Int_list of int list
val singleton : int_list{ _ = Int_list (1 :: []) } = Int_list [1]
|}]

type tree =
  | Leaf of int
  | Branch of tree list

let leaf = (Leaf 1 : tree{ _ = Leaf 1 })
let branch = (Branch [Leaf 1] : tree{ _ = Branch [Leaf 1] })

[%%expect {|
type tree = Leaf of int | Branch of tree list
val leaf : tree{ _ = Leaf 1 } = Leaf 1
val branch : tree{ _ = Branch (Leaf 1 :: []) } = Branch [Leaf 1]
|}]

type tuple_tree =
  | Tuple_leaf of int
  | Node of (int * tuple_tree list)

let tuple_tree =
  (Node (9, [Tuple_leaf 1; Node (2, [])])
    : tuple_tree{ _ = Node (9, [Tuple_leaf 1; Node (2, [])]) })

[%%expect {|
type tuple_tree = Tuple_leaf of int | Node of (int * tuple_tree list)
val tuple_tree :
  tuple_tree{ _ = Node (9, Tuple_leaf 1 :: Node (2, []) :: []) } =
  Node (9, [Tuple_leaf 1; Node (2, [])])
|}]

let wrong_tuple_tree =
  (Node (9, [Tuple_leaf 1; Node (2, [])])
    : tuple_tree{ _ = Node (9, [Tuple_leaf 1; Node (3, [])]) })

[%%expect {|
Lines 2-3, characters 2-63:
2 | ..(Node (9, [Tuple_leaf 1; Node (2, [])])
3 |     : tuple_tree{ _ = Node (9, [Tuple_leaf 1; Node (3, [])]) })
Error: Refinement verification failed (disproved)
|}]

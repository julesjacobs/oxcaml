type tree =
  | Leaf
  | Node of tree

let witness = (Node Leaf : tree{ _ = Node Leaf })


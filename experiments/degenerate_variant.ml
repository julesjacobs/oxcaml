type 'a degenerate : immutable_data with 'a =
  | Leaf of 'a
  | Branch of ('a * 'a) degenerate

(* type 'a foo : immutable_data =
  | Leaf
  | Branch of ('a list) foo list *)
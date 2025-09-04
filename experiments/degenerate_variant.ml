type 'a degenerate : immutable_data with ('a * 'a) =
  | Leaf of 'a
  | Branch of ('a * 'a) degenerate


let rec (depth @ total) n =
  match n with Definition.Z -> 0 | Definition.S x -> 1 + depth x
let (call @ total) n = Definition.depth n

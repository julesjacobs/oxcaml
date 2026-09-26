let (predecessor @ total) = function
  | Definition.Z -> Definition.Z
  | Definition.S n -> n

let rec (depth @ total) n =
  match n with Definition.Z -> 0 | Definition.S x -> 1 + depth x

let (call_predecessor @ total) n = Definition.predecessor n
let (call_depth @ total) n = Definition.depth n

type t = Z | S of t [@@inductive]
let (predecessor @ total) = function Z -> Z | S n -> n

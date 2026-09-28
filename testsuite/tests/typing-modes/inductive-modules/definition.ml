type t = Z | S of t [@@inductive]
let (predecessor @ total) = function Z -> Z | S n -> n
let rec (depth @ total) n = match n with Z -> 0 | S x -> 1 + depth x

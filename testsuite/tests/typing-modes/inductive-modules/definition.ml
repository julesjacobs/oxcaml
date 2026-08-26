type t = Z | S of t [@@inductive]
let rec (depth @ total) n = match n with Z -> 0 | S x -> 1 + depth x

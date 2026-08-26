type t = Z | S of t [@@inductive]
val depth : t -> int @@ total

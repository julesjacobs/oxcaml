type t = Z | S of t [@@inductive]
val predecessor : t -> t @@ total

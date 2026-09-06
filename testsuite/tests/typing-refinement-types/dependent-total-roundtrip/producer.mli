type t

val observe : t @ immutable -> bool @@ total
val keep : (x : t) -> { result : unit | observe x }

val keep_immutable : (x : t) @ immutable ->
  {result : unit | observe x}

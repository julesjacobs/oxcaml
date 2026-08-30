type t

val observe : t @ immutable contended -> bool @@ total
val keep : (x : t) -> { result : unit | observe x }

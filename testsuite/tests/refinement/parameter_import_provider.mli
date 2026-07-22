type t = int

val equal : t @ local logical -> t @ local logical -> bool @@ total
val member : int -> t @ local logical -> bool @@ total

val consume :
  left:t @ logical -> right:t @ logical ->
  witness:unit{ equal left right = true } -> query:int ->
  unit{ member query left = member query right } @@ total

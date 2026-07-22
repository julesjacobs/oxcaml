type t = int

val equal : t @ local logical -> t @ local logical -> bool @@ total

val consume :
  left:t @ logical -> right:t @ logical ->
  witness:unit{ equal left right = true } -> unit @@ total

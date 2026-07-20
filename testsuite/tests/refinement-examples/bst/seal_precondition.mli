type t

val member : int -> t @ local logical -> bool @@ total

val test :
  q:int ->
  tree:t @ logical ->
  pre:unit{ member q tree = true } ->
  unit{ q = q } @@ total

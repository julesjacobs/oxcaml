val relation : int @ logical -> int @ logical -> bool @@ total

val law :
  x:int @ logical ->
  y:int @ logical ->
  unit{ relation x y = true } @@ total

val law :
  x:int{ _ = 1 } @ logical ->
  x:int{ _ = 0 } @ logical ->
  unit{ x = 0 } @@ total

type contract =
  x:int{ _ = 0 } @ logical ->
  x:int{ _ = 1 } @ logical ->
  unit{ x = 0 }

type contract =
  x:int @ logical ->
  x:int{ _ = x + 10 } @ logical ->
  unit{ x = x }

val produce : unit -> contract
val consume : contract -> unit

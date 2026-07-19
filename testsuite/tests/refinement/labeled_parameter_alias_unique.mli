type contract = x:int @ logical -> unit{ x = x + 0 }

val produce : unit -> contract
val consume : contract -> unit

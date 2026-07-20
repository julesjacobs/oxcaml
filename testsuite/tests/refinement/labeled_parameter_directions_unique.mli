val produce : unit -> (x:int @ logical -> unit{ x = x + 0 })
val consume : (x:int @ logical -> unit{ x = x + 0 }) -> unit

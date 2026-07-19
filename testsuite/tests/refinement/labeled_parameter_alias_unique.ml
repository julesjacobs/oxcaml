type contract = x:int @ logical -> unit{ x = x + 0 }

let produce () ~x:(x : int @ logical) : unit{ x = x + 0 } = ()

let consume (_ : x:int @ logical -> unit{ x = x + 0 }) = ()

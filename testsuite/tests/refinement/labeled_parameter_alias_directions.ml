type contract =
  x:int @ logical ->
  x:int{ _ = x + 10 } @ logical ->
  unit{ x = x }

let produce ()
    ~x:(first : int @ logical)
    ~x:(second : int{ _ = first + 10 } @ logical)
    : unit{ second = second } =
  ()

let consume
    (_ :
      x:int @ logical ->
      x:int{ _ = x + 10 } @ logical ->
      unit{ x = x }) =
  ()

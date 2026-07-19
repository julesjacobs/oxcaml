let law
    ~x:(first : int{ _ = 0 } @ logical)
    ~x:(_second : int{ _ = 1 } @ logical)
    : unit{ first = 0 } =
  ()

let (grow @ total) (x : {x : int | x >= 0}) : {r : int | r >= x} =
  Int.Refined.(x lsl 1)

let (shrink @ total) (x : int) : {r : int | r <= x} = Int.Refined.(x lsr 1)

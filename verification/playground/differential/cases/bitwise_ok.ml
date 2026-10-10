let (low_byte @ total) (x : int) : {r : int | 0 <= r && r <= 255} = x land 255
let (shift @ total) (x : {x : int | 0 <= x && x < 1024}) :
    {r : int | 0 <= r && r < 1048576} = Int.Refined.(x lsl 10)
let (half @ total) (x : {x : int | x >= 0}) : {r : int | 0 <= r && r <= x} =
  Int.Refined.(x asr 1)
let (sign_bit @ total) (x : {x : int | x < 0}) : {r : int | r = -1} =
  Int.Refined.(x asr 62)

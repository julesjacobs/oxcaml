external raise_false : exn -> int{ false } = "%raise"

let impossible () = raise_false Exit

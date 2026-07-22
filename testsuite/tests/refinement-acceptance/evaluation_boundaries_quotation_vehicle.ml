external raise_false : exn -> int{ false } = "%raise"

let impossible () : int{ false } = raise_false Exit

external add : a:int -> b:int -> int @@ total = "%addint"
external equal : int -> int -> bool @@ total = "%equal"
type checked = { v : int | let g = add ~b:0 in equal (g ~a:v) v }

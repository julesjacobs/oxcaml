type t = int

let zero = 0

let same () : {r : t | r === zero} = refine_ zero

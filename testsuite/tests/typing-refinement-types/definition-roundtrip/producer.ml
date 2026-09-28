let[@def] next x = x + 2
let[@def] choose b x y = if b then x + 1 else y - 1

type 'a box = Box of 'a

let[@def] box (x @ immutable) = Box x

let[@def] dependent : (x : int) -> {v : int | v = x} -> int =
  fun x y -> x

let[@def] (witnessed @ total) (x : int) =
  let refine_ proof = ghost_ (next_def x) in
  x

type ghost_identity = {x : int | ghost_ (x + 1) === x + 1}

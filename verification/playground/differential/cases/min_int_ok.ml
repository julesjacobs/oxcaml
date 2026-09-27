let (abs @ total) (x : {x : int | x > min_int}) : {r : int | r >= 0} =
  if x < 0 then - x else x
let (m @ total) () : {r : int | r = max_int} = (-1) lsr 1
let (n @ total) () : {r : int | r = min_int} = max_int + 1

let (abs @ total) (x : int) : {r : int | r >= 0} = if x < 0 then - x else x

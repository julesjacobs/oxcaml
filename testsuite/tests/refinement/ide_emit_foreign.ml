# 100 "not-the-buffer.ml"
let foreign_result (x : int{ _ >= 0 }) : int{ _ >= 0 } =
  if x = 0 then 0 else x

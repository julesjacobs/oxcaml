let direct x =
  if Direct_if_total_call_api.is_zero ~x
  then (x : int{ _ = 0 })
  else 0

let bound x =
  let result = Direct_if_total_call_api.is_zero ~x in
  if result then (x : int{ _ = 0 }) else 0


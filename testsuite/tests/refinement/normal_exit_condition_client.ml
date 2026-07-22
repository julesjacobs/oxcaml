let direct x =
  if Normal_exit_condition_api.is_zero ~x
  then (x : int{ _ = 0 })
  else 0

let reexported x =
  if Normal_exit_condition_reexport.is_zero ~x
  then (x : int{ _ = 0 })
  else 0

let probe condition key =
  if condition
  then Labeled_parameter_join_api.left ~key
  else Labeled_parameter_join_api.right ~key;
  let result : int{ _ = 0 } = key in
  result

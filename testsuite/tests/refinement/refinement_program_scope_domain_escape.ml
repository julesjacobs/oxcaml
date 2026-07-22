let escaping value =
  let local = 1 in
  (value : int{ _ = local })

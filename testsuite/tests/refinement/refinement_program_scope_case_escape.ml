let escaping = function value ->
  let local = 1 in
  (value : int{ _ = local })

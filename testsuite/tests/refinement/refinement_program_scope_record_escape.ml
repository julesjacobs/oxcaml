type 'a box = { field : 'a }

let escaping () =
  let local = 1 in
  ({ field = local } : int{ _ = local } box)

type 'a box = Box of 'a

let escaping () =
  let local = 1 in
  (Box local : int{ _ = local } box)

type 'a box = { field : 'a }

let escaping x =
  let module M : sig val x : int end = struct
    let x = x
  end in
  ({ field = M.x } : int{ _ = M.x } box)

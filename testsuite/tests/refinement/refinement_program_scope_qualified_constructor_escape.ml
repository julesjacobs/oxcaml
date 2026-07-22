type 'a box = Box of 'a

let escaping x =
  let module M : sig val x : int end = struct
    let x = x
  end in
  (Box M.x : int{ _ = M.x } box)

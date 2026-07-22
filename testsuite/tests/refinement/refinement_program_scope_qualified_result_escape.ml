let escaping x =
  let module M : sig val x : int end = struct
    let x = x
  end in
  (M.x : int{ _ = M.x })

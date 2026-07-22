let escaping value x =
  let module M : sig val x : int end = struct
    let x = x
  end in
  (value : int{ _ = M.x })

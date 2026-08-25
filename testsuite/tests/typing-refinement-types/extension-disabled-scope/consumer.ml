let escaped =
  let module Local = struct
    let flag = true
  end in
  let module M = Producer.Make (Local) in
  M.value

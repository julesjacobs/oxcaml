let[@vox.def] relation (_x : int @ logical) (_y : int @ logical) = true

let law ~(x : int @ logical) ~(y : int @ logical)
    : unit{ relation x y = true }
  =
  let _proof = relation_def x y in
  ()

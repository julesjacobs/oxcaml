type t = Empty

let[@vox.def] member (_query : int) (_tree : t @ logical) = true

let test ~(q : int) ~(tree : t @ logical)
    ~(pre : unit{ member q tree = true }) : unit{ q = q } =
  ()

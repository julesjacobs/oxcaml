type t =
  | Empty
  | Full

let empty = Empty
let[@vox.def] insert (_key : int @ logical) (_tree : t @ logical) = Full
let[@vox.def] member (_query : int @ logical) (_tree : t @ logical) = true

let prove_true query tree : unit{ member query tree = true } =
  let _ = member_def query tree in ()

let empty_law ~(key : int @ logical)
    : unit{ member key empty = true } =
  prove_true key empty

let insert_law ~(key : int @ logical) ~(tree : t @ logical)
    : unit{ member key (insert key tree) = true } =
  prove_true key (insert key tree)

let member_insert_law ~(inserted : int @ logical)
    ~(tree : t @ logical) ~(query : int @ logical)
    : unit{ member query (insert inserted tree) = true } =
  prove_true query (insert inserted tree)

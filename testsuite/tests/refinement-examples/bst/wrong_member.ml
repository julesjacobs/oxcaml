type t =
  | Empty
  | Full

let empty = Empty
let[@vox.def] insert (_key : int @ logical) (_tree : t @ logical) = Full
let[@vox.def] member (_query : int @ logical) (_tree : t @ logical) = false

let prove_false query tree : unit{ member query tree = false } =
  let _ = member_def query tree in
  ()

let empty_law ~(key : int @ logical)
    : unit{ member key empty = false } =
  prove_false key empty

let insert_law ~(key : int @ logical) ~(tree : t @ logical)
    : unit{ member key (insert key tree) = false } =
  prove_false key (insert key tree)

let member_insert_law ~(inserted : int @ logical)
    ~(tree : t @ logical) ~(query : int @ logical)
    : unit{ member query (insert inserted tree) = false } =
  prove_false query (insert inserted tree)

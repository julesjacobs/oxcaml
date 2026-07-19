type t =
  | Empty
  | Full

let empty = Empty

let[@vox.def] member (_query : int @ logical) (tree : t @ logical) =
  match tree with Empty -> false | Full -> true

let[@vox.def] insert (_key : int @ logical) (tree : t @ logical) = tree

let empty_law ~(key : int @ logical)
    : unit{ member key empty = false } =
  let _ = member_def key empty in
  ()

let insert_law ~(key : int @ logical) ~(tree : t @ logical)
    : unit{
      member key (insert key tree) = member key tree
    } =
  let _ = insert_def key tree in
  ()

let member_insert_law ~(inserted : int @ logical)
    ~(tree : t @ logical) ~(query : int @ logical)
    : unit{
      member query (insert inserted tree) = member query tree
    } =
  let _ = insert_def inserted tree in
  ()

type t =
  | Empty
  | Full

let empty = Empty

let[@vox.def] member (_query : int @ logical) (tree : t @ logical) =
  match tree with Empty -> false | Full -> true

let[@vox.def] insert (_key : int @ logical) (tree : t @ logical) = tree
let[@vox.def] equal (_left : t @ logical) (_right : t @ logical) = false

let empty_law ~(key : int)
    : unit{ member key empty = false } =
  let _ = member_def key empty in
  ()

let insert_law ~(inserted : int)
    ~(tree : t @ logical) ~(query : int)
    : unit{
      member query (insert inserted tree) = member query tree
    } =
  let _ = insert_def inserted tree in
  ()

let equal_implies_member ~(t1 : t @ logical) ~(t2 : t @ logical)
    ~(query : int)
    : unit{
      equal t1 t2 = false || member query t1 = member query t2
    } =
  let _ = equal_def t1 t2 in
  ()

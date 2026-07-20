type t =
  | Empty
  | Full

let empty = Empty
let[@vox.def] insert (_key : int @ logical) (_tree : t @ logical) = Full
let[@vox.def] member (_query : int @ logical) (_tree : t @ logical) = false
let[@vox.def] equal (_left : t @ logical) (_right : t @ logical) = false

let prove_false query tree : unit{ member query tree = false } =
  let _ = member_def query tree in
  ()

let empty_law ~(key : int)
    : unit{ member key empty = false } =
  prove_false key empty

let insert_law ~(inserted : int)
    ~(tree : t @ logical) ~(query : int)
    : unit{ member query (insert inserted tree) = false } =
  prove_false query (insert inserted tree)

let equal_implies_member ~(t1 : t @ logical) ~(t2 : t @ logical)
    ~(query : int)
    : unit{
      equal t1 t2 = false || member query t1 = member query t2
    } =
  let _ = equal_def t1 t2 in
  ()

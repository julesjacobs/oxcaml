type t =
  | Empty
  | Full

let empty = Empty

let[@vox.def] member (_query : int @ logical) (tree : t @ logical) =
  match tree with Empty -> false | Full -> true

let[@vox.def] insert (_key : int @ logical) (tree : t @ logical) = tree

let empty_has_no_zero (_ : unit @ logical)
    : unit{ member 0 empty = false } =
  let _ = member_def 0 empty in ()

let empty_has_no_one (_ : unit @ logical)
    : unit{ member 1 empty = false } =
  let _ = member_def 1 empty in ()

let insert_zero_has_zero (_ : unit @ logical)
    : unit{ member 0 (insert 0 empty) = false } =
  let _ = insert_def 0 empty in
  let _ = member_def 0 empty in ()

let insert_zero_has_no_one (_ : unit @ logical)
    : unit{ member 1 (insert 0 empty) = false } =
  let _ = insert_def 0 empty in
  let _ = member_def 1 empty in ()

let insert_one_preserves_zero (_ : unit @ logical)
    : unit{ member 0 (insert 1 (insert 0 empty)) = false } =
  let _ = insert_def 0 empty in
  let _ = insert_def 1 empty in
  let _ = member_def 0 empty in ()

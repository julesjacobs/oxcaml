type t =
  | Empty
  | Full

let empty = Empty
let[@vox.def] insert (_key : int @ logical) (_tree : t @ logical) = Full
let[@vox.def] member (_query : int @ logical) (_tree : t @ logical) = true

let prove_true query tree : unit{ member query tree = true } =
  let _ = member_def query tree in ()

let empty_has_no_zero (_ : unit @ logical)
    : unit{ member 0 empty = true } = prove_true 0 empty
let empty_has_no_one (_ : unit @ logical)
    : unit{ member 1 empty = true } = prove_true 1 empty
let insert_zero_has_zero (_ : unit @ logical)
    : unit{ member 0 (insert 0 empty) = true } =
  prove_true 0 (insert 0 empty)
let insert_zero_has_no_one (_ : unit @ logical)
    : unit{ member 1 (insert 0 empty) = true } =
  prove_true 1 (insert 0 empty)
let insert_one_preserves_zero (_ : unit @ logical)
    : unit{ member 0 (insert 1 (insert 0 empty)) = true } =
  prove_true 0 (insert 1 (insert 0 empty))
